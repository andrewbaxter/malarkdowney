use {
    aargvark::{
        vark,
        Aargvark,
    },
    flowcontrol::ta_return,
    futures::FutureExt,
    http::Response,
    htwrap::htserve::{
        self,
        async_trait::async_trait,
        response_404,
        response_503,
        Body,
    },
    loga::{
        ea,
        fatal,
        DebugDisplay,
        ErrContext,
        Log,
        ResultContext,
    },
    serde::Deserialize,
    std::{
        env,
        io::{
            stdout,
            Cursor,
            Write,
        },
        path::{
            Path,
            PathBuf,
        },
        process::Stdio,
        sync::Arc,
    },
    taskmanager::TaskManager,
    tokio::{
        fs::{
            remove_dir_all,
            DirEntry,
        },
        net::TcpListener,
        process::Command,
    },
    tokio_stream::wrappers::TcpListenerStream,
};

async fn read_dir(path: &Path) -> Result<Vec<DirEntry>, loga::Error> {
    let mut out = vec![];
    let mut reader =
        tokio::fs::read_dir(path).await.context_with("Error reading directory", ea!(path = path.dbg_str()))?;
    while let Some(e) =
        reader.next_entry().await.context_with("Error reading directory entry", ea!(path = path.dbg_str()))? {
        out.push(e);
    }
    return Ok(out);
}

async fn ensure_dirs(path: &Path) -> Result<(), loga::Error> {
    tokio::fs::create_dir_all(path)
        .await
        .context_with("Error ensuring directory chain", ea!(path = path.dbg_str()))?;
    return Ok(());
}

async fn copy(from: &Path, to: &Path) -> Result<(), loga::Error> {
    tokio::fs::copy(from, to)
        .await
        .context_with("Error copying file", ea!(from = from.dbg_str(), to = to.dbg_str()))?;
    return Ok(());
}

/// `dest` is created and then the entries of `source` are put in `dest`.
async fn recursive_copy(source: &Path, dest: &Path) -> Result<(), loga::Error> {
    ensure_dirs(&dest).await?;
    for entry in read_dir(source).await? {
        let ty = entry.file_type().await?;
        if ty.is_dir() {
            recursive_copy(&entry.path(), &dest.join(entry.file_name())).boxed_local().await?;
        } else {
            copy(&entry.path(), &dest.join(entry.file_name())).await?;
        }
    }
    Ok(())
}

struct SimpleCommand<'a>(&'a mut Command);

impl<'a> SimpleCommand<'a> {
    async fn run(self) -> Result<(), loga::Error> {
        let log = Log::new().fork(ea!(command = self.0.dbg_str()));
        let o =
            self
                .0
                .spawn()
                .stack_context(&log, "Failed to start child process")?
                .wait_with_output()
                .await
                .stack_context(&log, "Failed to wait for child process to exit")?;
        if !o.status.success() {
            return Err(
                log.err_with(
                    "Child process exited with error",
                    ea!(code = o.status.code().dbg_str(), output = o.dbg_str()),
                ),
            );
        }
        return Ok(());
    }
}

trait SimpleCommandExt {
    fn simple<'a>(&'a mut self) -> SimpleCommand<'a>;
}

impl SimpleCommandExt for Command {
    fn simple<'a>(&'a mut self) -> SimpleCommand<'a> {
        return SimpleCommand(self);
    }
}

async fn command_cargo_build(log: &Log, command: &mut Command) -> Result<Vec<PathBuf>, loga::Error> {
    log.log_with(loga::INFO, "Building rust exe", ea!(command = command.dbg_str()));
    command.arg("--message-format=json").stdout(Stdio::piped());

    #[derive(Deserialize)]
    struct CargoLineMessage {
        level: String,
        rendered: String,
    }

    #[derive(Deserialize)]
    struct CargoLine {
        message: Option<CargoLineMessage>,
        executable: Option<String>,
    }

    let p = command.spawn().context_with("Failed to spawn cargo build", ea!(command = command.dbg_str()))?;
    let p =
        p
            .wait_with_output()
            .await
            .context_with("Error waiting for cargo build to complete", ea!(command = command.dbg_str()))?;
    let mut failed = false;
    let mut out = vec![];
    for line in std::io::BufRead::lines(Cursor::new(p.stdout)) {
        let line = match (move || -> Result<CargoLine, loga::Error> {
            let line = line?;
            let line = serde_json::from_str::<CargoLine>(&line)?;
            return Ok(line);
        })() {
            Ok(l) => l,
            Err(e) => {
                log.log_err(loga::WARN, e);
                continue;
            },
        };
        if let Some(m) = line.message {
            if m.level == "error" {
                failed = true;
                eprint!("{}", m.rendered);
            }
        }
        if let Some(exe) = line.executable {
            out.push(PathBuf::from(&exe));
        }
    }
    if failed {
        return Err(log.err("Build failed with errors"));
    }
    return Ok(out);
}

async fn build_web_static(log: &Log, root_dir: &Path, stage_dir: &Path, debug: bool) -> Result<PathBuf, loga::Error> {
    let source_dir = root_dir.join("source/wasm");
    let staging_dir = stage_dir.join("web");
    let static_dir = staging_dir.join("static");
    let bindgen_dir = staging_dir.join("bindgen");
    if staging_dir.exists() {
        remove_dir_all(&staging_dir)
            .await
            .context_with("Failed to clean up old staging dir", ea!(path = staging_dir.dbg_str()))?;
    }
    for d in [&static_dir, &bindgen_dir] {
        ensure_dirs(&d).await?;
    }
    let mut build_cmd = Command::new("cargo");
    build_cmd.current_dir(&source_dir);
    build_cmd.arg("build");
    build_cmd.arg("--target=wasm32-unknown-unknown");
    if !debug {
        build_cmd.arg("--release");
    }
    for exe in command_cargo_build(log, &mut build_cmd).await? {
        let mut bindgen_cmd = Command::new("wasm-bindgen");
        bindgen_cmd.arg(&exe);
        bindgen_cmd.arg("--out-dir").arg(&bindgen_dir);
        bindgen_cmd.arg("--target=web");
        bindgen_cmd.arg("--split-linked-modules");
        if debug {
            bindgen_cmd.arg("--keep-debug");
        }
        bindgen_cmd.simple().run().await?;
        recursive_copy(&bindgen_dir, &static_dir).await?;
    }
    let source_static_dir = root_dir.join(source_dir.join("prestatic"));
    for source in read_dir(&source_static_dir).await? {
        let dest = static_dir.join(source.path().strip_prefix(&source_static_dir).unwrap());
        ensure_dirs(&dest.parent().unwrap()).await?;
        copy(&source.path(), &dest).await?;
    }
    return Ok(static_dir);
}

/// Explicitly capturing async closure - clones elements in the second parens into
/// the closure. Anything else will be moved.
#[macro_export]
macro_rules! cap_fn{
    (($($a: pat_param), *)($($cap: ident), *) {
        $($t: tt) *
    }) => {
        {
            $(let $cap = $cap.clone();) * move | $($a),
            *| {
                $(let $cap = $cap.clone();) * async move {
                    $($t) *
                }
            }
        }
    };
}

#[derive(Aargvark)]
enum ArgCommand {
    Build,
    DevServer,
}

#[tokio::main]
async fn main() {
    match async {
        let log = Log::new_root(loga::INFO);
        let root_dir = PathBuf::from(&env::var("CARGO_MANIFEST_DIR").unwrap()).join("../../").canonicalize()?;
        let command = vark::<ArgCommand>();
        match command {
            ArgCommand::Build => {
                let stage_dir = root_dir.join("stage");
                let static_dir = build_web_static(&log, &root_dir, &stage_dir, false).await?;
                stdout().write_all(static_dir.as_os_str().as_encoded_bytes()).unwrap();
            },
            ArgCommand::DevServer => {
                let stage_dir = root_dir.join("stage");
                let static_dir = build_web_static(&log, &root_dir, &stage_dir, true).await?;
                let tm = TaskManager::new();
                let bind_addr = "127.0.0.1:8080";
                tm.critical_stream(
                    "Static server",
                    TcpListenerStream::new(
                        TcpListener::bind(bind_addr).await.stack_context(&log, "Error binding to address")?,
                    ),
                    {
                        let log = log.fork(ea!(sys = "stand-game-api"));

                        struct Handler {
                            log: Log,
                            static_dir: PathBuf,
                        }

                        #[async_trait]
                        impl htserve::Handler<Body> for Handler {
                            async fn handle(&self, args: htserve::HandlerArgs<'_>) -> Response<Body> {
                                let log = self.log.fork(ea!(url = args.head.uri, endpoint = "stand"));
                                match async {
                                    ta_return!(Response < Body >, loga::Error);
                                    let subpath = args.subpath.strip_prefix("/").unwrap_or(&args.subpath);
                                    let path = std::path::absolute(self.static_dir.join(subpath)).unwrap();
                                    if !path.starts_with(&self.static_dir) {
                                        return Ok(response_404());
                                    }
                                    let mime = mime_guess::from_path(&path).first_or_octet_stream();
                                    return Ok(
                                        htserve::response_file(&args.head.headers, &mime.to_string(), &path).await?,
                                    );
                                }.await {
                                    Ok(r) => r,
                                    Err(e) => {
                                        log.log_err(loga::WARN, e.context("Error serving response"));
                                        return response_503();
                                    },
                                }
                            }
                        }

                        let state = Arc::new(Handler {
                            log: log.clone(),
                            static_dir: static_dir,
                        });
                        cap_fn!((stream)(log, state) {
                            let stream = match stream {
                                Ok(s) => s,
                                Err(e) => {
                                    log.log_err(loga::DEBUG, e.context("Error opening peer stream"));
                                    return Ok(());
                                },
                            };
                            htserve::root_handle_http(&log, state, stream).await?;
                            return Ok(());
                        })
                    },
                );
                log.log_with(loga::INFO, "Server listening", ea!(addr = bind_addr));
                tm.join(&log).await?;
            },
        }
        return Ok(()) as Result<_, loga::Error>;
    }.await {
        Ok(_) => { },
        Err(e) => {
            fatal(e);
        },
    }
}
