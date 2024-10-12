use {
    std::{
        env,
        path::PathBuf,
    },
};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let root = PathBuf::from(&env::var("CARGO_MANIFEST_DIR").unwrap());
    let inline_grammar_path = root.join("inline.rustemo");
    println!("cargo:rerun-if-changed={}", inline_grammar_path.to_string_lossy());
    let inline_out_dir = PathBuf::from(&env::var("OUT_DIR").unwrap()).join("inline");
    rustemo_compiler::Settings::new()
        .out_dir_actions_root(inline_out_dir.clone())
        .out_dir_root(inline_out_dir)
        .process_grammar(&inline_grammar_path)
        .unwrap();
}
