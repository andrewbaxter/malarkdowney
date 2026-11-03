// Minimal CDP driver: launches headless Chrome, serves the built site dir, runs a
// test file. Usage: node cdp.mjs SITE_DIR TESTS.mjs (see run.sh)
import { spawn } from "node:child_process";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";
import os from "node:os";
const CHROME = process.env.CHROME || "google-chrome";
const SITE = path.resolve(process.argv[2]);
const types = { ".js": "text/javascript", ".wasm": "application/wasm", ".html": "text/html", ".css": "text/css" };
const server = http.createServer((req, res) => {
  const p = path.join(SITE, decodeURIComponent(req.url.split("?")[0]));
  if (!fs.existsSync(p)) { res.writeHead(404); return res.end(); }
  res.writeHead(200, { "content-type": types[path.extname(p)] || "application/octet-stream" });
  fs.createReadStream(p).pipe(res);
}).listen(0);
const port = server.address().port;
const chrome = spawn(CHROME, ["--headless=new", "--no-sandbox", "--remote-debugging-port=0", `--user-data-dir=${fs.mkdtempSync(path.join(os.tmpdir(), "mdtest-profile-"))}`, "--window-size=1000,800", "about:blank"], { stdio: ["ignore", "ignore", "pipe"], env: process.env });
const wsUrl = await new Promise((ok) => { let buf = ""; chrome.stderr.on("data", (d) => { buf += d; const m = buf.match(/DevTools listening on (ws:\S+)/); if (m) ok(m[1]); }); });
const sleep0 = (ms) => new Promise((ok) => setTimeout(ok, ms));
const ws = new WebSocket(wsUrl);
await new Promise((ok) => ws.addEventListener("open", ok));
let id = 0; const pending = new Map(); const listeners = [];
ws.addEventListener("message", (m) => { const d = JSON.parse(m.data); if (d.id && pending.has(d.id)) { pending.get(d.id)(d); pending.delete(d.id); } else listeners.forEach((l) => l(d)); });
const send = (method, params = {}, sessionId) => new Promise((ok, err) => { const i = ++id; pending.set(i, (d) => d.error ? err(new Error(method + ": " + JSON.stringify(d.error))) : ok(d.result)); ws.send(JSON.stringify({ id: i, method, params, sessionId })); });
await sleep0(300); const { targetInfos } = await send("Target.getTargets"); const targetId = targetInfos.find((t) => t.type === "page").targetId;
const { sessionId } = await send("Target.attachToTarget", { targetId, flatten: true });
const s = (m, p) => send(m, p, sessionId);
const logs = [];
listeners.push((d) => {
  if (d.method === "Runtime.exceptionThrown") logs.push("EXCEPTION: " + (d.params.exceptionDetails.exception?.description || d.params.exceptionDetails.text));
  if (d.method === "Runtime.consoleAPICalled" && ["error", "warning"].includes(d.params.type)) logs.push("CONSOLE " + d.params.type + ": " + d.params.args.map((a) => a.value ?? a.description).join(" "));
});
await s("Runtime.enable"); await s("Page.enable"); await s("Emulation.setFocusEmulationEnabled", { enabled: true }); await s("Page.bringToFront");
await s("Page.navigate", { url: `http://localhost:${port}/test.html` });
const sleep = (ms) => new Promise((ok) => setTimeout(ok, ms));
const ev = async (expr) => { const r = await s("Runtime.evaluate", { expression: expr, awaitPromise: true, returnByValue: true }); if (r.exceptionDetails) throw new Error(r.exceptionDetails.exception?.description || r.exceptionDetails.text); return r.result.value; };
for (let i = 0; i < 100 && !(await ev("!!window.ready")); i++) await sleep(50);
const key = async (k, mods = 0) => { const codes = { ArrowUp: 38, ArrowDown: 40, ArrowLeft: 37, ArrowRight: 39, Backspace: 8, Delete: 46, Enter: 13 }; const letter = /^[a-z]$/i.test(k); const base = { key: letter && (mods & 8) ? k.toUpperCase() : k, code: letter ? "Key" + k.toUpperCase() : k, windowsVirtualKeyCode: letter ? k.toUpperCase().charCodeAt(0) : codes[k], modifiers: mods }; await s("Input.dispatchKeyEvent", { type: "rawKeyDown", ...base, ...(k === "Enter" ? { text: "\r" } : {}) }); if (k === "Enter") await s("Input.dispatchKeyEvent", { type: "char", ...base, text: "\r" }); await s("Input.dispatchKeyEvent", { type: "keyUp", ...base }); };
const type = async (t) => { for (const ch of t) { await s("Input.dispatchKeyEvent", { type: "keyDown", text: ch, unmodifiedText: ch, key: ch }); await s("Input.dispatchKeyEvent", { type: "keyUp", key: ch }); } };
const ctx = { ev, key, type, sleep, logs, s };
const results = [];
const test = async (name, fn) => { logs.length = 0; let err = null; try { await fn(ctx); } catch (e) { err = e.message; } await sleep(50); const ok = !err && logs.length === 0; results.push(ok); console.log(`${ok ? "PASS" : "FAIL"} ${name}${err ? "\n    " + err : ""}${logs.length ? "\n    " + [...new Set(logs.map(l => l.split("\n")[0].slice(0, 200)))].join("\n    ") : ""}`); if (logs.some(l => l.includes("unreachable") || l.includes("RuntimeError"))) { await s("Page.reload"); for (let i = 0; i < 100 && !(await ev("!!window.ready").catch(()=>false)); i++) await sleep(50); } };
const tests = await import(path.resolve(process.argv[3]));
await tests.default(test, ctx);
const passed = results.filter(Boolean).length;
console.log(`${passed}/${results.length} passed`);
ws.close(); chrome.kill(); server.close(); process.exit(passed === results.length ? 0 : 1);
