#!/usr/bin/env bash
# Browser tests: builds the library, then drives headless Chrome over CDP.
#
# Requirements: node >= 22, `wasm-bindgen` (matching the version in Cargo.lock),
# Chrome/Chromium (set `CHROME` if it's not `google-chrome` on the path).
#
# If headless Chrome has no system fonts, text has zero height and editing silently
# does nothing - point `FONTCONFIG_FILE` at a fontconfig file with some fonts.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
wasm_dir=$(cd "$here/../.." && pwd)
site=$(mktemp -d)
trap 'rm -rf "$site"' EXIT
cd "$wasm_dir"
artifact=$(
  cargo build --target wasm32-unknown-unknown --lib --features browser_tests --message-format=json-render-diagnostics |
    node -e '
      let out; require("readline").createInterface({ input: process.stdin }).on("line", (l) => {
        const m = JSON.parse(l);
        if (m.reason === "compiler-artifact" && m.target.name === "malarkdowney") out = m.filenames.find((f) => f.endsWith(".wasm"));
      }).on("close", () => console.log(out));'
)
wasm-bindgen "$artifact" --out-dir "$site" --target web
cp prestatic/*.css prestatic/*.ttf "$here/test.html" "$site/"
node "$here/cdp.mjs" "$site" "${1:-$here/tests.mjs}"
