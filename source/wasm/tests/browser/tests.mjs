const eq = (a, b, msg) => { if (JSON.stringify(a) !== JSON.stringify(b)) throw new Error(`${msg || "mismatch"}: got ${JSON.stringify(a)} want ${JSON.stringify(b)}`); };
const DEB = 450; // > 200ms debounce

export default async (test, { ev, key, type, sleep }) => {
  await test("utf16: typing after multibyte text doesn't crash or misplace caret", async () => {
    await ev(`mk("")`);
    await type("éé*");
    await sleep(DEB);
    await type("x");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "éé*x");
    eq((await ev("caretInfo()")).anchor, 4, "caret");
    eq(await ev("errors"), []);
  });

  await test("utf16: emoji + formatting keeps caret at end", async () => {
    await ev(`mk("")`);
    await type("😀 _a");
    await sleep(DEB);
    await type("_ b");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "😀 _a_ b");
    eq((await ev("caretInfo()")).anchor, "😀 _a_ b".length, "caret");
    eq(await ev("!!ed.querySelector('em')"), true, "em rendered");
  });

  await test("code span: unclosed multi-backtick keeps all characters", async () => {
    await ev(`mk("")`);
    await type("``a`");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "``a`");
  });

  await test("empty editor: arrow keys don't crash", async () => {
    await ev(`mk("")`);
    await ev(`ed.focus(); getSelection().collapse(ed, 0)`);
    await key("ArrowDown");
    await key("ArrowUp");
    await type("hi");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "hi");
  });

  await test("code fence: list/quote prefixes inside fence stay literal", async () => {
    await ev(`mk("\`\`\`\\n* x\\n> y\\n\`\`\`")`);
    await sleep(DEB);
    eq(await ev("ed.querySelectorAll('.malarkdowney_block_code .malarkdowney_block').length"), 0, "nested blocks in code");
    eq(await ev("ed.querySelectorAll('codeline').length"), 4, "codelines");
    eq(await ev("md.getText(ed)"), "```\n* x\n> y\n```");
  });

  await test("set_text styles immediately (no debounce wait)", async () => {
    await ev(`mk(""); md.setText(ed, "# Title")`);
    eq(await ev("!!ed.querySelector('h1')"), true);
  });

  for (const [name, doc] of [
    ["paragraphs", "a\n\nb"],
    ["blank line kept", "a\n\n\nb"],
    ["heading + para", "# H\n\npara"],
    ["quote lines", "> a\n>\n> b"],
    ["quote then para", "> a\n\nb"],
    ["list", "* a\n\n* b\n\n   cont"],
    ["code block blank lines literal", "```\nx\n\ny\n```\n\nafter"],
    ["two blank", "a\n\n\n\nb"],
  ]) {
    await test(`round trip get_text(set_text(x)) == x: ${name}`, async () => {
      await ev(`mk("")`);
      const out = await ev(`md.setText(ed, ${JSON.stringify(doc)}); md.getText(ed)`);
      eq(out, doc);
      // And is stable through a second cycle
      eq(await ev(`md.setText(ed, md.getText(ed)); md.getText(ed)`), doc, "second cycle");
    });
  }

  await test("lines: separate editor paragraphs export as separate markdown paragraphs", async () => {
    await ev(`mk("")`);
    await type("one");
    await key("Enter");
    await type("two");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "one\n\ntwo");
  });

  await test("paste: multi-line plain text becomes lines, caret at end", async () => {
    await ev(`mk("ab")`);
    await sleep(DEB);
    await ev(`caretAt(1)`);
    await ev(`(() => { const dt = new DataTransfer(); dt.setData("text/plain", "X\\n\\n# Y\\n\\nZ"); ed.dispatchEvent(new ClipboardEvent("paste", {clipboardData: dt, bubbles: true, cancelable: true})); })()`);
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "aX\n\n# Y\n\nZb");
    eq(await ev("!!ed.querySelector('h1')"), true, "heading parsed");
    eq((await ev("caretInfo()")).anchor, "aX# YZ".length, "caret");
  });

  await test("paste: html is ignored in favour of text/plain", async () => {
    await ev(`mk("")`);
    await ev(`(() => { const dt = new DataTransfer(); dt.setData("text/html", "<b>bold</b><img src=x>"); dt.setData("text/plain", "bold"); ed.dispatchEvent(new ClipboardEvent("paste", {clipboardData: dt, bubbles: true, cancelable: true})); })()`);
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "bold");
    eq(await ev("ed.querySelectorAll('img, b').length"), 0);
  });

  await test("copy: selection across lines uses export format", async () => {
    await ev(`mk("ab\\n\\ncd\\n\\nef")`);
    await sleep(DEB);
    await ev(`caretAt(1, 5)`); // "b" .. "e"
    const got = await ev(`(() => { const dt = new DataTransfer(); ed.dispatchEvent(new ClipboardEvent("copy", {clipboardData: dt, bubbles: true, cancelable: true})); return dt.getData("text/plain"); })()`);
    eq(got, "b\n\ncd\n\ne");
  });

  await test("selection: range made right after typing isn't collapsed by restyle", async () => {
    await ev(`mk("")`);
    await type("hello *world*");
    await sleep(50);
    await ev(`caretAt(0, 5)`);
    await sleep(DEB);
    const info = await ev("caretInfo()");
    eq([info.collapsed, info.text], [false, "hello"]);
  });

  await test("indent alignment shrinks when the prefix gets narrower", async () => {
    await ev(`mk("10. a")`);
    await sleep(DEB);
    const wide = await ev(`parseFloat(ed.querySelector('.malarkdowney_block_ol').style.paddingLeft)`);
    // Edit in place (same block): "10. a" -> "1. a"
    await ev(`caretAt(2)`);
    await key("Backspace");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "1. a");
    const narrow = await ev(`parseFloat(ed.querySelector('.malarkdowney_block_ol').style.paddingLeft)`);
    if (!(wide > 0 && narrow > 0 && narrow < wide)) throw new Error(`wide ${wide} narrow ${narrow}`);
  });

  await test("indent alignment: editor attached after build is aligned", async () => {
    await ev(`document.querySelectorAll(".malarkdowney_root").forEach(e => e.remove()); window.ed = md.build("* item\\n   cont")`);
    await sleep(100);
    await ev(`document.body.appendChild(ed)`);
    await sleep(DEB);
    const pad = await ev(`parseFloat(ed.querySelector('.malarkdowney_block_ul').style.paddingLeft || "0")`);
    if (!(pad > 0)) throw new Error(`padding ${pad}`);
    // Prefix span starts at block's left edge
    const d = await ev(`(() => { const b = ed.querySelector('.malarkdowney_block_ul').getBoundingClientRect().left; const a = ed.querySelector('.malarkdowney_aligned').getBoundingClientRect().left; return Math.abs(a - b); })()`);
    if (d > 1) throw new Error(`misaligned by ${d}`);
  });


  await test("arrow down/up moves between lines in different blocks", async () => {
    await ev(`mk("> abc\\n\\nxyz")`);
    await sleep(DEB);
    await ev(`caretAt(3)`); // after "> a"
    await key("ArrowDown");
    const info = await ev("caretInfo()");
    if (!(info.anchor >= 6 && info.anchor <= 8)) throw new Error(`caret ${info.anchor}`);
    await key("ArrowUp");
    const info2 = await ev("caretInfo()");
    if (!(info2.anchor >= 2 && info2.anchor <= 4)) throw new Error(`caret up ${info2.anchor}`);
  });

  await test("shift+arrow still extends the selection", async () => {
    await ev(`mk("abc")`);
    await sleep(DEB);
    await ev(`caretAt(3)`);
    await key("ArrowLeft", 8);
    await key("ArrowLeft", 8);
    eq((await ev("caretInfo()")).text, "bc");
  });

  await test("cut removes the selection and exports it", async () => {
    await ev(`mk("ab\\n\\ncd")`);
    await sleep(DEB);
    await ev(`caretAt(1, 3)`);
    const got = await ev(`(() => { const dt = new DataTransfer(); ed.dispatchEvent(new ClipboardEvent("cut", {clipboardData: dt, bubbles: true, cancelable: true})); return dt.getData("text/plain"); })()`);
    eq(got, "b\n\nc");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "ad");
  });

  await test("rooted (Rust) build: works while the El is alive", async () => {
    await ev(`(() => { document.querySelectorAll(".malarkdowney_root").forEach(e => e.remove()); md.testRootedDropAll(); const e = md.testRootedBuild(""); document.body.appendChild(e); e.focus(); window.ed = e; })()`);
    await type("*a*");
    await sleep(DEB);
    eq(await ev("[!!ed.querySelector('strong'), md.getText(ed)]"), [true, "*a*"]);
    await key("z", 2);
    eq(await ev("md.getText(ed)"), "", "undo");
    eq(await ev("md.canRedo(ed)"), true);
    eq(await ev("md.redo(ed)"), true);
    eq(await ev("[md.testRootedHandleUndo(), md.getText(ed)]"), [true, ""], "undo via the Rust Editor handle");
  });

  await test("rooted (Rust) build: dropping the El detaches everything", async () => {
    await ev(`(() => { document.querySelectorAll(".malarkdowney_root").forEach(e => e.remove()); md.testRootedDropAll(); const e = md.testRootedBuild("x"); document.body.appendChild(e); e.focus(); window.ed = e; })()`);
    await sleep(DEB);
    await ev(`md.testRootedDropAll()`);
    eq(await ev("ed.__malarkdowney"), undefined, "state unlinked");
    eq(await ev("[md.undo(ed), md.canUndo(ed)]"), [false, false], "ops are no-ops");
    await ev(`caretAt(1)`);
    await type(" *b*");
    await key("z", 2);
    await key("ArrowUp");
    await sleep(DEB);
    eq(await ev("!!ed.querySelector('strong')"), false, "no restyling");
    eq(await ev("errors"), [], "no calls into dropped closures");
  });

  await test("gc: removed editors (with pending updates) are collectable", async ({ s }) => {
    // (Not focused: Chrome itself retains the last focused contenteditable, plain
    // ones included.)
    await ev(`window.refs = []; for (let i = 0; i < 20; i++) { const e = md.build("* item " + i + "\\n> *q*"); document.body.appendChild(e); refs.push(new WeakRef(e)); }`);
    await sleep(100);
    // Mutate so each has pending debounced work, then remove before it runs
    await ev(`refs.forEach(r => { const e = r.deref(); e.querySelector("p, h1, li").append(" *x*"); e.remove(); }); window.ed = null;`);
    await sleep(DEB);
    for (let i = 0; i < 5; i++) { await s("HeapProfiler.collectGarbage"); await sleep(100); }
    eq(await ev(`refs.filter(r => r.deref()).length`), 0, "editors still alive");
  });


  const CTRL = 2, SHIFT = 8;
  const histCount = `(() => { window.histEvents = 0; ed.addEventListener("malarkdowney-history", () => window.histEvents++); })()`;

  await test("gc: editor state (history etc) is freed with the element", async ({ s }) => {
    const gc = async () => { for (let i = 0; i < 6; i++) { await s("HeapProfiler.collectGarbage"); await sleep(150); } };
    await ev(`document.querySelectorAll(".malarkdowney_root").forEach(e => e.remove()); window.ed = null;`);
    await gc();
    const before = await ev("md.testLiveEditorCount()");
    await ev(`for (let i = 0; i < 20; i++) { const e = md.build("* item " + i); document.body.appendChild(e); e.querySelector("p").append(" *x*"); }`);
    await sleep(DEB);
    eq(await ev("md.testLiveEditorCount()"), before + 20, "live while attached");
    await ev(`document.querySelectorAll(".malarkdowney_root").forEach(e => e.remove());`);
    await gc();
    eq(await ev("md.testLiveEditorCount()"), before, "states freed");
  });

  await test("undo/redo: keyboard shortcuts, steps split at typing pauses, caret restored", async () => {
    await ev(`mk("")`);
    await type("abc");
    await sleep(DEB);
    await type(" *def*");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "abc *def*");
    await key("z", CTRL);
    eq(await ev("md.getText(ed)"), "abc");
    eq((await ev("caretInfo()")).anchor, 3, "caret after undo");
    await key("z", CTRL);
    eq(await ev("md.getText(ed)"), "");
    await key("z", CTRL);
    eq(await ev("md.getText(ed)"), "", "nothing more to undo");
    await key("z", CTRL | SHIFT);
    eq(await ev("md.getText(ed)"), "abc");
    eq((await ev("caretInfo()")).anchor, 3, "caret after redo");
    await key("y", CTRL);
    eq(await ev("md.getText(ed)"), "abc *def*");
    eq(await ev("!!ed.querySelector('strong')"), true, "restyled after redo");
  });

  await test("undo: pending (debounced) typing is its own step", async () => {
    await ev(`mk("x")`);
    await sleep(DEB);
    await ev(`caretAt(1)`);
    await type("yz");
    await key("z", CTRL); // before the debounce fires
    eq(await ev("md.getText(ed)"), "x");
  });

  await test("undo: structure (lists, quotes, code) is restored", async () => {
    await ev(`mk("")`);
    for (const l of ["* a", "> q"]) { await type(l); await key("Enter"); }
    await sleep(DEB);
    await type("```");
    await sleep(DEB);
    const before = await ev("[ed.querySelectorAll('.malarkdowney_block_ul').length, ed.querySelectorAll('.malarkdowney_block_quote').length, ed.querySelectorAll('.malarkdowney_block_code').length]");
    eq(before, [1, 1, 1]);
    await key("z", CTRL);
    eq(await ev("ed.querySelectorAll('.malarkdowney_block_code').length"), 0);
    eq(await ev("[ed.querySelectorAll('.malarkdowney_block_ul').length, ed.querySelectorAll('.malarkdowney_block_quote').length]"), [1, 1]);
  });

  await test("undo: paste is one step", async () => {
    await ev(`mk("ab")`);
    await sleep(DEB);
    await ev(`caretAt(1)`);
    await ev(`(() => { const dt = new DataTransfer(); dt.setData("text/plain", "X\\n\\nY"); ed.dispatchEvent(new ClipboardEvent("paste", {clipboardData: dt, bubbles: true, cancelable: true})); })()`);
    eq(await ev("md.getText(ed)"), "aX\n\nYb");
    await type("!");
    await key("z", CTRL);
    eq(await ev("md.getText(ed)"), "aX\n\nYb", "typing after paste undone first");
    await key("z", CTRL);
    eq(await ev("md.getText(ed)"), "ab");
    eq((await ev("caretInfo()")).anchor, 1, "caret back at paste position");
  });

  await test("hooks: undo/redo/canUndo/canRedo functions and history event", async () => {
    await ev(`mk("")`);
    await ev(histCount);
    eq(await ev("[md.canUndo(ed), md.canRedo(ed)]"), [false, false]);
    await type("hi");
    eq(await ev("md.canUndo(ed)"), true, "canUndo flushes pending typing");
    const n = await ev("histEvents");
    if (n < 1) throw new Error("no history event on step");
    eq(await ev("md.undo(ed)"), true);
    eq(await ev("[md.getText(ed), md.canUndo(ed), md.canRedo(ed)]"), ["", false, true]);
    eq(await ev("md.undo(ed)"), false);
    eq(await ev("md.redo(ed)"), true);
    eq(await ev("md.getText(ed)"), "hi");
    eq(await ev("histEvents"), n + 2, "event per undo/redo");
    eq(await ev("md.undo(document.createElement('div'))"), false, "non-editor element");
  });

  await test("hooks: external undo while the editor isn't focused", async () => {
    await ev(`mk("")`);
    await type("abc");
    await sleep(DEB);
    await ev(`(() => { const b = document.createElement("button"); b.id = "btn"; document.body.appendChild(b); b.focus(); })()`);
    eq(await ev("md.undo(ed)"), true);
    eq(await ev("[md.getText(ed), document.activeElement.id]"), ["", "btn"]);
    await ev(`document.getElementById("btn").remove()`);
  });

  await test("hooks: host can take over shortcuts via preventDefault in capture phase", async () => {
    await ev(`mk("")`);
    await type("abc");
    await sleep(DEB);
    await ev(`(() => { window.hostUndos = 0; window.hostHandler = (e) => { if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === "z") { e.preventDefault(); window.hostUndos++; } }; document.addEventListener("keydown", hostHandler, true); })()`);
    await key("z", CTRL);
    await ev(`document.removeEventListener("keydown", hostHandler, true)`);
    eq(await ev("[md.getText(ed), hostUndos]"), ["abc", 1]);
  });

  await test("hooks: browser menu undo (beforeinput historyUndo)", async () => {
    await ev(`mk("")`);
    await type("abc");
    await sleep(DEB);
    const prevented = await ev(`(() => { const e = new InputEvent("beforeinput", {inputType: "historyUndo", cancelable: true, bubbles: true}); ed.dispatchEvent(e); return e.defaultPrevented; })()`);
    eq([prevented, await ev("md.getText(ed)")], [true, ""]);
  });

  await test("set_text resets history", async () => {
    await ev(`mk("")`);
    await type("abc");
    await sleep(DEB);
    await ev(`md.setText(ed, "new")`);
    eq(await ev("[md.canUndo(ed), md.undo(ed), md.getText(ed)]"), [false, false, "new"]);
  });

  await test("typing into nested structures (smoke)", async () => {
    await ev(`mk("")`);
    for (const l of ["> quote *b*", "> * item", ">    more", "1. one", "2. two _e_", "```", "code * x", "```", "## h2 [l](http://x)"]) {
      await type(l);
      await key("Enter");
    }
    await sleep(DEB);
    eq(await ev("md.getText(ed).split('\\n').filter(x => x.trim() !== '' && x.trim() !== '>')"), ["> quote *b*", "> * item", ">    more", "1. one", "2. two _e_", "```", "code * x", "```", "## h2 [l](http://x)"]);
    eq(await ev("errors"), []);
  });
};
