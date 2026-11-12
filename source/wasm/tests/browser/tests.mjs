const eq = (a, b, msg) => { if (JSON.stringify(a) !== JSON.stringify(b)) throw new Error(`${msg || "mismatch"}: got ${JSON.stringify(a)} want ${JSON.stringify(b)}`); };
const DEB = 450; // > the 200ms typing pause after which edits are committed (history, change event)

export default async (test, { ev, key, type, sleep, s }) => {
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

  await test("lines: separate editor lines export as separate markdown paragraphs", async () => {
    await ev(`mk("")`);
    await type("one");
    await key("Enter");
    await type("two");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "one\n\ntwo");
    await key("Enter");
    await key("Enter");
    await sleep(DEB);
    eq(await ev("ed.querySelectorAll('.malarkdowney_line').length"), 4, "two trailing empty lines");
    const text = await ev("md.getText(ed)");
    eq(text, "one\n\ntwo\n\n\n");
    eq(await ev(`md.setText(ed, ${JSON.stringify(text)}); md.getText(ed)`), text, "round trips");
    await ev(`caretAt(0); getSelection().collapse(ed.querySelector('.malarkdowney_line:last-child'), 0)`);
    await key("Backspace");
    await key("Backspace");
    await sleep(DEB);
    eq(await ev("[md.getText(ed), ed.querySelectorAll('.malarkdowney_line').length]"), ["one\n\ntwo", 2], "each Backspace removes one line");
  });

  await test("lines: no phantom trailing lines after deleting everything", async () => {
    await ev(`mk("a\\n\\nb")`);
    await sleep(DEB);
    await ev("getSelection().selectAllChildren(ed)");
    await key("Backspace");
    await sleep(DEB);
    eq(await ev("[md.getText(ed), ed.querySelectorAll('.malarkdowney_line').length]"), ["", 1]);
    await type("x");
    await key("Enter");
    await sleep(DEB);
    eq(await ev("ed.querySelectorAll('.malarkdowney_line').length"), 2);
    await key("Backspace");
    await key("Backspace");
    await sleep(DEB);
    eq(await ev("[md.getText(ed), ed.querySelectorAll('.malarkdowney_line').length]"), ["", 1], "back to one empty line");
    eq(await ev("errors"), []);
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
    await ev(`mk("> abc\\nxyz")`);
    await sleep(DEB);
    await ev(`caretAt(3)`); // after "> a"
    await key("ArrowDown");
    const info = await ev("caretInfo()");
    if (!(info.anchor >= 6 && info.anchor <= 8)) throw new Error(`caret ${info.anchor}`);
    await key("ArrowUp");
    const info2 = await ev("caretInfo()");
    if (!(info2.anchor >= 2 && info2.anchor <= 4)) throw new Error(`caret up ${info2.anchor}`);
  });

  await test("arrow down/up visits an empty line", async () => {
    await ev(`mk("abc\\n\\n\\nxyz")`); // (the second blank line is kept as an empty line)
    await sleep(DEB);
    eq(await ev("ed.querySelectorAll('.malarkdowney_line').length"), 3);
    const onEmpty = `(() => { const n = getSelection().anchorNode; return n.nodeType === 1 && n.textContent === "" && n.classList.contains("malarkdowney_line"); })()`;
    await ev(`caretAt(1)`);
    await key("ArrowDown");
    eq(await ev(onEmpty), true, "down onto the empty line");
    await key("ArrowDown");
    const info = await ev("caretInfo()");
    if (!(info.anchor >= 3 && info.anchor <= 6)) throw new Error(`caret ${info.anchor}`);
    await ev(`caretAt(4)`);
    await key("ArrowUp");
    eq(await ev(onEmpty), true, "up onto the empty line");
    await key("ArrowUp");
    const info2 = await ev("caretInfo()");
    if (!(info2.anchor >= 0 && info2.anchor <= 3)) throw new Error(`caret up ${info2.anchor}`);
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
    // (Enter continues a block with a generated prefix; backspace over it to leave)
    for (const l of ["* a", "> q"]) { await type(l); await key("Enter"); await key("Backspace"); await key("Backspace"); }
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
    // Enter continues the enclosing block with a generated prefix; backspacing over it leaves
    await ev(`mk("")`);
    for (const [backspaces, l] of [[0, "> quote *b*"], [0, "* item"], [0, "more"], [4, "1. one"], [3, "2. two _e_"], [3, "```"], [0, "code * x"], [0, "```"], [0, "## h2 [l](http://x)"]]) {
      for (let i = 0; i < backspaces; i++) await key("Backspace");
      await type(l);
      await key("Enter");
    }
    await sleep(DEB);
    eq(await ev("md.getText(ed).split('\\n').filter(x => x.trim() !== '' && x.trim() !== '>')"), ["> quote *b*", "> * item", ">   more", "1. one", "2. two _e_", "```", "code * x", "```", "## h2 [l](http://x)"]);
    eq(await ev("errors"), []);
  });

  await test("join: Delete at end of an empty list item joins the next item without crashing", async () => {
    // Chrome merges the two <p>s into one containing both prefix spans; placement then
    // creates a nested list. Used to panic (and wipe the editor) when a third item followed.
    await ev(`mk("* \\n* b\\n* c")`);
    await sleep(DEB);
    await ev(`(() => { const t = texts(); getSelection().collapse(t[0], 2); })()`);
    await key("Delete");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "* * b\n\n* c");
    eq(await ev("ed.querySelectorAll('.malarkdowney_block_ul').length"), 2, "nested list");
    eq(await ev("errors"), []);
  });

  await test("recovery: unexpected DOM shape rebuilds from text instead of trapping", async ({ logs }) => {
    await ev(`mk("* a\\n* b")`);
    await sleep(DEB);
    // Simulate a host (or browser) mangling the structure
    await ev(`ed.querySelector('.malarkdowney_block').removeAttribute('malarkdowney_id')`);
    await ev(`caretAt(6)`); // end of "b"
    await type("c");
    await sleep(DEB);
    eq(logs.filter(l => l.includes("rebuilding from text")).length, 1, "rebuild logged");
    logs.length = 0;
    eq(await ev("md.getText(ed)"), "* a\n\n* bc", "text kept");
    eq(await ev("!!ed.querySelector('.malarkdowney_block[malarkdowney_id]')"), true, "structure rebuilt");
    eq((await ev("caretInfo()")).anchor, 7, "caret kept");
    eq(await ev("errors"), []);
    await type("d");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "* a\n\n* bcd", "editor still works");
  });

  await test("paste: replaces a selection spanning blocks, as one undo step", async () => {
    await ev(`mk("* abc\\n> def\\nghi")`);
    await sleep(DEB);
    await ev("caretAt(3, 9)"); // "b" .. "e"
    await ev(`(() => { const dt = new DataTransfer(); dt.setData("text/plain", "X\\nY"); ed.dispatchEvent(new ClipboardEvent("paste", {clipboardData: dt, bubbles: true, cancelable: true})); })()`);
    eq(await ev("md.getText(ed)"), "* aX\n\nYf\n\nghi");
    eq((await ev("caretInfo()")).anchor, 5, "caret after pasted text");
    await key("z", CTRL);
    eq(await ev("md.getText(ed)"), "* abc\n\n> def\n\nghi");
  });

  await test("paste: works with a root-level selection and into an empty editor", async () => {
    await ev(`mk("")`);
    await ev("getSelection().collapse(ed, 0)");
    await ev(`(() => { const dt = new DataTransfer(); dt.setData("text/plain", "a\\nb"); ed.dispatchEvent(new ClipboardEvent("paste", {clipboardData: dt, bubbles: true, cancelable: true})); })()`);
    eq(await ev("md.getText(ed)"), "a\n\nb");
  });

  await test("paste: non-text clipboard (e.g. an image) is ignored, no DOM junk", async () => {
    await ev(`mk("a")`);
    await sleep(DEB);
    await ev("caretAt(1)");
    await ev(`(() => { const dt = new DataTransfer(); dt.setData("text/html", "<img src=x>"); ed.dispatchEvent(new ClipboardEvent("paste", {clipboardData: dt, bubbles: true, cancelable: true})); })()`);
    await sleep(DEB);
    eq(await ev("[md.getText(ed), ed.querySelectorAll('img').length]"), ["a", 0]);
  });

  await test("drop: inserts plain text at the drop point", async () => {
    await ev(`mk("abc")`);
    await sleep(DEB);
    const prevented = await ev(`(() => { const t = texts()[0]; const r = document.createRange(); r.setStart(t, 1); r.setEnd(t, 1); const b = r.getBoundingClientRect(); const dt = new DataTransfer(); dt.setData("text/plain", "X"); dt.setData("text/html", "<b>X</b>"); const e = new DragEvent("drop", {dataTransfer: dt, bubbles: true, cancelable: true, clientX: b.left, clientY: b.top + 2}); ed.dispatchEvent(e); return e.defaultPrevented; })()`);
    eq(prevented, true, "default prevented");
    eq(await ev("[md.getText(ed), ed.querySelectorAll('b').length]"), ["aXbc", 0]);
  });

  await test("prefix parsing: ordered list numbers and heading levels", async () => {
    await ev(`mk("10. a\\n    b\\n# h\\n####### no\\n#no\\n1.no")`);
    await sleep(DEB);
    eq(await ev("[ed.querySelectorAll('.malarkdowney_block_ol').length, ed.querySelectorAll('.malarkdowney_block_ol p').length, ed.querySelectorAll('h1,h2,h3,h4,h5,h6').length]"), [1, 2, 1]);
  });

  await test("embedding: insertText at the caret, or appended when unfocused; change event; blank class", async () => {
    await ev(`mk("ab")`);
    await sleep(DEB);
    await ev(`window.changes = 0; ed.addEventListener("malarkdowney-change", () => window.changes++)`);
    await ev("caretAt(1)");
    await ev(`md.insertText(ed, "😀")`);
    eq(await ev("md.getText(ed)"), "a😀b");
    eq((await ev("caretInfo()")).anchor, 3, "caret after insert");
    await type("!");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "a😀!b");
    await ev(`(() => { const b = document.createElement("button"); b.id = "btn"; document.body.appendChild(b); b.focus(); getSelection().removeAllRanges(); })()`);
    await ev(`md.insertText(ed, "\\nend")`);
    eq(await ev("[md.getText(ed), document.activeElement.id]"), ["a😀!b\n\nend", "btn"]);
    await ev(`document.getElementById("btn").remove()`);
    eq(await ev("changes"), 3, "one change event per insert/typing step");
    eq(await ev("ed.classList.contains('malarkdowney_blank')"), false);
    await ev(`md.setText(ed, "")`);
    eq(await ev("ed.classList.contains('malarkdowney_blank')"), true, "blank after setText('')");
    await ev(`md.setText(ed, "\\n")`);
    eq(await ev("ed.classList.contains('malarkdowney_blank')"), true, "only empty lines is blank");
    await ev("ed.focus(); getSelection().collapse(ed.querySelector('.malarkdowney_line'), 0)");
    await type("x");
    await sleep(DEB);
    eq(await ev("ed.classList.contains('malarkdowney_blank')"), false, "not blank after typing");
    await key("Backspace");
    await sleep(DEB);
    eq(await ev("ed.classList.contains('malarkdowney_blank')"), true, "blank again after deleting");
    eq(await ev("[ed.getAttribute('role'), ed.getAttribute('aria-multiline')]"), ["textbox", "true"]);
  });

  await test("Enter inside a block continues it with the prefix, caret after the prefix", async () => {
    await ev(`mk("> a")`);
    await sleep(DEB);
    await ev("caretAt(3)");
    await key("Enter");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "> a\n>\n> ", "quote prefix generated");
    await type("b");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "> a\n>\n> b", "typed after the prefix");
    await key("Enter");
    await type("c"); // prefix is generated before the next keystroke regardless of typing speed
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "> a\n>\n> b\n>\n> c");
    await key("Enter");
    await key("Backspace");
    await key("Backspace"); // deleting the generated prefix leaves the block (no regeneration)
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "> a\n>\n> b\n>\n> c\n\n", "empty line after the quote");
    await type("d");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "> a\n>\n> b\n>\n> c\n\nd");
    await ev("caretAt(0)");
    await key("Enter"); // new line above the first: no context, plain
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "\n> a\n>\n> b\n>\n> c\n\nd", "Enter at the start");

    await ev(`mk("* a")`);
    await sleep(DEB);
    await ev("caretAt(3)");
    await key("Enter");
    await sleep(DEB);
    await type("b");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "* a\n\n  b", "list continuation indent, typed after it");
    await ev(`mk("1. a")`);
    await sleep(DEB);
    await ev("caretAt(4)");
    await key("Enter");
    await sleep(DEB);
    await type("b");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "1. a\n\n   b", "ordered list continuation indent");
  });

  await test("newlineOnEnter: false - Enter reaches a normal keydown listener and inserts nothing, Shift+Enter still does", async () => {
    await ev(`(() => { document.querySelectorAll(".malarkdowney_root").forEach(e => e.remove()); const e = md.build("a", {newlineOnEnter: false}); document.body.appendChild(e); e.focus(); window.ed = e; window.enters = []; e.addEventListener("keydown", ev => { if (ev.key === "Enter") enters.push([ev.shiftKey, ev.defaultPrevented]); }); })()`);
    await ev("caretAt(1)");
    await key("Enter");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "a", "Enter didn't insert a newline");
    eq(await ev("ed.querySelectorAll('.malarkdowney_line').length"), 1, "one line");
    eq(await ev("enters"), [[false, false]], "listener saw an unhandled Enter");
    await key("Enter", 8);
    await type("b");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "a\nb", "Shift+Enter inserted a newline");
    await ev(`mk("a"); caretAt(1)`);
    await key("Enter");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "a\n\n", "default still inserts a newline");
  });

  await test("buildReadonly: renders, not editable, no event listeners", async () => {
    await ev(`(() => { document.querySelectorAll(".malarkdowney_root").forEach(e => e.remove()); window.ro = md.buildReadonly("# T\\n\\n* a _b_\\n\\n   cont"); document.body.appendChild(ro); })()`);
    await sleep(100);
    eq(await ev("[ro.hasAttribute('contenteditable'), ro.hasAttribute('role'), ro.isContentEditable]"), [false, false, false]);
    eq(await ev("[!!ro.querySelector('h1'), !!ro.querySelector('em')]"), [true, true], "formatted");
    eq(await ev("[...ro.querySelectorAll('.malarkdowney_block')].some(b => b.style.paddingLeft !== '')"), true, "list continuation aligned");
    eq(await ev("md.getText(ro)"), "", "no editor state");
    const { result } = await s("Runtime.evaluate", { expression: "ro" });
    const { listeners } = await s("DOMDebugger.getEventListeners", { objectId: result.objectId, depth: -1 });
    eq(listeners.map(l => l.type), [], "event listeners");
    eq(await ev("errors"), []);
  });

  // Restyling happens as the browser reports each edit; only history/change events wait for a pause
  const frames = () => ev("new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)))");
  const prefixesAligned = () => ev(`[...ed.querySelectorAll('.malarkdowney_aligned')].every(a => {
    const block = a.closest('[malarkdowney_id="' + a.getAttribute('malarkdowney_indent_id') + '"]');
    const inner = a.firstElementChild.getBoundingClientRect();
    return Math.abs(inner.left - block.getBoundingClientRect().left) < 1 && inner.width <= parseFloat(getComputedStyle(block).paddingLeft) + 0.5;
  })`);

  await test("restyle: inline style appears immediately, change event only after the pause", async () => {
    await ev(`mk(""); window.changes = 0; ed.addEventListener("malarkdowney-change", () => window.changes++)`);
    await type("_a_");
    eq(await ev("!!ed.querySelector('em')"), true, "styled before the pause");
    eq(await ev("changes"), 0, "not committed yet");
    await sleep(DEB);
    eq(await ev("changes"), 1, "committed after the pause");
    eq(await ev("errors"), []);
  });

  await test("restyle: prefixes stay aligned before the commit (widening, new list line)", async () => {
    await ev(`mk("9. a")`);
    await sleep(DEB);
    eq(await prefixesAligned(), true, "initial");
    await ev("caretAt(0)");
    await type("1");
    await frames();
    eq(await ev("ed.querySelector('.malarkdowney_aligned').textContent"), "19. ", "prefix widened");
    eq(await prefixesAligned(), true, "wider prefix aligned before commit");
    await ev("caretAt(5)");
    await key("Enter");
    await type("b");
    await frames();
    eq(await ev("ed.querySelectorAll('.malarkdowney_aligned').length"), 2, "continuation prefix added");
    eq(await prefixesAligned(), true, "new line aligned before commit");
    await sleep(DEB);
    eq(await prefixesAligned(), true, "after commit");
    eq(await ev("md.getText(ed)"), "19. a\n\n    b");
    eq(await ev("errors"), []);
  });

  await test("ime: composition is left alone until it ends", async () => {
    await ev(`mk("")`);
    await s("Input.imeSetComposition", { text: "*a*", selectionStart: 3, selectionEnd: 3 });
    await frames();
    eq(await ev("[!!ed.querySelector('strong'), ed.textContent]"), [false, "*a*"], "not restyled during composition");
    await s("Input.insertText", { text: "*a*" });
    await frames();
    eq(await ev("!!ed.querySelector('strong')"), true, "restyled after composition");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "*a*");
    eq((await ev("caretInfo()")).anchor, 3, "caret");
    eq(await ev("errors"), []);
  });

  const CHAT = `{ prefixes: [{ kind: "mention", prefix: "@", chars: "_.-" }, { kind: "tag", prefix: "#" }], delimiters: [{ kind: "spoiler", open: "||" }, { kind: "tpl", open: "{{", close: "}}" }] }`;

  await test("ast: editor loading matches the pure structurer, AST round trip is exact", async () => {
    await ev(`mk("", ${CHAT})`);
    const bad = await ev(`(() => {
      const syn = ${CHAT};
      const tokens = ["a", " ", "*", "_", "> ", "* ", "- ", "1. ", "12. ", "\`\`\`", "~~~", "||", "{{", "}}", "\\n", "\\n\\n", "  ", "   ", "@b", "#", "# ", "\`"];
      let seed = 1;
      const rand = (n) => { seed = (seed * 1103515245 + 12345) % 2147483648; return seed % n; };
      const docs = ["* > a\\n* > b", "* \`\`\`\\n* x", "> * a\\n>\\n>   b\\n\\nc", "\`\`\`js\\n~~~\\n\`\`\`\\nx", "1. a\\n\\n   * b\\n\\n     c"];
      for (let i = 0; i < 400; i++) { let d = ""; for (let j = rand(10); j >= 0; j--) d += tokens[rand(tokens.length)]; docs.push(d); }
      const out = [];
      for (const doc of docs) {
        md.setText(ed, doc);
        const text = md.getText(ed);
        const pure = md.testPureExport(doc);
        if (text !== pure) { out.push({ doc, text, pure }); continue; }
        const shape1 = JSON.stringify(shape(ed));
        md.setText(ed, text);
        if (md.getText(ed) !== text || JSON.stringify(shape(ed)) !== shape1) out.push({ doc, text, reloaded: md.getText(ed) });
        const ast = JSON.stringify(md.getAst(ed));
        if (ast !== JSON.stringify(md.textToAst(md.astToText(md.getAst(ed), syn), syn))) out.push({ doc, ast });
      }
      return out;
    })()`);
    eq(bad.slice(0, 3), [], "mismatches");
    eq(await ev("errors"), []);
  });

  await test("ast: typed structure matches loading the same text", async () => {
    await ev(`mk("", ${CHAT})`);
    for (const line of ["* a", "> q", "* b", "1. c", "```js", "x", "```", "# h @bob ||s||"]) {
      await type(line);
      await key("Enter");
    }
    await type("end");
    await sleep(DEB);
    const text = await ev("md.getText(ed)");
    const typed = await ev("JSON.stringify(shape(ed))");
    const loaded = await ev(`(() => { const r = md.buildReadonly(md.getText(ed), ${CHAT}); return JSON.stringify(shape(r)); })()`);
    eq(typed, loaded, `structure of ${JSON.stringify(text)}`);
    eq(await ev(`md.astToText(md.getAst(ed), ${CHAT})`), text, "ast round trip");
    eq(await ev("errors"), []);
  });

  await test("custom prefixes: rendered as tokens, updated while typing", async () => {
    await ev(`mk("hi @bob and #general.", ${CHAT})`);
    eq(await ev(`[...ed.querySelectorAll(".malarkdowney_inline_prefixed")].map(e => [e.getAttribute("malarkdowney_custom"), e.getAttribute("malarkdowney_custom_value"), e.textContent, e.classList.contains("malarkdowney_custom_" + e.getAttribute("malarkdowney_custom"))])`),
      [["mention", "bob", "@bob", true], ["tag", "general", "#general", true]]);
    eq(await ev("md.getText(ed)"), "hi @bob and #general.");
    await ev(`mk("", ${CHAT})`);
    await type("email a@b.c, ping @al");
    await sleep(DEB);
    await type("ice_");
    await frames();
    eq(await ev(`[...ed.querySelectorAll(".malarkdowney_inline_prefixed")].map(e => e.textContent)`), ["@alice"], "token grows, trailing _ excluded");
    eq((await ev("caretInfo()")).anchor, "email a@b.c, ping @alice_".length, "caret");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "email a@b.c, ping @alice_");
    eq(await ev("md.getAst(ed)"), [{ type: "paragraph", children: [{ type: "text", text: "email a@b.c, ping " }, { type: "prefixed", kind: "mention", value: "alice" }, { type: "text", text: "_" }] }]);
    eq(await ev("errors"), []);
  });

  await test("custom delimiters: rendered as spans with nested formatting, incomplete while typing", async () => {
    await ev(`mk("a ||secret *b*|| c {{x}} ||open", ${CHAT})`);
    eq(await ev(`[...ed.querySelectorAll(".malarkdowney_inline_delimited")].map(e => [e.getAttribute("malarkdowney_custom"), e.classList.contains("malarkdowney_custom_" + e.getAttribute("malarkdowney_custom")), e.classList.contains("malarkdowney_incomplete"), !!e.querySelector("strong"), e.textContent])`), [
      ["spoiler", true, false, true, "||secret *b*||"],
      ["tpl", true, false, false, "{{x}}"],
      ["spoiler", true, true, false, "||open"],
    ]);
    await ev(`mk("", ${CHAT})`);
    await type("hi ||x");
    await frames();
    eq(await ev("ed.querySelector('.malarkdowney_inline_delimited').classList.contains('malarkdowney_incomplete')"), true, "unclosed");
    await type("||");
    await frames();
    eq(await ev("ed.querySelector('.malarkdowney_inline_delimited').classList.contains('malarkdowney_incomplete')"), false, "closed");
    eq((await ev("caretInfo()")).anchor, "hi ||x||".length, "caret");
    await sleep(DEB);
    eq(await ev("md.getAst(ed)"), [{ type: "paragraph", children: [{ type: "text", text: "hi " }, { type: "delimited", kind: "spoiler", children: [{ type: "text", text: "x" }] }] }]);
    eq(await ev("errors"), []);
  });

  await test("code blocks: info attribute follows the opening line", async () => {
    await ev(`mk("\`\`\`mermaid\\nx\\n\`\`\`\\nafter")`);
    eq(await ev("shape(ed)"), [{ block: "code", info: "mermaid", children: ["codeline:\`\`\`mermaid", "codeline:x", "codeline:\`\`\`"] }, "p:after"]);
    await ev("caretAt(10)");
    await type(" x");
    await frames();
    eq(await ev("ed.querySelector('.malarkdowney_block_code').getAttribute('malarkdowney_code_info')"), "mermaid x", "info updated");
    await ev("caretAt(3, 12)");
    await key("Backspace");
    await frames();
    eq(await ev("ed.querySelector('.malarkdowney_block_code').hasAttribute('malarkdowney_code_info')"), false, "info removed");
    await sleep(DEB);
    eq(await ev("md.getText(ed)"), "\`\`\`\nx\n\`\`\`\nafter");
    eq(await ev("errors"), []);
  });

  await test("ast api: build/buildReadonly from an AST, getAst/setAst, invalid input throws", async () => {
    const ast = [
      { type: "heading", level: 2, children: ["Hi ", { type: "prefixed", kind: "mention", value: "bob" }, " ", { type: "delimited", kind: "spoiler", children: ["s"] }] },
      { type: "list", items: [{ children: [{ type: "paragraph", children: [{ type: "strong", children: ["x"] }, " *y*"] }] }] },
      { type: "code", info: "js", lines: ["a", ""] },
    ];
    await ev(`mk(${JSON.stringify(ast)}, ${CHAT})`);
    eq(await ev("md.getText(ed)"), "## Hi @bob ||s||\n\n* *x* \\*y\\*\n```js\na\n\n```");
    eq(await ev("md.getAst(ed)"), [
      { type: "heading", level: 2, children: [{ type: "text", text: "Hi " }, { type: "prefixed", kind: "mention", value: "bob" }, { type: "text", text: " " }, { type: "delimited", kind: "spoiler", children: [{ type: "text", text: "s" }] }] },
      { type: "list", ordered: false, items: [{ marker: "*", children: [{ type: "paragraph", children: [{ type: "strong", children: [{ type: "text", text: "x" }] }, { type: "text", text: " *y*" }] }] }] },
      { type: "code", info: "js", lines: ["a", ""], closed: true },
    ]);
    await ev("window.changes = 0; ed.addEventListener('malarkdowney-change', () => changes++)");
    await ev(`md.setAst(ed, [{ type: "quote", children: [{ type: "paragraph", children: ["> q"] }] }])`);
    eq(await ev("[md.getText(ed), changes, !!ed.querySelector('.malarkdowney_block_quote')]"), ["> \\> q", 1, true]);
    eq(await ev(`(() => { const r = md.buildReadonly(${JSON.stringify(ast)}, ${CHAT}); return [!!r.querySelector('h2 .malarkdowney_custom_mention'), r.querySelector('.malarkdowney_block_code').getAttribute('malarkdowney_code_info')]; })()`), [true, "js"]);
    eq(await ev(`md.textToAst("@bob")`), [{ type: "paragraph", children: [{ type: "text", text: "@bob" }] }], "no prefixes by default");
    const throws = async (expr) => ev(`(() => { try { ${expr}; return null; } catch (e) { return e instanceof TypeError; } })()`);
    eq(await throws(`md.build("", { prefixes: [{ kind: "x", prefix: "*" }] })`), true, "reserved prefix");
    eq(await throws(`md.build("", { delimiters: [{ kind: "x", open: "**" }] })`), true, "reserved delimiter");
    eq(await throws(`md.build("", { prefixes: [{ kind: "x", prefix: "|" }], delimiters: [{ kind: "y", open: "||" }] })`), true, "delimiter overlaps prefix");
    eq(await throws(`md.setAst(ed, [{ type: "nope" }])`), true, "bad block");
    eq(await throws(`md.astToText([{ type: "heading", level: 7, children: [] }])`), true, "bad level");
    eq(await ev("md.getText(ed)"), "> \\> q", "unchanged after a failed setAst");
    eq(await ev("errors"), []);
  });
};
