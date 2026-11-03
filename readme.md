This is a wasm/js markdown editor widget.

[What, why, interactive readme](https://andrewbaxter.github.io/malarkdowney/)

Features:

- Intuitive

- Space efficient

- Nice looking and easily stylable

- Just markdown - copy and paste work normally

# Rust usage

Do `cargo add malarkdowney`

- `build(markdown_text: &str)` - Returns an `Element`, the editable markdown text block.

- `get_text(element: &Element)` - Returns the markdown text from the block.

- `set_text(element: &Element, markdown_text: &str)` - Replaces the text in the block (clears history).

- `undo(element: &Element)`, `redo(element: &Element)` - Undo/redo the last change, returns whether there was anything to undo/redo.

- `can_undo(element: &Element)`, `can_redo(element: &Element)`

- `destroy(element: &Element)` - Optional, detaches the editor from the element (removes listeners, stops restyling).

- `Editor::new(markdown_text: &str)` - Alternative to `build` with scoped lifetime: dropping the `Editor` calls `destroy`. Use `editor.root()` to get the element.

# Typescript usage

TODO publish to npm.

Currently you can build the npm package (wasm, package.json, typescript definitions and glue javascript) by:

1. `cargo install wasm-pack` (make sure it matches the version in `wasm/Cargo.lock`)

2. In `wasm/` do `wasm-pack build`

3. The package is generated in `wasm/pkg`

The functions are the same as specified in rust usage, but camel case:

- `build(markdownText: string)`

- `getText(element: Element)`

- `setText(element: Element, markdownText: string)`

- `undo(element: Element)`, `redo(element: Element)`, `canUndo(element: Element)`, `canRedo(element: Element)`

- `destroy(element: Element)`

# Undo/redo

The editor has its own undo history (the browser's doesn't work since lines are re-rendered). Changes are grouped into steps by pauses in typing; pastes and cuts are separate steps.

Ctrl/Cmd+Z, Ctrl/Cmd+Shift+Z, Ctrl+Y and the browser's undo/redo menu items are handled automatically. To integrate with an external editor or app-level undo:

- Call `undo`/`redo` (and `canUndo`/`canRedo` to update buttons). The editor element dispatches a `malarkdowney-history` event whenever the history changes.

- To handle the shortcuts yourself instead, listen for `keydown`/`beforeinput` in the capture phase on an ancestor of the editor and call `preventDefault()` - the editor ignores events that were already handled.

# Caveats

- Slack style inline markdown: `*bold*`, `_italic_`, `~strikethrough~`.

- Not all markdown elements are implemented currently

- Markdown relies on blank lines to separate blocks, and will mean that e.g. abutting paragraphs with no blank lines inbetween will be combined into a single paragraph when rendered. Doing this however would be "invisible control" i.e. you'd press enter and see nothing change, once, and there could be invisible newlines anywhere. So this treats all new lines as hard block element splits.

  You can still add blank lines where they would be required for other parsers, this parser is just interprets the situations where they're missing differently.

- This uses contenteditable for full styling flexibility and ease of use. However, the contenteditable was designed by by monkeys and keyboard cursor movement does something insane 50% of the time. I tried to work around it, I think I made it slightly better, but it's still bad. This is a browser/web spec issue - if you don't like it, report it upstream.
