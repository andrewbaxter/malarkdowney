This is a wasm/js markdown editor widget.

[What, why, interactive readme](https://andrewbaxter.github.io/malarkdowney/)

Features:

- Intuitive

- Space efficient

- Nice looking and easily stylable

- Just markdown - copy and paste work normally

# Rust usage

Do `cargo add malarkdowney`

- `build(markdown_text: &str) -> (rootin::El, Editor)` - Returns a [rooting](https://github.com/andrewbaxter/rooting) `El`, the editable markdown text block, and an `Editor` handle.

  `Options` (`Options::default()` for the defaults):

  - `newline_on_enter: bool` - Default `true`. When `false` Enter doesn't insert a newline (Shift+Enter still does), so you can handle Enter (e.g. to submit) with a normal `keydown` listener.

  - `syntax: Syntax` - Custom prefixes and delimiters, see [Custom syntax](#custom-syntax).

- `build_readonly(markdown_text: &str, syntax: &Syntax) -> rooting::El` - Renders the markdown as a non-editable block, with no event listeners and no editor state.

- `ast::text_to_ast(markdown_text: &str, syntax: &Syntax) -> Vec<ast::Block>`, `ast::ast_to_text(blocks: &[ast::Block], syntax: &Syntax) -> String` - See [AST](#ast).

`Editor` methods:

- `get_text()` - Returns the markdown text from the block.

- `set_text(markdown_text: &str)` - Replaces the text in the block (clears history).

- `get_ast()`, `set_ast(blocks: &[ast::Block])` - Same as `get_text`/`set_text` but with an AST.

- `undo()`, `redo()` - Undo/redo the last change, returns whether there was anything to undo/redo.

- `can_undo()`, `can_redo()`

- `insert_text(text: &str)` - Replaces the selection with the text (one undo step), or appends it if the selection isn't in the editor.

# Typescript usage

TODO publish to npm.

Currently you can build the npm package (wasm, package.json, typescript definitions and glue javascript) by:

1. `cargo install wasm-pack`

2. In `source/wasm/` do `wasm-pack build`

3. The package is generated in `source/wasm/pkg`

In Javascript the editor is the element itself - everything is garbage collected along with it:

- `build(markdown: string | Block[], options?: Options)` - Returns an `Element`, the editable markdown text block.

  `options` (optional):

  - `newlineOnEnter?: boolean` - Default `true`. When `false` Enter doesn't insert a newline (Shift+Enter still does), so you can handle Enter (e.g. to submit) with a normal `keydown` listener.

  - `prefixes?: Prefix[]`, `delimiters?: Delimiter[]` - See [Custom syntax](#custom-syntax).

- `buildReadonly(markdown: string | Block[], syntax?: Syntax)` - Returns an `Element`, the markdown rendered as a non-editable block, with no event listeners and no editor state.

- `textToAst(markdownText: string, syntax?: Syntax): Block[]`, `astToText(blocks: Block[], syntax?: Syntax): string` - See [AST](#ast).

- `getText(element: Element)` - Returns the markdown text from the block.

- `setText(element: Element, markdownText: string)` - Replaces the text in the block (clears history).

- `getAst(element: Element)`, `setAst(element: Element, blocks: Block[])` - Same as `getText`/`setText` but with an AST.

Invalid options or ASTs throw a `TypeError`.

- `undo(element: Element)`, `redo(element: Element)` - Undo/redo the last change, returns whether there was anything to undo/redo.

- `canUndo(element: Element)`, `canRedo(element: Element)`

- `insertText(element: Element, text: string)` - Replaces the selection with the text (one undo step), or appends it if the selection isn't in the editor.

# Custom syntax

`Syntax` (Rust: `Syntax::default().with_prefix(...)?.with_delimiter(...)?`, Typescript: an object with these properties):

- `prefixes` - Inline tokens like `@user` or `#channel`: `{kind: "mention", prefix: "@", chars: "_.-"}`. The prefix is one ASCII punctuation character other than ``\ * _ ~ ` [ ] ( )``. The body is letters, digits and the characters in `chars`, and ends with a letter or digit (so `@xyz.` is `@xyz` then `.`). A prefix after a letter or digit isn't a token (`a@b.com`), and `\@` escapes it.

  Tokens are rendered as `<span class="malarkdowney_inline_prefixed malarkdowney_custom_<kind>" malarkdowney_custom="<kind>" malarkdowney_custom_value="<body>">`.

- `delimiters` - Inline spans like `||spoiler||`: `{kind: "spoiler", open: "||"}` (`close` defaults to `open`, e.g. `{kind: "tpl", open: "{{", close: "}}"}`). Delimiters are ASCII punctuation other than the reserved characters above and the prefixes. They work like `*bold*`: the content can contain other formatting, the opening delimiter must be followed by a non-space and the closing one preceded by one, and unclosed spans are plain text.

  Spans are rendered as `<span class="malarkdowney_inline_delimited malarkdowney_custom_<kind>" malarkdowney_custom="<kind>">` (plus `malarkdowney_incomplete` while unclosed).

Code blocks have a `malarkdowney_code_info` attribute with the text after the opening ```` ``` ```` (if any), so e.g. `[malarkdowney_code_info~="mermaid"]` can style them.

# AST

`textToAst`/`astToText` convert between text and an AST of blocks and inline content (see `Block` and `Inline` in the Typescript definitions, or the `ast` module). The editor and the AST conversion share the same parsing, so `getAst` is `textToAst(getText())` and `setAst` is `setText(astToText())`.

- Each editor line is a block: a paragraph is one line.

- Text in the AST is unescaped, and unclosed formatting (e.g. `*a`) is plain text. `astToText` escapes text only where needed.

- `textToAst(astToText(ast))` gives back the same AST, if the AST came from `textToAst`. Other ASTs may be normalized: adjacent quotes or lists are merged, an empty paragraph right after a quote or list becomes part of it, newlines in text become spaces, and structures markdown can't express (e.g. bold text starting with a space) become plain text.

- `astToText(textToAst(text))` keeps the structure, but inline syntax may be spelled differently (e.g. an unnecessary escape is dropped, code span delimiters are shortened).

- In Typescript, inline children can be plain strings on input (`["Hi ", {type: "prefixed", kind: "mention", value: "bob"}]`). Missing list item markers are filled in and a missing code `closed` is `true`.

# Embedding

- The element dispatches a `malarkdowney-change` event whenever the text changes (after the short pause the editor uses to group typing, and on undo/redo/`setText`). Call `getText` from the handler.

- It has class `malarkdowney_blank` when empty.

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
