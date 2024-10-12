# What is this malarkdown

Malarkdown is an inline markdown(-ish) editor.

[Try it out](x)!

In typical markdown editors, the mapping between the markdown and the rendered representation is hard to understand. While not as bad as restructured text or other markups, things like now many spaces are needed to make a line part of a block, whether a symbol needs escaping (and how to escape it) and whether `()` or `[]` comes first in a link are easy to confuse. Many editors offer live previews, but this effectively halves available screen space.

Conversely, WYSIWYG rich text editors have the issue that because markup state transitions are invisible and zero-width editing intent is ambiguous. For example, if your cursor is between a normal letter and a bold letter and you type a new letter, should that letter be bold or not? There's no way to convey what you want to the editor, and this is both frustrating and depending on the editor can lead to a lot of junk markup.

Malarkdown sits in the middle: it applies the style to the markdown text as you write it without modifying any text - that is, all control characters are always visible, resolving cursor ambiguity and providing feedback on the final style.

## Implementation

This is done in Rust (wasm) using [rooting](x). You can use it as a library or call `window.malarkdown("text")` from javascript to create a malarkdown element. At the time of writing wasm doesn't support GC hooks so if you delete a malarkdown element in javascript it'll be leaked.

## Architecture

In an ideal world, we'd hook into a mid-level accessibility-enabled text editing api in the browser that tells us when the selection/cursor moves and when text changes. On such an event, we'd modify an internal model of the text, then sync changes from the model to the DOM. Simple to implement, efficient to run.

In a world where a bunch steer web technologies who can't decide if they should be providing powerful low/mid level tools or high-level declarative tools and so fail to do both, we get `contenteditable`. `contenteditable` is expectedly terrible and the whole implementation is basically a giant hack.

In a `contenteditable` widget, poorly specified by W3C, basically anything can/will happen. We balance the scope of the problem and regeneration of page elements by splitting the markup into lines and re-parsing/generating the whole line when it changes.

Each line contains the full verbatim markup for the line.

- When a line is created or modified, we

  1. Re-parse the line

  2. Re-place it if relevant prefixes/indentation changed and set a flag to update subsequent lines

  3. Update the styling on the line, then

  4. If the flag is set to update subsequent lines repeat the process for the next line.

- When a line is deleted, treat the following line as modified and perform the above procedure

Lines are placed in block html elements which do things like provide indentation/wrapping alignments. Since the indentation is repeated as text within the line, javascript/css is used to overlap the indentation text with the related block element's padding.

Determining if relevant prefixes/indentation changed requires looking primarily at parent elements, which contain enough metadata to determine which blocks the line was nested in.
