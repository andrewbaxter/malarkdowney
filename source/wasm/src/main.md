# What is this malarkdowney

## It is a visual markdown editor

When editing markdown in common text editors, the mapping between the markdown and the rendered representation is hard to understand. While not as bad as ReST or HTML, things like how many spaces are needed to make a line part of a block, whether a symbol needs escaping (and how to escape it) and whether `()` or `[]` comes first in a link are easy to confuse.

- One solution many editors take is to offer a preview, either as a toggle button or as live preview in a separate pane. A toggle button adds friction to editing though, and a separate live preview pane effectively halves available screen space.

- Conversely, WYSIWYG rich text editors have the issue that because markup state transitions are invisible and zero-width, editing intent is ambiguous.

  For example, if your cursor is between a normal letter and a bold letter and you press "x", should "x" be bold or not? There's no way to convey what you want to the editor, and this is both frustrating and, depending on the editor, can lead to a lot of leftover junk markup.

Malarkdowney sits in the middle: it applies the style to the markdown text as you write it without modifying any text - that is, _all_ ~control~ *characters* are always visible, simultaneously resolving cursor ambiguity and providing feedback on the final style.

(This text is editable.)
