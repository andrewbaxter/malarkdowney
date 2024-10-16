# Contenteditable

Contenteditable has this behavior:

- Making a new line in a block element will create a new text element with `<br>` before it
- Making a new line in an inline element will create a duplicate chain of elements up to the nearest block element and place it after the current element

# Re-placing lines

If indentation changes, a line may be moved to a different indent-block.

The current line is either the next sibling of any parent node with no intervening siblings, or the first child of all parents in a subtree in such a parent node.

The line will be placed somewhere under the context root, then under the placement root, then under any created parent blocks.

The context root is the lowest existing block of the desired placement with the context line.

The placement root is the lowest existing block of the desired placement under the context line with the current line (i.e. bounded by the context root).

## Case 1, current line outside context root

If the current line is outside the context root, the placement root and context root are the same.

```
root
  block - context root, placement root
    block
      block
        line
        line
        context line
    create block
      create block
        place current line
  branch
    current line
    line
    line
```

- The first two blocks are shared with the context line's context
- Remaining indents need to be created
- The current line is outside the placement root

## Case 2, current line under context root

```
root
  block - context root
    block
      block
        line
        line
        context line
    block - placement root
      create block
        place current line
      block
        block
          current line
          line
          line
```

Could lift following lines here.

```
root
  block - context root, placement root
    block
      block
        line
        line
        context line
    create block
      place current line
    block
      block
        block
          current line
          line
          line
```

```
root
  block - context root, placement root
    block
      block
        line
        line
        context line
      current line
      line
      line
    create block
      place current line
    place following line
    place following line
```

When the re-placement of the current line would split a block, following lines up to the placement root need to be moved along with it in order to not change the ordering. If there's already a split between the context line and current line this might not be necessary, but it's not harmful to do it anyway.
