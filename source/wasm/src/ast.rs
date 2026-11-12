#[cfg(test)]
mod tests {
    use super::*;

    fn chat() -> Syntax {
        return Syntax::default()
            .with_prefix("mention", '@', "_.-")
            .unwrap()
            .with_prefix("tag", '#', "")
            .unwrap()
            .with_delimiter("spoiler", "||", "||")
            .unwrap();
    }

    fn item(marker: &str, children: Vec<Block>) -> ListItem {
        return ListItem {
            marker: marker.to_string(),
            children: children,
        };
    }

    fn p(c: Vec<Inline>) -> Block {
        return Block::Paragraph(c);
    }

    #[test]
    fn parse_structure() {
        let syntax = chat();
        assert_eq!(
            text_to_ast("# Hi @bob\n\n> q *b*\n>\n> * x\n>\n>   y\n\n1. one\n\n2. two", &syntax),
            vec![Block::Heading {
                level: 1,
                children: vec![t("Hi "), Inline::Prefixed {
                    kind: "mention".to_string(),
                    value: "bob".to_string(),
                }],
            }, Block::Quote(vec![p(vec![t("q "), Inline::Strong(vec![t("b")])]), Block::List {
                ordered: false,
                items: vec![item("*", vec![p(vec![t("x")]), p(vec![t("y")])])],
            }],), Block::List {
                ordered: true,
                items: vec![item("1", vec![p(vec![t("one")])]), item("2", vec![p(vec![t("two")])])],
            }],
        );
        assert_eq!(text_to_ast("```js\na\n~~~\n```\n*x ||_s_ @a||", &syntax), vec![Block::Code {
            info: "js".to_string(),
            lines: vec!["a".to_string(), "~~~".to_string()],
            closed: true,
        }, p(vec![t("*x "), Inline::Delimited {
            kind: "spoiler".to_string(),
            children: vec![Inline::Emphasis(vec![t("s")]), t(" "), Inline::Prefixed {
                kind: "mention".to_string(),
                value: "a".to_string(),
            }],
        }])]);
        assert_eq!(text_to_ast("\\*a\\* [l *x*](u\\)) ``a`b`` \\q", &syntax), vec![p(vec![t("*a* "), Inline::Link {
            children: vec![t("l "), Inline::Strong(vec![t("x")])],
            address: "u)".to_string(),
        }, t(" "), Inline::Code("a`b".to_string()), t(" \\q")],)],);
    }

    #[test]
    fn round_trip_exhaustive() {
        let syntax = chat();
        let alphabet = ["*", "_", "`", "[", "](", ")", "\\", "a", " ", "@", "> ", "* ", "1. ", "```", "||", "\n"];
        let mut s = vec![];

        fn rec(alphabet: &[&'static str], syntax: &Syntax, s: &mut Vec<&'static str>, depth: usize) {
            let text = s.concat();
            let ast = text_to_ast(&text, syntax);
            let text2 = ast_to_text(&ast, syntax);
            let ast2 = text_to_ast(&text2, syntax);
            assert_eq!(ast2, ast, "text {:?} -> {:?}", text, text2);
            assert_eq!(ast_to_text(&ast2, syntax), text2, "text {:?}", text);
            if depth == 0 {
                return;
            }
            for c in alphabet {
                s.push(c);
                rec(alphabet, syntax, s, depth - 1);
                s.pop();
            }
        }

        rec(&alphabet, &syntax, &mut s, 4);
    }

    #[test]
    fn serialize_escapes() {
        let syntax = chat();
        for (
            ast,
            want,
        ) in [
            (vec![p(vec![t("2 * 3 *x*")])], "2 \\* 3 \\*x\\*"),
            (vec![p(vec![t("2 * 3 and "), Inline::Strong(vec![t("x")])])], "2 * 3 and *x*"),
            (vec![p(vec![t("> not a quote")])], "\\> not a quote"),
            (vec![p(vec![t("1. not a list")])], "1\\. not a list"),
            (vec![p(vec![t("``` not code")])], "\\``` not code"),
            (vec![p(vec![t("||c||")])], "\\|\\|c\\|\\|"),
            (vec![p(vec![Inline::Delimited {
                kind: "spoiler".to_string(),
                children: vec![t("x")],
            }, t(" y")])], "||x|| y"),
            (vec![p(vec![t("@notmention a\\b c\\")])], "\\@notmention a\\b c\\\\"),
            (vec![p(vec![Inline::Code("`x".to_string())])], "`` `x ``"),
            (vec![p(vec![Inline::Code(" `x ".to_string())])], "``  `x  ``"),
            (vec![p(vec![Inline::Code("  x ".to_string())])], "`  x `"),
            (vec![p(vec![t("a\nb")])], "a b"),
        ] {
            let text = ast_to_text(&ast, &syntax);
            assert_eq!(text, want);
            let mut normalized = vec![];
            if let Block::Paragraph(c) = &ast[0] {
                normalize_inlines(c, &mut normalized);
            }
            assert_eq!(text_to_ast(&text, &syntax), vec![p(normalized)], "text {:?}", text);
        }
    }

    #[test]
    fn serialize_structure() {
        let syntax = chat();
        let ast = vec![Block::List {
            ordered: false,
            items: vec![
                item("*", vec![Block::Quote(vec![p(vec![t("a")])])]),
                item("-", vec![Block::Quote(vec![p(vec![t("b")])]), p(vec![t("c")]), Block::Code {
                    info: "rs".to_string(),
                    lines: vec!["x".to_string(), "".to_string()],
                    closed: true,
                }]),
            ],
        }, Block::List {
            ordered: true,
            items: vec![item("9", vec![]), item("x", vec![p(vec![])])],
        }];
        let text = ast_to_text(&ast, &syntax);
        assert_eq!(text, "* > a\n\n- > b\n\n  c\n  ```rs\n  x\n  \n  ```\n9. \n2. ");
        let mut want = ast.clone();
        if let Block::List { items, .. } = &mut want[1] {
            items[0].children = vec![p(vec![])];
            items[1].marker = "2".to_string();
        }
        assert_eq!(text_to_ast(&text, &syntax), want);
    }

    fn t(s: &str) -> Inline {
        return Inline::Text(s.to_string());
    }
}

use crate::{
    linemarkdown,
    syntax::Syntax,
    textformat::{
        self,
        ExportBlock,
        ExportLine,
    },
};

pub const CODE_FENCE: &str = "```";

pub fn ast_to_lines(blocks: &[Block], syntax: &Syntax) -> Vec<String> {
    let mut out = vec![];
    emit_blocks(blocks, "", "", syntax, &mut out);
    return out;
}

pub fn ast_to_text(blocks: &[Block], syntax: &Syntax) -> String {
    return textformat::export_lines(
        &structure(&ast_to_lines(blocks, syntax)).iter().map(|l| l.export()).collect::<Vec<_>>(),
    );
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Block {
    Code {
        info: String,
        lines: Vec<String>,
        closed: bool,
    },
    Heading {
        level: usize,
        children: Vec<Inline>,
    },
    List {
        ordered: bool,
        items: Vec<ListItem>,
    },
    Paragraph(Vec<Inline>),
    Quote(Vec<Block>),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BlockKind {
    Code,
    Ol,
    Quote,
    Ul,
}

fn close_frame(frames: &mut Vec<(usize, Frame)>) {
    let block = match frames.pop().unwrap().1 {
        Frame::Root(_) => unreachable!(),
        Frame::Quote(children) => Block::Quote(children),
        Frame::List { ordered, items } => Block::List {
            ordered: ordered,
            items: items,
        },
        Frame::Code { info, lines, closed } => Block::Code {
            info: info,
            lines: lines,
            closed: closed,
        },
    };
    frame_children(&mut frames.last_mut().unwrap().1).push(block);
}

pub fn code_info(begin_text: &str) -> &str {
    return begin_text.get(CODE_FENCE.len()..).unwrap_or_default().trim();
}

fn code_padded(content: &str) -> bool {
    if content.len() < 2 || !content.starts_with(' ') || !content.ends_with(' ') {
        return false;
    }
    let inner = content.trim_matches(' ');
    return inner.starts_with(linemarkdown::CODE_DELIM) || inner.ends_with(linemarkdown::CODE_DELIM);
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum CodeDelim {
    Begin,
    End,
}

#[derive(Clone)]
pub struct ContextBlock {
    pub kind: BlockKind,
    pub width: usize,
}

fn convert_inlines(inlines: Vec<linemarkdown::Inline>, out: &mut Vec<Inline>) {
    for inline in inlines {
        match inline {
            linemarkdown::Inline::Text(t) => push_text(out, &unescape(&t)),
            linemarkdown::Inline::Strong(linemarkdown::InlineStrong { begin_delim, children, end_delim }) |
            linemarkdown::Inline::Emphasis(linemarkdown::InlineEmphasis { begin_delim, children, end_delim }) |
            linemarkdown::Inline::Strikethrough(
                linemarkdown::InlineStrikethrough { begin_delim, children, end_delim },
            ) => {
                if end_delim.is_none() {
                    push_text(out, &begin_delim);
                    convert_inlines(children, out);
                    continue;
                }
                let mut converted = vec![];
                convert_inlines(children, &mut converted);
                out.push(match begin_delim.chars().next() {
                    Some(linemarkdown::STRONG_DELIM) => Inline::Strong(converted),
                    Some(linemarkdown::EMPHASIS_DELIM) => Inline::Emphasis(converted),
                    _ => Inline::Strikethrough(converted),
                });
            },
            linemarkdown::Inline::Code(code) => {
                if code.incomplete {
                    push_text(out, &code.text);
                    continue;
                }
                let chars = code.text.chars().collect::<Vec<_>>();
                let delim_len = chars.iter().take_while(|c| **c == linemarkdown::CODE_DELIM).count();
                let mut content = &chars[delim_len .. chars.len() - delim_len];
                if code_padded(&content.iter().collect::<String>()) {
                    content = &content[1 .. content.len() - 1];
                }
                out.push(Inline::Code(content.iter().collect()));
            },
            linemarkdown::Inline::Link(link) => {
                if !link.incomplete {
                    let mut children = vec![];
                    convert_inlines(link.title, &mut children);
                    out.push(Inline::Link {
                        children: children,
                        address: unescape(
                            &link.continuation.and_then(|c| c.address).map(|a| a.address).unwrap_or_default(),
                        ),
                    });
                    continue;
                }
                push_text(out, &link.title_begin_delim);
                convert_inlines(link.title, out);
                if let Some(continuation) = link.continuation {
                    push_text(out, &continuation.title_end_delim);
                    if let Some(address) = continuation.address {
                        push_text(out, &format!("{}{}{}", address.begin_delim, address.address, address.end_delim));
                    }
                }
            },
            linemarkdown::Inline::Prefixed(prefixed) => {
                out.push(Inline::Prefixed {
                    kind: prefixed.kind,
                    value: prefixed.value,
                });
            },
            linemarkdown::Inline::Delimited(delimited) => {
                if delimited.end_delim.is_none() {
                    push_text(out, &delimited.begin_delim);
                    convert_inlines(delimited.children, out);
                    continue;
                }
                let mut children = vec![];
                convert_inlines(delimited.children, &mut children);
                out.push(Inline::Delimited {
                    kind: delimited.kind,
                    children: children,
                });
            },
        }
    }
}

fn emit_blocks(blocks: &[Block], first: &str, rest: &str, syntax: &Syntax, out: &mut Vec<String>) {
    let blocks =
        blocks.iter().filter(|b| !matches!(b, Block::List { items, .. } if items.is_empty())).collect::<Vec<_>>();
    if blocks.is_empty() {
        out.push(first.to_string());
        return;
    }
    for (i, block) in blocks.into_iter().enumerate() {
        let first = if i == 0 {
            first
        } else {
            rest
        };
        match block {
            Block::Paragraph(children) => {
                out.push(format!("{}{}", first, inline_text(children, true, syntax)));
            },
            Block::Heading { level, children } => {
                out.push(
                    format!("{}{} {}", first, "#".repeat((*level).clamp(1, 6)), inline_text(children, false, syntax)),
                );
            },
            Block::Quote(children) => {
                emit_blocks(children, &format!("{}> ", first), &format!("{}> ", rest), syntax, out);
            },
            Block::List { ordered, items } => {
                for (j, item) in items.iter().enumerate() {
                    let marker = if *ordered {
                        if !item.marker.is_empty() && item.marker.bytes().all(|b| b.is_ascii_digit()) {
                            format!("{}. ", item.marker)
                        } else {
                            format!("{}. ", j + 1)
                        }
                    } else if item.marker == "-" {
                        "- ".to_string()
                    } else {
                        "* ".to_string()
                    };
                    emit_blocks(&item.children, &format!("{}{}", if j == 0 {
                        first
                    } else {
                        rest
                    }, marker), &format!("{}{}", rest, " ".repeat(marker.len())), syntax, out);
                }
            },
            Block::Code { info, lines, closed } => {
                out.push(format!("{}{}{}", first, CODE_FENCE, info.replace(['\n', '\r'], " ")));
                for line in lines {
                    for line in line.split('\n') {
                        out.push(format!("{}{}", rest, line.strip_suffix('\r').unwrap_or(line)));
                    }
                }
                if *closed {
                    out.push(format!("{}{}", rest, CODE_FENCE));
                }
            },
        }
    }
}

enum Frame {
    Code {
        info: String,
        lines: Vec<String>,
        closed: bool,
    },
    List {
        ordered: bool,
        items: Vec<ListItem>,
    },
    Quote(Vec<Block>),
    Root(Vec<Block>),
}

fn frame_children(frame: &mut Frame) -> &mut Vec<Block> {
    match frame {
        Frame::Root(children) | Frame::Quote(children) => return children,
        Frame::List { items, .. } => return &mut items.last_mut().unwrap().children,
        Frame::Code { .. } => unreachable!(),
    }
}

pub fn heading_level(text: &str) -> Option<usize> {
    let level = text.bytes().take_while(|b| *b == b'#').count();
    if level == 0 || level > 6 || !text[level..].starts_with(' ') {
        return None;
    }
    return Some(level);
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Inline {
    Code(String),
    Delimited {
        kind: String,
        children: Vec<Inline>,
    },
    Emphasis(Vec<Inline>),
    Link {
        children: Vec<Inline>,
        address: String,
    },
    Prefixed {
        kind: String,
        value: String,
    },
    Strikethrough(Vec<Inline>),
    Strong(Vec<Inline>),
    Text(String),
}

fn inline_text(inlines: &[Inline], paragraph: bool, syntax: &Syntax) -> String {
    let mut want = vec![];
    normalize_inlines(inlines, &mut want);
    let mut out = String::new();
    for escape_all in [false, true] {
        out.clear();
        write_inlines(&want, escape_all, syntax, &mut out);
        if paragraph && matches!(want.first(), Some(Inline::Text(_))) {
            let escape_at =
                if out.starts_with("> ") || out.starts_with("* ") || out.starts_with("- ") ||
                    heading_level(&out).is_some() ||
                    out.starts_with(CODE_FENCE) {
                    Some(0)
                } else {
                    ol_marker_len(&out).map(|len| len - 2)
                };
            if let Some(at) = escape_at {
                out.insert(at, '\\');
            }
        }
        if parse_inlines(&out, syntax) == want {
            break;
        }
    }
    return out;
}

fn item_marker(b: &MatchedBlock) -> String {
    if !b.item {
        return if b.kind == BlockKind::Ul {
            "*".to_string()
        } else {
            "1".to_string()
        };
    }
    return b.prefix.trim_end().trim_end_matches('.').to_string();
}

pub enum Leaf {
    Code {
        delim: Option<CodeDelim>,
        text: String,
    },
    Text(String),
}

pub struct LineMatch {
    pub continued: Vec<MatchedBlock>,
    pub created: Vec<MatchedBlock>,
    pub leaf: Leaf,
}

pub fn lines_to_ast(lines: &[String], syntax: &Syntax) -> Vec<Block> {
    let mut frames = vec![(usize::MAX, Frame::Root(vec![]))];
    for line in structure(lines) {
        let mut matched = 0;
        let mut new_item = None;
        for (i, (id, b)) in line.blocks.iter().enumerate() {
            if frames.get(i + 1).map_or(true, |f| f.0 != *id) {
                break;
            }
            matched = i + 1;
            if b.item {
                new_item = Some(item_marker(b));
                break;
            }
        }
        while frames.len() > matched + 1 {
            close_frame(&mut frames);
        }
        if let Some(marker) = new_item {
            if let Frame::List { items, .. } = &mut frames.last_mut().unwrap().1 {
                items.push(ListItem {
                    marker: marker,
                    children: vec![],
                });
            }
        }
        for (id, b) in &line.blocks[matched..] {
            frames.push((*id, match b.kind {
                BlockKind::Quote => Frame::Quote(vec![]),
                BlockKind::Ul | BlockKind::Ol => Frame::List {
                    ordered: b.kind == BlockKind::Ol,
                    items: vec![ListItem {
                        marker: item_marker(b),
                        children: vec![],
                    }],
                },
                BlockKind::Code => Frame::Code {
                    info: String::new(),
                    lines: vec![],
                    closed: false,
                },
            }));
        }
        let frame = &mut frames.last_mut().unwrap().1;
        match line.leaf {
            Leaf::Code { delim, text } => {
                if let Frame::Code { info, lines, closed, .. } = frame {
                    match delim {
                        Some(CodeDelim::Begin) => *info = text[CODE_FENCE.len()..].to_string(),
                        Some(CodeDelim::End) => *closed = true,
                        None => lines.push(text),
                    }
                }
            },
            Leaf::Text(text) => {
                let block = match heading_level(&text) {
                    Some(level) => Block::Heading {
                        level: level,
                        children: parse_inlines(&text[level + 1..], syntax),
                    },
                    None => Block::Paragraph(parse_inlines(&text, syntax)),
                };
                frame_children(frame).push(block);
            },
        }
    }
    while frames.len() > 1 {
        close_frame(&mut frames);
    }
    let Some((_, Frame::Root(out))) = frames.pop() else {
        unreachable!();
    };
    return out;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ListItem {
    pub children: Vec<Block>,
    pub marker: String,
}

pub fn match_line(context: &[ContextBlock], context_code_end: bool, line: &str, new_line: bool) -> LineMatch {
    let mut text = line;
    let mut continued = vec![];
    let mut in_code = false;
    for block in context {
        if new_line && text.is_empty() && block.kind != BlockKind::Code {
            continued.push(MatchedBlock {
                kind: block.kind,
                prefix: match block.kind {
                    BlockKind::Quote => "> ".to_string(),
                    _ => " ".repeat(block.width),
                },
                item: false,
                generated: true,
            });
            continue;
        }
        let mut item = false;
        let prefix_len = match block.kind {
            BlockKind::Quote => {
                if !text.starts_with("> ") {
                    break;
                }
                2
            },
            BlockKind::Ul | BlockKind::Ol => {
                let marker_len = if block.kind == BlockKind::Ul {
                    (text.starts_with("* ") || text.starts_with("- ")).then_some(2)
                } else {
                    ol_marker_len(text)
                };
                if let Some(marker_len) = marker_len {
                    item = true;
                    marker_len
                } else if text.starts_with(&" ".repeat(block.width)) {
                    block.width
                } else {
                    break;
                }
            },
            BlockKind::Code => {
                if context_code_end {
                    break;
                }
                in_code = true;
                0
            },
        };
        continued.push(MatchedBlock {
            kind: block.kind,
            prefix: text[..prefix_len].to_string(),
            item: item,
            generated: false,
        });
        text = &text[prefix_len..];
        if item {
            break;
        }
    }
    let mut created = vec![];
    let leaf;
    if in_code {
        leaf = Leaf::Code {
            delim: text.starts_with(CODE_FENCE).then_some(CodeDelim::End),
            text: text.to_string(),
        };
    } else {
        loop {
            let (kind, prefix_len) = if text.starts_with("> ") {
                (BlockKind::Quote, 2)
            } else if text.starts_with("* ") || text.starts_with("- ") {
                (BlockKind::Ul, 2)
            } else if let Some(len) = ol_marker_len(text) {
                (BlockKind::Ol, len)
            } else if text.starts_with(CODE_FENCE) {
                (BlockKind::Code, 0)
            } else {
                leaf = Leaf::Text(text.to_string());
                break;
            };
            created.push(MatchedBlock {
                kind: kind,
                prefix: text[..prefix_len].to_string(),
                item: kind == BlockKind::Ul || kind == BlockKind::Ol,
                generated: false,
            });
            text = &text[prefix_len..];
            if kind == BlockKind::Code {
                leaf = Leaf::Code {
                    delim: Some(CodeDelim::Begin),
                    text: text.to_string(),
                };
                break;
            }
        }
    }
    return LineMatch {
        continued: continued,
        created: created,
        leaf: leaf,
    };
}

pub struct MatchedBlock {
    pub generated: bool,
    pub item: bool,
    pub kind: BlockKind,
    pub prefix: String,
}

fn normalize_inlines(inlines: &[Inline], out: &mut Vec<Inline>) {
    for inline in inlines {
        let normalized = |children: &[Inline]| {
            let mut out = vec![];
            normalize_inlines(children, &mut out);
            return out;
        };
        match inline {
            Inline::Text(t) => push_text(out, &t.replace(['\n', '\r'], " ")),
            Inline::Strong(c) => out.push(Inline::Strong(normalized(c))),
            Inline::Emphasis(c) => out.push(Inline::Emphasis(normalized(c))),
            Inline::Strikethrough(c) => out.push(Inline::Strikethrough(normalized(c))),
            Inline::Link { children, address } => out.push(Inline::Link {
                children: normalized(children),
                address: address.clone(),
            }),
            Inline::Delimited { kind, children } => out.push(Inline::Delimited {
                kind: kind.clone(),
                children: normalized(children),
            }),
            Inline::Code(_) | Inline::Prefixed { .. } => out.push(inline.clone()),
        }
    }
}

pub fn ol_marker_len(text: &str) -> Option<usize> {
    let digits = text.bytes().take_while(|b| b.is_ascii_digit()).count();
    if digits == 0 || !text[digits..].starts_with(". ") {
        return None;
    }
    return Some(digits + 2);
}

fn parse_inlines(text: &str, syntax: &Syntax) -> Vec<Inline> {
    let mut out = vec![];
    convert_inlines(linemarkdown::parse(text, syntax), &mut out);
    return out;
}

fn push_text(out: &mut Vec<Inline>, text: &str) {
    if text.is_empty() {
        return;
    }
    if let Some(Inline::Text(last)) = out.last_mut() {
        last.push_str(text);
        return;
    }
    out.push(Inline::Text(text.to_string()));
}

pub struct StructLine {
    pub blocks: Vec<(usize, MatchedBlock)>,
    pub leaf: Leaf,
}

impl StructLine {
    pub fn export(&self) -> ExportLine {
        let leaf_text = match &self.leaf {
            Leaf::Text(t) => t,
            Leaf::Code { text, .. } => text,
        };
        let mut text = String::new();
        for (_, b) in &self.blocks {
            text.push_str(&b.prefix);
        }
        text.push_str(leaf_text);
        return ExportLine {
            text: text,
            code: matches!(self.leaf, Leaf::Code { .. }),
            content_empty: leaf_text.is_empty(),
            blocks: self.blocks.iter().map(|(id, b)| ExportBlock {
                id: *id,
                quote: b.kind == BlockKind::Quote,
                prefix_len: b.prefix.chars().count(),
            }).collect(),
        };
    }
}

pub fn structure(lines: &[String]) -> Vec<StructLine> {
    let mut structurer = Structurer::default();
    return lines.iter().map(|line| {
        let matched = structurer.match_next(line);
        return structurer.advance(matched);
    }).collect();
}

#[derive(Default)]
pub struct Structurer {
    code_end: bool,
    context: Vec<ContextBlock>,
    ids: Vec<usize>,
    next_id: usize,
}

impl Structurer {
    pub fn advance(&mut self, matched: LineMatch) -> StructLine {
        self.ids.truncate(matched.continued.len());
        self.context.truncate(matched.continued.len());
        for b in &matched.created {
            self.ids.push(self.next_id);
            self.next_id += 1;
            self.context.push(ContextBlock {
                kind: b.kind,
                width: 0,
            });
        }
        let blocks = matched.continued.into_iter().chain(matched.created).collect::<Vec<_>>();
        for (c, b) in self.context.iter_mut().zip(&blocks) {
            if !b.prefix.is_empty() {
                c.width = b.prefix.chars().count();
            }
        }
        self.code_end = matches!(matched.leaf, Leaf::Code { delim: Some(CodeDelim::End), .. });
        return StructLine {
            blocks: self.ids.iter().copied().zip(blocks).collect(),
            leaf: matched.leaf,
        };
    }

    pub fn match_next(&self, line: &str) -> LineMatch {
        return match_line(&self.context, self.code_end, line, true);
    }
}

pub fn text_to_ast(text: &str, syntax: &Syntax) -> Vec<Block> {
    return lines_to_ast(&textformat::import_lines(text), syntax);
}

fn unescape(text: &str) -> String {
    let mut out = String::new();
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(next) = chars.peek().copied().filter(|n| n.is_ascii_punctuation()) {
                out.push(next);
                chars.next();
                continue;
            }
        }
        out.push(c);
    }
    return out;
}

fn write_escaped(text: &str, escape_all: bool, also: &[char], syntax: &Syntax, out: &mut String) {
    let chars = text.chars().collect::<Vec<_>>();
    for (i, c) in chars.iter().copied().enumerate() {
        match c {
            '\n' | '\r' => out.push(' '),
            '\\' => {
                out.push('\\');
                if chars.get(i + 1).map_or(true, |n| n.is_ascii_punctuation()) {
                    out.push('\\');
                }
            },
            _ => {
                if also.contains(&c) || (escape_all && syntax.is_syntax_char(c)) {
                    out.push('\\');
                }
                out.push(c);
            },
        }
    }
}

fn write_inlines(inlines: &[Inline], escape_all: bool, syntax: &Syntax, out: &mut String) {
    for inline in inlines {
        let mut delimited = |delim: char, children: &[Inline]| {
            out.push(delim);
            write_inlines(children, escape_all, syntax, out);
            out.push(delim);
        };
        match inline {
            Inline::Text(t) => write_escaped(t, escape_all, &[], syntax, out),
            Inline::Strong(c) => delimited(linemarkdown::STRONG_DELIM, c),
            Inline::Emphasis(c) => delimited(linemarkdown::EMPHASIS_DELIM, c),
            Inline::Strikethrough(c) => delimited(linemarkdown::STRIKE_DELIM, c),
            Inline::Code(content) => {
                let content = content.replace(['\n', '\r'], " ");
                let mut runs = vec![];
                let mut run = 0;
                for c in content.chars().chain([' ']) {
                    if c == linemarkdown::CODE_DELIM {
                        run += 1;
                    } else if run > 0 {
                        runs.push(run);
                        run = 0;
                    }
                }
                let delim_len = (1..).find(|n| !runs.contains(n)).unwrap();
                let delim = linemarkdown::CODE_DELIM.to_string().repeat(delim_len);
                let pad =
                    content.starts_with(linemarkdown::CODE_DELIM) || content.ends_with(linemarkdown::CODE_DELIM) ||
                        code_padded(&content);
                let pad = if pad {
                    " "
                } else {
                    ""
                };
                out.push_str(&format!("{}{}{}{}{}", delim, pad, content, pad, delim));
            },
            Inline::Link { children, address } => {
                out.push(linemarkdown::LINK_BEGIN_DELIM);
                write_inlines(children, escape_all, syntax, out);
                out.push(linemarkdown::LINK_END_DELIM);
                out.push(linemarkdown::LINK_ADDR_BEGIN_DELIM);
                write_escaped(address, false, &[linemarkdown::LINK_ADDR_END_DELIM], syntax, out);
                out.push(linemarkdown::LINK_ADDR_END_DELIM);
            },
            Inline::Prefixed { kind, value } => {
                match syntax.prefixes().iter().find(|p| &p.kind == kind) {
                    Some(prefix) => {
                        out.push(prefix.prefix);
                        out.push_str(value);
                    },
                    None => write_escaped(value, escape_all, &[], syntax, out),
                }
            },
            Inline::Delimited { kind, children } => {
                let delimiter = syntax.delimiters().iter().find(|d| &d.kind == kind);
                if let Some(delimiter) = delimiter {
                    out.push_str(&delimiter.open);
                }
                write_inlines(children, escape_all, syntax, out);
                if let Some(delimiter) = delimiter {
                    out.push_str(&delimiter.close);
                }
            },
        }
    }
}
