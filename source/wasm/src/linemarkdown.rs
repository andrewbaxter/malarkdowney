pub struct InlineStrong {
    pub begin_delim: String,
    pub children: Vec<Inline>,
    pub end_delim: Option<String>,
}

pub struct InlineEmphasis {
    pub begin_delim: String,
    pub children: Vec<Inline>,
    pub end_delim: Option<String>,
}

pub struct InlineStrikethrough {
    pub begin_delim: String,
    pub children: Vec<Inline>,
    pub end_delim: Option<String>,
}

pub struct InlineLink {
    pub incomplete: bool,
    pub title_begin_delim: String,
    pub title: Vec<Inline>,
    pub continuation: Option<InlineLinkContinuation>,
}

pub struct InlineLinkContinuation {
    pub title_end_delim: String,
    pub address: Option<InlineLinkAddress>,
}

pub struct InlineLinkAddress {
    pub begin_delim: String,
    pub address: String,
    pub end_delim: String,
}

pub struct InlineCode {
    pub incomplete: bool,
    pub text: String,
}

pub enum Inline {
    Text(String),
    Strong(InlineStrong),
    Emphasis(InlineEmphasis),
    Strikethrough(InlineStrikethrough),
    Link(InlineLink),
    Code(InlineCode),
}

const ESCAPE: char = '\\';
pub const STRONG_DELIM: char = '*';
pub const STRIKE_DELIM: char = '~';
pub const EMPHASIS_DELIM: char = '_';
pub const CODE_DELIM: char = '`';
pub const LINK_BEGIN_DELIM: char = '[';
pub const LINK_END_DELIM: char = ']';
pub const LINK_ADDR_BEGIN_DELIM: char = '(';
pub const LINK_ADDR_END_DELIM: char = ')';

struct Cursor {
    chars: Vec<char>,
    i: usize,
}

impl Cursor {
    fn peek(&self) -> Option<char> {
        return self.chars.get(self.i).copied();
    }

    fn peek_at(&self, rel: isize) -> Option<char> {
        let at = self.i as isize + rel;
        if at < 0 {
            return None;
        }
        return self.chars.get(at as usize).copied();
    }

    fn advance(&mut self) -> Option<char> {
        let out = self.peek();
        if out.is_some() {
            self.i += 1;
        }
        return out;
    }
}

fn is_closer(c: &Cursor) -> bool {
    let Some(prev) = c.peek_at(-1) else {
        return false;
    };
    if prev.is_whitespace() {
        return false;
    }
    if let Some(next) = c.peek_at(1) {
        if next.is_alphanumeric() {
            return false;
        }
    }
    return true;
}

pub fn parse(text: &str) -> Vec<Inline> {
    return parse_inline(&mut Cursor {
        chars: text.chars().collect(),
        i: 0,
    }, &[]);
}

fn parse_inline(c: &mut Cursor, ends: &[char]) -> Vec<Inline> {
    let mut children = vec![];
    let mut text = String::new();

    fn flush(children: &mut Vec<Inline>, text: &mut String) {
        if !text.is_empty() {
            children.push(Inline::Text(std::mem::take(text)));
        }
    }

    loop {
        let Some(next) = c.peek() else {
            flush(&mut children, &mut text);
            return children;
        };
        if ends.contains(&next) && (next == LINK_END_DELIM || is_closer(c)) {
            flush(&mut children, &mut text);
            return children;
        }
        match next {
            ESCAPE => {
                c.advance();
                text.push(next);
                if let Some(escaped) = c.advance() {
                    text.push(escaped);
                }
            },
            STRONG_DELIM | STRIKE_DELIM | EMPHASIS_DELIM if
                c.peek_at(1).map_or(false, |n| !n.is_whitespace()) &&
                    c.peek_at(-1).map_or(true, |p| !p.is_alphanumeric()) => {
                flush(&mut children, &mut text);
                children.push({
                    c.advance();
                    let children = parse_inline(c, &with_end(ends, next));
                    let end_delim = if c.peek() == Some(next) && is_closer(c) {
                        c.advance();
                        Some(next.to_string())
                    } else {
                        None
                    };
                    let begin_delim = next.to_string();
                    match next {
                        STRONG_DELIM => Inline::Strong(InlineStrong {
                            begin_delim: begin_delim,
                            children: children,
                            end_delim: end_delim,
                        }),
                        EMPHASIS_DELIM => Inline::Emphasis(InlineEmphasis {
                            begin_delim: begin_delim,
                            children: children,
                            end_delim: end_delim,
                        }),
                        STRIKE_DELIM => Inline::Strikethrough(InlineStrikethrough {
                            begin_delim: begin_delim,
                            children: children,
                            end_delim: end_delim,
                        }),
                        _ => unreachable!(),
                    }
                });
            },
            CODE_DELIM => {
                flush(&mut children, &mut text);
                children.push({
                    let start = c.i;
                    let mut open_len = 0;
                    while c.peek() == Some(CODE_DELIM) {
                        c.advance();
                        open_len += 1;
                    }
                    let body_start = c.i;
                    let mut complete = false;
                    let mut at = body_start;
                    while at < c.chars.len() {
                        if c.chars[at] != CODE_DELIM {
                            at += 1;
                            continue;
                        }
                        let run_start = at;
                        while at < c.chars.len() && c.chars[at] == CODE_DELIM {
                            at += 1;
                        }
                        if at - run_start == open_len {
                            complete = true;
                            break;
                        }
                    }
                    c.i = at;
                    Inline::Code(InlineCode {
                        incomplete: !complete,
                        text: c.chars[start .. c.i].iter().collect(),
                    })
                });
            },
            LINK_BEGIN_DELIM => {
                flush(&mut children, &mut text);
                children.push({
                    c.advance();
                    let mut complete = false;
                    let title = parse_inline(c, &with_end(ends, LINK_END_DELIM));
                    let continuation = if c.peek() == Some(LINK_END_DELIM) {
                        c.advance();
                        Some(InlineLinkContinuation {
                            title_end_delim: LINK_END_DELIM.to_string(),
                            address: if c.peek() == Some(LINK_ADDR_BEGIN_DELIM) {
                                c.advance();
                                let mut address = String::new();
                                let mut end_delim = String::new();
                                while let Some(next) = c.advance() {
                                    match next {
                                        ESCAPE => {
                                            address.push(next);
                                            if let Some(escaped) = c.advance() {
                                                address.push(escaped);
                                            }
                                        },
                                        LINK_ADDR_END_DELIM => {
                                            complete = true;
                                            end_delim.push(next);
                                            break;
                                        },
                                        _ => {
                                            address.push(next);
                                        },
                                    }
                                }
                                Some(InlineLinkAddress {
                                    begin_delim: LINK_ADDR_BEGIN_DELIM.to_string(),
                                    address: address,
                                    end_delim: end_delim,
                                })
                            } else {
                                None
                            },
                        })
                    } else {
                        None
                    };
                    Inline::Link(InlineLink {
                        incomplete: !complete,
                        title_begin_delim: LINK_BEGIN_DELIM.to_string(),
                        title: title,
                        continuation: continuation,
                    })
                });
            },
            _ => {
                c.advance();
                text.push(next);
            },
        }
    }
}

fn with_end(ends: &[char], end: char) -> Vec<char> {
    let mut out = ends.to_vec();
    out.push(end);
    return out;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unparse(inlines: &[Inline], out: &mut String) {
        for i in inlines {
            match i {
                Inline::Text(t) => out.push_str(t),
                Inline::Strong(InlineStrong { begin_delim, children, end_delim }) |
                Inline::Emphasis(InlineEmphasis { begin_delim, children, end_delim }) |
                Inline::Strikethrough(InlineStrikethrough { begin_delim, children, end_delim }) => {
                    out.push_str(begin_delim);
                    unparse(children, out);
                    if let Some(e) = end_delim {
                        out.push_str(e);
                    }
                },
                Inline::Code(code) => out.push_str(&code.text),
                Inline::Link(link) => {
                    out.push_str(&link.title_begin_delim);
                    unparse(&link.title, out);
                    if let Some(cont) = &link.continuation {
                        out.push_str(&cont.title_end_delim);
                        if let Some(addr) = &cont.address {
                            out.push_str(&addr.begin_delim);
                            out.push_str(&addr.address);
                            out.push_str(&addr.end_delim);
                        }
                    }
                },
            }
        }
    }

    fn dump(inlines: &[Inline]) -> String {
        let mut out = String::new();
        for i in inlines {
            match i {
                Inline::Text(t) => out.push_str(&format!("T({})", t)),
                Inline::Strong(s) => out.push_str(&format!("S{}[{}]", if s.end_delim.is_some() {
                    ""
                } else {
                    "?"
                }, dump(&s.children))),
                Inline::Emphasis(s) => out.push_str(&format!("E{}[{}]", if s.end_delim.is_some() {
                    ""
                } else {
                    "?"
                }, dump(&s.children))),
                Inline::Strikethrough(s) => out.push_str(&format!("X{}[{}]", if s.end_delim.is_some() {
                    ""
                } else {
                    "?"
                }, dump(&s.children))),
                Inline::Code(c) => out.push_str(&format!("C{}({})", if c.incomplete {
                    "?"
                } else {
                    ""
                }, c.text)),
                Inline::Link(l) => out.push_str(&format!("L{}[{}]", if l.incomplete {
                    "?"
                } else {
                    ""
                }, dump(&l.title))),
            }
        }
        return out;
    }

    #[test]
    fn round_trip_exhaustive() {
        let alphabet = ['*', '_', '~', '`', '[', ']', '(', ')', '\\', 'a', ' ', 'é'];
        let mut s = vec![];

        fn rec(alphabet: &[char], s: &mut Vec<char>, depth: usize) {
            let text: String = s.iter().collect();
            let mut out = String::new();
            unparse(&parse(&text), &mut out);
            assert_eq!(out, text);
            if depth == 0 {
                return;
            }
            for c in alphabet {
                s.push(*c);
                rec(alphabet, s, depth - 1);
                s.pop();
            }
        }

        rec(&alphabet, &mut s, 5);
    }

    #[test]
    fn structure() {
        for (
            input,
            want,
        ) in [
            ("*bold*", "S[T(bold)]"),
            ("_em_ ~s~", "E[T(em)]T( )X[T(s)]"),
            ("*unclosed", "S?[T(unclosed)]"),
            ("snake_case_name", "T(snake_case_name)"),
            ("2 * 3 * 4", "T(2 * 3 * 4)"),
            ("a*b*c", "T(a*b*c)"),
            ("*a _b* c", "S[T(a )E?[T(b)]]T( c)"),
            ("[a *b](c)", "L[T(a )S?[T(b)]]"),
            ("[t](u) x", "L[T(t)]T( x)"),
            ("``a`", "C?(``a`)"),
            ("```a``", "C?(```a``)"),
            ("``a`b``c", "C(``a`b``)T(c)"),
            ("`a``b`", "C(`a``b`)"),
            ("\\*x*", "T(\\*x*)"),
        ] {
            assert_eq!(dump(&parse(input)), want, "input: {:?}", input);
        }
    }
}
