#[cfg(test)]
mod tests {
    use super::*;

    fn chat_syntax() -> Syntax {
        return Syntax::default()
            .with_prefix("mention", '@', "_.-")
            .unwrap()
            .with_prefix("tag", '#', "")
            .unwrap()
            .with_delimiter("spoiler", "||", "||")
            .unwrap()
            .with_delimiter("tpl", "{{", "}}")
            .unwrap();
    }

    #[test]
    fn custom_prefixes() {
        for (
            input,
            want,
        ) in [
            ("hi @alice.", "T(hi )Umention(alice)T(.)"),
            ("@a_b-c", "Umention(a_b-c)"),
            ("bob@example.com", "T(bob@example.com)"),
            ("@ x @", "T(@ x @)"),
            ("@_", "T(@_)"),
            ("\\@a", "T(\\@a)"),
            ("#general, #b2", "Utag(general)T(, )Utag(b2)"),
            ("*@a*", "S[Umention(a)]"),
            ("[@a](u)", "L[Umention(a)]"),
            ("`@a`", "C(`@a`)"),
            ("@@a", "T(@)Umention(a)"),
            ("||secret|| x", "Dspoiler[T(secret)]T( x)"),
            ("a ||*b*||", "T(a )Dspoiler[S[T(b)]]"),
            ("*a ||b* c", "S[T(a )Dspoiler?[T(b)]]T( c)"),
            ("||a *b||", "Dspoiler[T(a )S?[T(b)]]"),
            ("a||b||", "T(a||b||)"),
            ("|| a||", "T(|| a||)"),
            ("||a ||", "Dspoiler?[T(a ||)]"),
            ("{{@x}}!", "Dtpl[Umention(x)]T(!)"),
            ("\\||a||", "T(\\||a||)"),
            ("|a|", "T(|a|)"),
        ] {
            assert_eq!(dump(&parse(input, &chat_syntax())), want, "input: {:?}", input);
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
                Inline::Prefixed(c) => out.push_str(&format!("U{}({})", c.kind, c.value)),
                Inline::Delimited(d) => out.push_str(&format!("D{}{}[{}]", d.kind, if d.end_delim.is_some() {
                    ""
                } else {
                    "?"
                }, dump(&d.children))),
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
        let alphabet = ['*', '_', '~', '`', '[', ']', '(', ')', '\\', 'a', ' ', 'é', '@', '.', '|', '{', '}'];
        let syntax = chat_syntax();
        let mut s = vec![];

        fn rec(alphabet: &[char], syntax: &Syntax, s: &mut Vec<char>, depth: usize) {
            let text: String = s.iter().collect();
            let mut out = String::new();
            unparse(&parse(&text, syntax), &mut out);
            assert_eq!(out, text);
            if depth == 0 {
                return;
            }
            for c in alphabet {
                s.push(*c);
                rec(alphabet, syntax, s, depth - 1);
                s.pop();
            }
        }

        rec(&alphabet, &syntax, &mut s, 5);
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
            assert_eq!(dump(&parse(input, &Syntax::default())), want, "input: {:?}", input);
        }
    }

    fn unparse(inlines: &[Inline], out: &mut String) {
        for i in inlines {
            match i {
                Inline::Text(t) => out.push_str(t),
                Inline::Strong(InlineStrong { begin_delim, children, end_delim }) |
                Inline::Emphasis(InlineEmphasis { begin_delim, children, end_delim }) |
                Inline::Strikethrough(InlineStrikethrough { begin_delim, children, end_delim }) |
                Inline::Delimited(InlineDelimited { begin_delim, children, end_delim, .. }) => {
                    out.push_str(begin_delim);
                    unparse(children, out);
                    if let Some(e) = end_delim {
                        out.push_str(e);
                    }
                },
                Inline::Code(code) => out.push_str(&code.text),
                Inline::Prefixed(prefixed) => {
                    out.push(prefixed.prefix);
                    out.push_str(&prefixed.value);
                },
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
}

use crate::syntax::Syntax;

pub const CODE_DELIM: char = '`';
pub const EMPHASIS_DELIM: char = '_';
const ESCAPE: char = '\\';
pub const LINK_ADDR_BEGIN_DELIM: char = '(';
pub const LINK_ADDR_END_DELIM: char = ')';
pub const LINK_BEGIN_DELIM: char = '[';
pub const LINK_END_DELIM: char = ']';
pub const STRIKE_DELIM: char = '~';
pub const STRONG_DELIM: char = '*';

struct Cursor {
    chars: Vec<char>,
    i: usize,
}

impl Cursor {
    fn advance(&mut self) -> Option<char> {
        let out = self.peek();
        if out.is_some() {
            self.i += 1;
        }
        return out;
    }

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
}

pub enum Inline {
    Code(InlineCode),
    Delimited(InlineDelimited),
    Emphasis(InlineEmphasis),
    Link(InlineLink),
    Prefixed(InlinePrefixed),
    Strikethrough(InlineStrikethrough),
    Strong(InlineStrong),
    Text(String),
}

pub struct InlineCode {
    pub incomplete: bool,
    pub text: String,
}

pub struct InlineDelimited {
    pub begin_delim: String,
    pub children: Vec<Inline>,
    pub end_delim: Option<String>,
    pub kind: String,
}

pub struct InlineEmphasis {
    pub begin_delim: String,
    pub children: Vec<Inline>,
    pub end_delim: Option<String>,
}

pub struct InlineLink {
    pub continuation: Option<InlineLinkContinuation>,
    pub incomplete: bool,
    pub title: Vec<Inline>,
    pub title_begin_delim: String,
}

pub struct InlineLinkAddress {
    pub address: String,
    pub begin_delim: String,
    pub end_delim: String,
}

pub struct InlineLinkContinuation {
    pub address: Option<InlineLinkAddress>,
    pub title_end_delim: String,
}

pub struct InlinePrefixed {
    pub kind: String,
    pub prefix: char,
    pub value: String,
}

pub struct InlineStrikethrough {
    pub begin_delim: String,
    pub children: Vec<Inline>,
    pub end_delim: Option<String>,
}

pub struct InlineStrong {
    pub begin_delim: String,
    pub children: Vec<Inline>,
    pub end_delim: Option<String>,
}

fn is_closer(c: &Cursor, len: usize) -> bool {
    let Some(prev) = c.peek_at(-1) else {
        return false;
    };
    if prev.is_whitespace() {
        return false;
    }
    if let Some(next) = c.peek_at(len as isize) {
        if next.is_alphanumeric() {
            return false;
        }
    }
    return true;
}

pub fn parse(text: &str, syntax: &Syntax) -> Vec<Inline> {
    return parse_inline(&mut Cursor {
        chars: text.chars().collect(),
        i: 0,
    }, &[], syntax);
}

fn parse_inline(c: &mut Cursor, ends: &[String], syntax: &Syntax) -> Vec<Inline> {
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
        if ends.iter().any(|e| {
            return starts_with(c, e) && (e.starts_with(LINK_END_DELIM) || is_closer(c, e.chars().count()));
        }) {
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
                    let children = parse_inline(c, &with_end(ends, &next.to_string()), syntax);
                    let end_delim = if c.peek() == Some(next) && is_closer(c, 1) {
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
                    let title = parse_inline(c, &with_end(ends, &LINK_END_DELIM.to_string()), syntax);
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
                let delimiter = syntax.delimiters().iter().find(|d| {
                    return starts_with(c, &d.open) &&
                        c.peek_at(d.open.chars().count() as isize).map_or(false, |n| !n.is_whitespace()) &&
                        c.peek_at(-1).map_or(true, |p| !p.is_alphanumeric());
                });
                if let Some(delimiter) = delimiter {
                    flush(&mut children, &mut text);
                    c.i += delimiter.open.chars().count();
                    let inner = parse_inline(c, &with_end(ends, &delimiter.close), syntax);
                    let close_len = delimiter.close.chars().count();
                    let end_delim = if starts_with(c, &delimiter.close) && is_closer(c, close_len) {
                        c.i += close_len;
                        Some(delimiter.close.clone())
                    } else {
                        None
                    };
                    children.push(Inline::Delimited(InlineDelimited {
                        kind: delimiter.kind.clone(),
                        begin_delim: delimiter.open.clone(),
                        children: inner,
                        end_delim: end_delim,
                    }));
                    continue;
                }
                let custom =
                    syntax
                        .prefix(next)
                        .filter(|_| !c.peek_at(-1).map_or(false, |p| p.is_alphanumeric()))
                        .map(|p| {
                            let mut len = 0;
                            let mut end = 0;
                            while let Some(next) = c.peek_at(1 + len as isize) {
                                if next.is_alphanumeric() {
                                    len += 1;
                                    end = len;
                                } else if p.chars.contains(next) {
                                    len += 1;
                                } else {
                                    break;
                                }
                            }
                            return (p, end);
                        })
                        .filter(|(_, len)| *len > 0);
                if let Some((prefix, len)) = custom {
                    flush(&mut children, &mut text);
                    c.advance();
                    let value = c.chars[c.i .. c.i + len].iter().collect();
                    c.i += len;
                    children.push(Inline::Prefixed(InlinePrefixed {
                        kind: prefix.kind.clone(),
                        prefix: next,
                        value: value,
                    }));
                } else {
                    c.advance();
                    text.push(next);
                }
            },
        }
    }
}

fn starts_with(c: &Cursor, delim: &str) -> bool {
    return delim.chars().enumerate().all(|(i, ch)| c.peek_at(i as isize) == Some(ch));
}

fn with_end(ends: &[String], end: &str) -> Vec<String> {
    let mut out = ends.to_vec();
    out.push(end.to_string());
    return out;
}
