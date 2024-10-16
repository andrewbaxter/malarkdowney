use {
    flowcontrol::shed,
    std::{
        mem::swap,
        str::Chars,
    },
};

#[derive(Debug)]
pub struct InlineStrong {
    pub begin_delim: String,
    pub children: Vec<Inline>,
    pub end_delim: Option<String>,
}

#[derive(Debug)]
pub struct InlineEmphasis {
    pub begin_delim: String,
    pub children: Vec<Inline>,
    pub end_delim: Option<String>,
}

#[derive(Debug)]
pub struct InlineLink {
    pub incomplete: bool,
    // `[`
    pub title_begin_delim: String,
    pub title: Vec<Inline>,
    pub continuation: Option<InlineLinkContinuation>,
}

#[derive(Debug)]
pub struct InlineLinkContinuation {
    // `]`
    pub title_end_delim: String,
    pub address: Option<InlineLinkAddress>,
}

#[derive(Debug)]
pub struct InlineLinkAddress {
    // `(`
    pub begin_delim: String,
    pub address: String,
    // `)`
    pub end_delim: String,
}

#[derive(Debug)]
pub struct InlineCode {
    pub incomplete: bool,
    pub text: String,
}

#[derive(Debug)]
pub enum Inline {
    Text(String),
    Strong(InlineStrong),
    Emphasis(InlineEmphasis),
    Link(InlineLink),
    Code(InlineCode),
}

#[derive(Clone)]
struct Cursor<'a> {
    iter: Chars<'a>,
    prev_iter: Option<Chars<'a>>,
}

impl<'a> Cursor<'a> {
    fn read(&mut self) -> Option<char> {
        self.prev_iter = Some(self.iter.clone());
        return self.iter.next()
    }

    fn undo(&mut self) {
        self.iter = self.prev_iter.take().unwrap();
    }

    fn commit(&mut self, other: Cursor<'a>) {
        self.iter = other.iter;
        self.prev_iter = other.prev_iter;
    }
}

pub fn parse(text: &str) -> Vec<Inline> {
    return parse_inline(&mut Cursor {
        iter: text.chars(),
        prev_iter: None,
    }, None);
}

fn parse_inline(c: &mut Cursor, want_end: Option<char>) -> Vec<Inline> {
    let mut children = vec![];

    struct Text {
        text: String,
    }

    impl Text {
        fn push(&mut self, c: char) {
            self.text.push(c);
        }

        fn flush(&mut self) -> Vec<Inline> {
            let mut done = String::new();
            swap(&mut self.text, &mut done);
            if done.is_empty() {
                return vec![];
            } else {
                return vec![Inline::Text(done)];
            }
        }
    }

    let mut text = Text { text: String::new() };
    let mut escape = false;
    loop {
        let Some(next) = c.read() else {
            children.extend(text.flush());
            return children;
        };
        if escape {
            escape = false;
            text.push(next);
        } else {
            if Some(next) == want_end {
                c.undo();
                children.extend(text.flush());
                return children;
            }
            match next {
                ESCAPE => {
                    escape = true;
                    text.push(next);
                },
                STRONG_DELIM => {
                    children.extend(text.flush());
                    children.push(parse_strong(c));
                },
                EMPHASIS_DELIM => {
                    children.extend(text.flush());
                    children.push(parse_emphasis(c));
                },
                CODE_DELIM => {
                    children.extend(text.flush());
                    children.push(parse_code(c));
                },
                LINK_BEGIN_DELIM => {
                    children.extend(text.flush());
                    children.push(parse_link(c));
                },
                _ => {
                    text.push(next);
                },
            }
        }
    }
}

const ESCAPE: char = '\\';
pub const STRONG_DELIM: char = '*';
pub const EMPHASIS_DELIM: char = '_';
pub const CODE_DELIM: char = '`';
pub const LINK_BEGIN_DELIM: char = '[';
pub const LINK_END_DELIM: char = ']';
pub const LINK_ADDR_BEGIN_DELIM: char = '(';
pub const LINK_ADDR_END_DELIM: char = ')';

fn parse_strong(c: &mut Cursor) -> Inline {
    let mut complete = false;
    let children = parse_inline(c, Some(STRONG_DELIM));
    shed!{
        let Some(next) = c.read() else {
            break;
        };
        if next != STRONG_DELIM {
            c.undo();
            break;
        }
        complete = true;
    }
    return Inline::Strong(InlineStrong {
        begin_delim: STRONG_DELIM.to_string(),
        children: children,
        end_delim: if complete {
            Some(STRONG_DELIM.to_string())
        } else {
            None
        },
    });
}

fn parse_emphasis(c: &mut Cursor) -> Inline {
    let mut complete = false;
    let children = parse_inline(c, Some(EMPHASIS_DELIM));
    shed!{
        let Some(next) = c.read() else {
            break;
        };
        if next != EMPHASIS_DELIM {
            c.undo();
            break;
        }
        complete = true;
    }
    return Inline::Emphasis(InlineEmphasis {
        begin_delim: EMPHASIS_DELIM.to_string(),
        children: children,
        end_delim: if complete {
            Some(EMPHASIS_DELIM.to_string())
        } else {
            None
        },
    });
}

fn parse_code(c: &mut Cursor) -> Inline {
    let mut complete = false;
    let mut text = CODE_DELIM.to_string();
    shed!{
        'eof _;
        // Count repeated begin delimiters
        let mut extra_delim_reps = 0;
        loop {
            let Some(next) = c.read() else {
                break 'eof;
            };
            if next != CODE_DELIM {
                c.undo();
                break;
            }
            text.push(next);
            extra_delim_reps += 1;
        }
        // Read body and end
        'read_text : loop {
            let Some(next) = c.read() else {
                break 'eof;
            };

            // If next char is delim, try to read the same number of delims as at the begin
            if next == CODE_DELIM {
                shed!{
                    'not_end _;
                    let mut c1 = c.clone();
                    for _ in 0 .. extra_delim_reps {
                        let Some(next1) = c1.read() else {
                            break 'eof;
                        };
                        if next1 != CODE_DELIM {
                            break 'not_end;
                        }
                    }
                    for _ in 0 .. (extra_delim_reps + 1) {
                        text.push(CODE_DELIM);
                    }
                    c.commit(c1);
                    break 'read_text;
                };
            }

            // Otherwise the char is code text
            text.push(next);
        }
        complete = true;
    };
    return Inline::Code(InlineCode {
        incomplete: !complete,
        text: text,
    });
}

fn parse_link(c: &mut Cursor) -> Inline {
    let mut complete = false;
    let title = parse_inline(c, Some(LINK_END_DELIM));
    let continuation = shed!{
        let Some(next) = c.read() else {
            break None;
        };
        if next != LINK_END_DELIM {
            c.undo();
            break None;
        }
        Some(InlineLinkContinuation {
            title_end_delim: next.to_string(),
            address: shed!{
                let Some(next) = c.read() else {
                    break None;
                };
                if next != '(' {
                    c.undo();
                    break None;
                }
                let mut addr_text = "".to_string();
                let mut addr_suffix = "".to_string();
                let mut escape = false;
                loop {
                    let Some(next) = c.read() else {
                        break;
                    };
                    if escape {
                        escape = false;
                        addr_text.push(next);
                    } else {
                        match next {
                            ESCAPE => {
                                addr_text.push(next);
                                escape = true;
                            },
                            ')' => {
                                complete = true;
                                addr_suffix.push(next);
                                break;
                            },
                            _ => {
                                addr_text.push(next);
                            },
                        }
                    }
                }
                Some(InlineLinkAddress {
                    begin_delim: next.to_string(),
                    address: addr_text,
                    end_delim: addr_suffix,
                })
            },
        })
    };
    return Inline::Link(InlineLink {
        incomplete: !complete,
        title_begin_delim: LINK_BEGIN_DELIM.to_string(),
        title: title,
        continuation: continuation,
    });
}

pub fn text(text: impl AsRef<str>) -> Inline {
    return Inline::Text(text.as_ref().to_string());
}

pub fn strong(children: Vec<Inline>) -> Inline {
    return Inline::Strong(InlineStrong {
        begin_delim: STRONG_DELIM.to_string(),
        children: children,
        end_delim: Some(STRONG_DELIM.to_string()),
    });
}

pub fn emphasis(children: Vec<Inline>) -> Inline {
    return Inline::Emphasis(InlineEmphasis {
        begin_delim: EMPHASIS_DELIM.to_string(),
        children: children,
        end_delim: Some(EMPHASIS_DELIM.to_string()),
    });
}

pub fn code(text: impl AsRef<str>) -> Inline {
    let text = text.as_ref();
    let mut longest_run = 0;
    let mut run = 0;
    for c in text.chars() {
        if c == CODE_DELIM {
            run += 1;
        } else {
            if run > longest_run {
                longest_run = run;
                run = 0;
            }
        }
    }
    if run > longest_run {
        longest_run = run;
    }
    let delim = CODE_DELIM.to_string().repeat(longest_run + 1);
    return Inline::Code(InlineCode {
        incomplete: false,
        text: format!("{}{}{}", delim, text, delim),
    })
}

pub fn link(title: Vec<Inline>, address: impl AsRef<str>) -> Inline {
    return Inline::Link(InlineLink {
        incomplete: false,
        title_begin_delim: LINK_BEGIN_DELIM.to_string(),
        title: title,
        continuation: Some(InlineLinkContinuation {
            title_end_delim: LINK_END_DELIM.to_string(),
            address: Some(InlineLinkAddress {
                begin_delim: LINK_ADDR_BEGIN_DELIM.to_string(),
                address: address.as_ref().to_string(),
                end_delim: LINK_ADDR_END_DELIM.to_string(),
            }),
        }),
    });
}
