pub const RESERVED: &[char] = &['\\', '*', '_', '~', '`', '[', ']', '(', ')'];

#[derive(Clone, Debug)]
pub struct Delimiter {
    pub close: String,
    pub kind: String,
    pub open: String,
}

#[derive(Clone, Debug)]
pub struct Prefix {
    pub chars: String,
    pub kind: String,
    pub prefix: char,
}

#[derive(Clone, Debug, Default)]
pub struct Syntax {
    delimiters: Vec<Delimiter>,
    prefixes: Vec<Prefix>,
}

impl Syntax {
    fn check_kind(&self, kind: &str) -> Result<(), String> {
        if kind.is_empty() || !kind.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-') {
            return Err(format!("Kind [{}] must be non-empty letters, digits, `_` or `-`", kind));
        }
        if self.prefixes.iter().any(|p| p.kind == kind) || self.delimiters.iter().any(|d| d.kind == kind) {
            return Err(format!("Kind [{}] is already defined", kind));
        }
        return Ok(());
    }

    pub fn delimiters(&self) -> &[Delimiter] {
        return &self.delimiters;
    }

    pub fn is_syntax_char(&self, c: char) -> bool {
        return RESERVED.contains(&c) || self.prefix(c).is_some() ||
            self.delimiters.iter().any(|d| d.open.contains(c) || d.close.contains(c));
    }

    pub fn prefix(&self, c: char) -> Option<&Prefix> {
        return self.prefixes.iter().find(|p| p.prefix == c);
    }

    pub fn prefixes(&self) -> &[Prefix] {
        return &self.prefixes;
    }

    pub fn with_delimiter(mut self, kind: &str, open: &str, close: &str) -> Result<Self, String> {
        self.check_kind(kind)?;
        for delim in [open, close] {
            if delim.is_empty() || !delim.chars().all(|c| c.is_ascii_punctuation() && !RESERVED.contains(&c)) {
                return Err(
                    format!(
                        "Delimiter [{}] must be ASCII punctuation other than {}",
                        delim,
                        RESERVED.iter().collect::<String>()
                    ),
                );
            }
            if let Some(p) = self.prefixes.iter().find(|p| delim.contains(p.prefix)) {
                return Err(format!("Delimiter [{}] contains the prefix [{}]", delim, p.prefix));
            }
        }
        if self.delimiters.iter().any(|d| d.open == open) {
            return Err(format!("Opening delimiter [{}] is already defined", open));
        }
        self.delimiters.push(Delimiter {
            kind: kind.to_string(),
            open: open.to_string(),
            close: close.to_string(),
        });
        self.delimiters.sort_by(|a, b| b.open.len().cmp(&a.open.len()));
        return Ok(self);
    }

    pub fn with_prefix(mut self, kind: &str, prefix: char, chars: &str) -> Result<Self, String> {
        self.check_kind(kind)?;
        if !prefix.is_ascii_punctuation() || RESERVED.contains(&prefix) {
            return Err(
                format!(
                    "Prefix [{}] must be ASCII punctuation other than {}",
                    prefix,
                    RESERVED.iter().collect::<String>()
                ),
            );
        }
        if let Some(c) = chars.chars().find(|c| c.is_whitespace() || ['\\', '`', '[', ']', '(', ')'].contains(c)) {
            return Err(format!("Prefix [{}] body characters can't include [{}]", prefix, c));
        }
        if self.prefixes.iter().any(|p| p.prefix == prefix) ||
            self.delimiters.iter().any(|d| d.open.contains(prefix) || d.close.contains(prefix)) {
            return Err(format!("Prefix [{}] is already used", prefix));
        }
        self.prefixes.push(Prefix {
            kind: kind.to_string(),
            prefix: prefix,
            chars: chars.to_string(),
        });
        return Ok(self);
    }
}
