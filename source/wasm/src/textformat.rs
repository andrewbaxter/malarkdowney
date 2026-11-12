#[cfg(test)]
mod tests {
    use super::*;

    fn check(lines: &[ExportLine], want: &str) {
        let got = export_lines(lines);
        assert_eq!(got, want);
        let want_lines: Vec<String> = lines.iter().map(|l| l.text.clone()).collect();
        assert_eq!(import_lines(&got), want_lines, "import of {:?}", got);
    }

    #[test]
    fn code() {
        check(
            &[
                line("a", false, &[]),
                line("```", true, &[]),
                line("x", true, &[]),
                line("", true, &[]),
                line("y", true, &[]),
                line("```", true, &[]),
                line("", false, &[]),
                line("b", false, &[]),
            ],
            "a\n```\nx\n\ny\n```\n\nb",
        );
        check(&[line("* ```", true, &[(1, false, 2)]), line("  ", true, &[(1, false, 2)])], "* ```\n  ");
    }

    #[test]
    fn empty_list_item() {
        check(&[line("* ", false, &[(1, false, 2)]), line("  ", false, &[(1, false, 2)])], "* \n  ");
        check(&[line("> * ", false, &[(1, true, 2), (2, false, 2)]), line("b", false, &[])], "> * \nb");
        check(&[line("*  ", false, &[(1, false, 2)]), line("  ", false, &[(1, false, 2)])], "*  \n\n  ");
    }

    #[test]
    fn import_code_blank_lines() {
        assert_eq!(import_lines("```\na\n\n~~~\n```\n\nc"), vec!["```", "a", "", "~~~", "```", "", "c"]);
    }

    #[test]
    fn import_external() {
        assert_eq!(import_lines("a\n"), vec!["a"]);
        assert_eq!(import_lines("a\r\n\r\nb\r\n"), vec!["a", "b"]);
        assert_eq!(import_lines(""), vec![""]);
    }

    fn line(text: &str, code: bool, blocks: &[(usize, bool, usize)]) -> ExportLine {
        let prefix: usize = blocks.iter().map(|b| b.2).sum();
        return ExportLine {
            text: text.to_string(),
            code: code,
            content_empty: text.chars().count() <= prefix,
            blocks: blocks.iter().map(|(id, quote, prefix_len)| ExportBlock {
                id: *id,
                quote: *quote,
                prefix_len: *prefix_len,
            }).collect(),
        };
    }

    #[test]
    fn paragraphs() {
        check(&[line("a", false, &[]), line("b", false, &[])], "a\n\nb");
        check(&[line("a", false, &[]), line("", false, &[]), line("b", false, &[])], "a\n\n\nb");
        check(
            &[line("a", false, &[]), line("", false, &[]), line("", false, &[]), line("b", false, &[])],
            "a\n\n\n\nb",
        );
        check(&[line("a", false, &[]), line("", false, &[])], "a\n\n");
    }

    #[test]
    fn quotes() {
        let q = (1, true, 2);
        check(&[line("> a", false, &[q]), line("> b", false, &[q])], "> a\n>\n> b");
        check(
            &[line("> a", false, &[q]), line("> ", false, &[q]), line("> b", false, &[q])],
            "> a\n>\n> \n> b",
        );
        check(&[line("> a", false, &[q]), line("b", false, &[])], "> a\n\nb");
        check(
            &[line("> * a", false, &[q, (2, false, 2)]), line(">    b", false, &[q, (2, false, 3)])],
            "> * a\n>\n>    b",
        );
        check(
            &[
                line("* > a", false, &[(3, false, 2), (4, true, 2)]),
                line("  > b", false, &[(3, false, 2), (4, true, 2)]),
            ],
            "* > a\n  >\n  > b",
        );
    }
}

use crate::{
    ast::{
        Leaf,
        Structurer,
    },
};

pub fn export_lines(lines: &[ExportLine]) -> String {
    let mut out = String::new();
    for (i, line) in lines.iter().enumerate() {
        if i > 0 {
            out.push('\n');
            let a = &lines[i - 1];
            let b = line;
            let separator = 'separator: {
                if a.code || b.code || a.content_empty {
                    break 'separator None;
                }
                let mut shared = 0;
                while shared < a.blocks.len() && shared < b.blocks.len() && a.blocks[shared].id == b.blocks[shared].id {
                    shared += 1;
                }
                let Some(last_quote) = b.blocks[..shared].iter().rposition(|x| x.quote) else {
                    break 'separator Some(String::new());
                };
                let mut sep = String::new();
                for block in &b.blocks[..=last_quote] {
                    if block.quote {
                        sep.push_str("> ");
                    } else {
                        sep.push_str(&" ".repeat(block.prefix_len));
                    }
                }
                break 'separator Some(sep.trim_end().to_string());
            };
            if let Some(sep) = separator {
                out.push_str(&sep);
                out.push('\n');
            }
        }
        out.push_str(&line.text);
    }
    return out;
}

#[derive(Clone, Debug)]
pub struct ExportBlock {
    pub id: usize,
    pub prefix_len: usize,
    pub quote: bool,
}

#[derive(Clone, Debug)]
pub struct ExportLine {
    pub blocks: Vec<ExportBlock>,
    pub code: bool,
    pub content_empty: bool,
    pub text: String,
}

pub fn import_lines(text: &str) -> Vec<String> {
    let mut out = vec![];
    let mut structurer = Structurer::default();
    let mut prev_droppable = false;
    for line in text.split('\n') {
        let line = line.strip_suffix('\r').unwrap_or(line);
        let matched = structurer.match_next(line);
        let (code, content_empty) = match &matched.leaf {
            Leaf::Text(t) => (false, t.is_empty()),
            Leaf::Code { .. } => (true, false),
        };
        if !code && prev_droppable && line.chars().all(|c| c == '>' || c.is_whitespace()) {
            prev_droppable = false;
            continue;
        }
        prev_droppable = !code && !content_empty;
        structurer.advance(matched);
        out.push(line.to_string());
    }
    return out;
}
