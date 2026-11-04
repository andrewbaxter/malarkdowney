use {
    dom::{
        ScanDirection,
        ScanInclusivity,
    },
    flowcontrol::{
        shed,
        superif,
    },
    gloo::{
        timers::callback::Timeout,
        utils::{
            document,
            window,
        },
    },
    js_sys::{
        Array,
        Object,
        Reflect,
    },
    history::{
        History,
        SelPos,
    },
    linemarkdown::Inline,
    rooting::{
        defer,
        el_from_raw,
        El,
    },
    shadowdom::{
        ShadowDom,
        ShadowDomElement,
        sync_shadow_dom,
    },
    textformat::{
        ExportBlock,
        ExportLine,
    },
    std::{
        cell::{
            Cell,
            RefCell,
        },
        collections::{
            HashMap,
            HashSet,
        },
        rc::{
            Rc,
            Weak,
        },
        str::FromStr,
    },
    wasm_bindgen::{
        JsCast,
        JsValue,
        closure::{
            Closure,
        },
        prelude::wasm_bindgen,
    },
    web_sys::{
        console,
        ClipboardEvent,
        DragEvent,
        Element,
        Event,
        HtmlDocument,
        HtmlElement,
        InputEvent,
        KeyboardEvent,
        MutationObserver,
        MutationObserverInit,
        MutationRecord,
        Node,
        ResizeObserver,
        ResizeObserverEntry,
    },
};

pub mod dom;
pub mod shadowdom;
pub mod linemarkdown;
pub mod textformat;
pub mod history;

macro_rules! ns{
    ($suffix: literal) => {
        concat!("malarkdowney", $suffix)
    };
}

const NAMESPACE: &str = ns!("");
const ATTR_ID: &str = ns!("_id");
const ATTR_INDENT_ID: &str = ns!("_indent_id");
const ATTR_BLOCK_TYPE: &str = ns!("_block_type");
const ATTR_BLOCKCODE_BEGIN: &str = ns!("_codeblock_begin");
const ATTR_BLOCKCODE_END: &str = ns!("_codeblock_end");
const CLASS_NAMESPACE: &str = NAMESPACE;
const CLASS_ROOT: &str = ns!("_root");
const CLASS_INCOMPLETE: &str = ns!("_incomplete");
const CLASS_ALIGNED: &str = ns!("_aligned");
const CLASS_BLOCK: &str = ns!("_block");
const CLASS_LINE: &str = ns!("_line");
const CLASS_UL: &str = ns!("_block_ul");
const CLASS_BLANK: &str = ns!("_blank");
const PROP_STATE: &str = "__malarkdowney";
const ATTR_EDITOR_ID: &str = ns!("_editor_id");
const PREFIX_UL_FIRST: &str = "* ";
const PREFIX_UL_FIRST2: &str = "- ";
const PREFIX_UL: &str = "   ";
const PREFIX_BLOCKQUOTE: &str = "> ";
const PREFIX_BLOCKCODE: &str = "```";

#[derive(Clone, Copy)]
enum BlockType {
    Quote,
    Ul,
    Ol,
    Code,
}

const BLOCK_TYPE_QUOTE: &str = "quote";
const BLOCK_TYPE_UL: &str = "ul";
const BLOCK_TYPE_OL: &str = "ol";
const BLOCK_TYPE_CODE: &str = "code";

impl FromStr for BlockType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        return Ok(match s {
            BLOCK_TYPE_QUOTE => BlockType::Quote,
            BLOCK_TYPE_CODE => BlockType::Code,
            BLOCK_TYPE_UL => BlockType::Ul,
            BLOCK_TYPE_OL => BlockType::Ol,
            _ => {
                return Err(format!("Unknown block type [{}]", s));
            },
        });
    }
}

struct LineIndent {
    source_indent: usize,
    type_: BlockType,
    first_text: Option<String>,
    text: String,
}

enum BlockMatch {
    /// The line follows a line in the block: may be a new item, a continuation line (indented
    /// by the block's prefix width, `indent.text`), or empty.
    Continue {
        context_codeblock_end: bool,
    },
    /// The line is the first in the block (the block starts with it): must have the block's
    /// starting prefix.
    First,
}

enum BlockMatched {
    No,
    Yes,
    CodeFirst,
    CodeNonfirst,
}

/// Tries to match the start of `text` against the prefix of the block described by
/// `indent`. On a match, strips the prefix from `text` and sets the prefix text(s) in
/// `indent` (what gets rendered in the aligned prefix column).
fn match_block_prefix(text: &mut String, indent: &mut LineIndent, mode: BlockMatch) -> BlockMatched {
    let continuing = matches!(mode, BlockMatch::Continue { .. });

    // An empty line following a line in a block (i.e. a new line from pressing Enter) stays
    // in the block, and gets the block's continuation prefix.
    if continuing && text.is_empty() && !matches!(indent.type_, BlockType::Code) {
        if matches!(indent.type_, BlockType::Quote) {
            indent.text = PREFIX_BLOCKQUOTE.to_string();
        }
        return BlockMatched::Yes;
    }
    match indent.type_ {
        BlockType::Quote => {
            let Some(suffix) = text.strip_prefix(PREFIX_BLOCKQUOTE) else {
                return BlockMatched::No;
            };
            *text = suffix.to_string();
            indent.text = PREFIX_BLOCKQUOTE.to_string();
        },
        BlockType::Ul => {
            if let Some(suffix) = text.strip_prefix(PREFIX_UL_FIRST) {
                *text = suffix.to_string();
                indent.first_text = Some(PREFIX_UL_FIRST.to_string());
                indent.text = PREFIX_UL.to_string();
            } else if let Some(suffix) = text.strip_prefix(PREFIX_UL_FIRST2) {
                *text = suffix.to_string();
                indent.first_text = Some(PREFIX_UL_FIRST2.to_string());
                indent.text = PREFIX_UL.to_string();
            } else if let Some(suffix) = text.strip_prefix(&indent.text).filter(|_| continuing) {
                *text = suffix.to_string();
            } else {
                return BlockMatched::No;
            }
        },
        BlockType::Ol => {
            if let Some(parsed) = parse_ol_number_prefix(text) {
                *text = parsed.suffix;
                indent.text = " ".repeat(parsed.number.len());
                indent.first_text = Some(parsed.number);
            } else if let Some(suffix) = text.strip_prefix(&indent.text).filter(|_| continuing) {
                *text = suffix.to_string();
            } else {
                return BlockMatched::No;
            }
        },
        BlockType::Code => {
            match mode {
                BlockMatch::Continue { context_codeblock_end } => {
                    if context_codeblock_end {
                        return BlockMatched::No;
                    }
                    return BlockMatched::CodeNonfirst;
                },
                BlockMatch::First => {
                    if !text.starts_with(PREFIX_BLOCKCODE) {
                        return BlockMatched::No;
                    }
                    return BlockMatched::CodeFirst;
                },
            }
        },
    }
    return BlockMatched::Yes;
}

fn generate_aligned(out_want_children: &mut Vec<ShadowDom>, indents: &mut [LineIndent]) {
    for indent in indents {
        if indent.first_text.is_none() && indent.text.is_empty() {
            // Empty line within a block - no prefix text, and an empty span would only get
            // in the way of caret placement
            continue;
        }
        out_want_children.push(ShadowDom::Element(ShadowDomElement {
            tag: "span",
            classes: [
                CLASS_NAMESPACE.to_string(),
                CLASS_ALIGNED.to_string(),
                format!("{}_aligned_{}", NAMESPACE, indent.source_indent),
            ]
                .into_iter()
                .collect(),
            attrs: [(ATTR_INDENT_ID.to_string(), indent.source_indent.to_string())].into_iter().collect(),
            //. children: vec![ ShadowDom::Text(indent.first_text.take().unwrap_or_else(|| indent.text.clone()))],
            children: vec![ShadowDom::Element(ShadowDomElement {
                tag: "span",
                classes: [CLASS_NAMESPACE.to_string()].into_iter().collect(),
                attrs: Default::default(),
                children: vec![ShadowDom::Text(indent.first_text.take().unwrap_or_else(|| indent.text.clone()))],
            })],
        }));
    }
}

#[derive(Debug, PartialEq, Eq)]
enum BlockcodeDelim {
    Begin,
    End,
}

fn parse_px(v: &str) -> Option<f64> {
    return v.trim().strip_suffix("px")?.parse::<f64>().ok();
}

fn realign(root: &Element) {
    if !root.is_connected() {
        return;
    }
    let mut blocks = HashMap::new();
    {
        let els = root.get_elements_by_class_name(CLASS_BLOCK);
        for i in 0 .. els.length() {
            let Some(e) = els.item(i) else {
                continue;
            };
            let Some(id) = e.get_attribute(ATTR_ID).and_then(|x| x.parse::<usize>().ok()) else {
                continue;
            };
            let Ok(e) = e.dyn_into::<HtmlElement>() else {
                continue;
            };
            blocks.insert(id, e);
        }
    }
    let mut aligned = vec![];
    {
        let els = root.get_elements_by_class_name(CLASS_ALIGNED);
        for i in 0 .. els.length() {
            let Some(e) = els.item(i) else {
                continue;
            };
            let Some(id) = e.get_attribute(ATTR_INDENT_ID).and_then(|x| x.parse::<usize>().ok()) else {
                continue;
            };
            let Ok(e) = e.dyn_into::<HtmlElement>() else {
                continue;
            };
            aligned.push((id, e));
        }
    }
    for b in blocks.values() {
        _ = b.style().remove_property("padding-left");
    }
    let mut prefix_widths = HashMap::<usize, f64>::new();
    for (id, e) in &aligned {
        let width = e.first_element_child().map(|inner| inner.get_bounding_client_rect().width()).unwrap_or(0.);
        let have = prefix_widths.entry(*id).or_insert(0.);
        if width > *have {
            *have = width;
        }
    }
    let mut set_paddings = vec![];
    for (id, b) in &blocks {
        let base =
            window()
                .get_computed_style(b)
                .ok()
                .flatten()
                .and_then(|s| s.get_property_value("padding-left").ok())
                .and_then(|v| parse_px(&v))
                .unwrap_or(0.);
        let want = prefix_widths.get(id).copied().unwrap_or(0.);
        if want > base {
            set_paddings.push((b, want));
        }
    }
    for (b, want) in set_paddings {
        _ = b.style().set_property("padding-left", &format!("{}px", want));
    }
    let mut set_lefts = vec![];
    for (id, e) in &aligned {
        let Some(b) = blocks.get(id) else {
            continue;
        };
        let style = e.style();
        let have = style.get_property_value("left").ok().and_then(|v| parse_px(&v)).unwrap_or(0.);
        let want = b.get_bounding_client_rect().left() - e.get_bounding_client_rect().left() + have;
        if (have - want).abs() > 0.25 {
            set_lefts.push((style, want));
        }
    }
    for (style, want) in set_lefts {
        _ = style.set_property("left", &format!("{}px", want));
    }
}

thread_local!{
    static NEXT_ID: Cell<usize> = Cell::new(0);
    static FONT_LISTENER: Cell<bool> = Cell::new(false);
}

fn is_line(node: &Node) -> bool {
    let Some(parent) = node.parent_element() else {
        return false;
    };
    let parent_classes = parent.class_list();
    if !(parent_classes.contains(CLASS_BLOCK) || parent_classes.contains(CLASS_ROOT)) {
        return false;
    }
    return true;
}

fn is_root(at: &Node) -> bool {
    return at.dyn_ref::<Element>().map_or(true, |e| e.class_list().contains(CLASS_ROOT));
}

fn line_scan(start: &Node, direction: ScanDirection, inclusivity: ScanInclusivity) -> Option<Node> {
    return dom::scan(start, is_root, |at| {
        if !is_line(at) {
            return false;
        }
        if let Some(at_el) = at.dyn_ref::<Element>() {
            let class_list = at_el.class_list();
            if class_list.contains(CLASS_BLOCK) {
                return false;
            }
        }
        return true;
    }, direction, inclusivity);
}

struct OlNumberPrefix {
    /// Like `12. `
    number: String,
    suffix: String,
}

/// Matches `^([0-9]+\. )(.*)$`
fn parse_ol_number_prefix(text: &str) -> Option<OlNumberPrefix> {
    let digits = text.bytes().take_while(|b| b.is_ascii_digit()).count();
    if digits == 0 {
        return None;
    }
    let rest = &text[digits ..];
    if !rest.starts_with(". ") {
        return None;
    }
    let number_len = digits + 2;
    return Some(OlNumberPrefix {
        number: text[.. number_len].to_string(),
        suffix: text[number_len ..].to_string(),
    });
}

/// Matches `^(#{1,6}) `, returns the heading level
fn parse_heading_prefix(text: &str) -> Option<usize> {
    let level = text.bytes().take_while(|b| *b == b'#').count();
    if level == 0 || level > 6 || !text[level ..].starts_with(' ') {
        return None;
    }
    return Some(level);
}

fn generate_el_block_indent(type_: BlockType, type_class: &str) -> (usize, Element) {
    let e = document().create_element("div").unwrap();
    let id = NEXT_ID.with(|x| {
        let out = x.get();
        x.set(out + 1);
        return out;
    });
    e.class_list().add_3(CLASS_BLOCK, type_class, &format!("{}_indent_{}", NAMESPACE, id)).unwrap();
    e.set_attribute(ATTR_ID, &id.to_string()).unwrap();
    e.set_attribute(ATTR_BLOCK_TYPE, match type_ {
        BlockType::Quote => BLOCK_TYPE_QUOTE,
        BlockType::Ul => BLOCK_TYPE_UL,
        BlockType::Ol => BLOCK_TYPE_OL,
        BlockType::Code => BLOCK_TYPE_CODE,
    }).unwrap();
    return (id, e);
}

fn req<T>(o: Option<T>, what: &'static str) -> Result<T, String> {
    return o.ok_or_else(|| format!("Missing {}", what));
}

fn jsok<T>(r: Result<T, JsValue>, what: &'static str) -> Result<T, String> {
    return r.map_err(|e| format!("DOM error ({}): {:?}", what, e));
}

fn find_root(node: &Node) -> Option<Element> {
    let mut at = node.parent_element()?;
    while !at.class_list().contains(CLASS_ROOT) {
        at = at.parent_element()?;
    }
    return Some(at);
}

/// Position of (node, offset) within `line` as (segment index, utf16 offset in segment),
/// where segments are delimited by the `brs`.
fn br_segment_pos(line: &Node, brs: &[Node], node: &Node, offset: u32) -> Option<(usize, usize)> {
    if !line.contains(Some(node)) {
        return None;
    }
    let r = document().create_range().ok()?;
    r.set_start(line, 0).ok()?;
    r.set_end(node, offset).ok()?;
    let mut segment = 0;
    for br in brs {
        if !r.intersects_node(br).ok()? {
            break;
        }
        segment += 1;
        r.set_start_after(br).ok()?;
    }
    return Some((segment, r.unchecked_ref::<js_sys::Object>().to_string().length() as usize));
}

/// Browsers represent line breaks within a block with `<br>` (e.g. Chrome merges two empty
/// lines into one `<p><br><br></p>`). The editor's model is one element per line, so split
/// such a line into several, moving the selection along. Returns whether anything changed.
fn split_line_at_brs(line: &Element) -> Result<bool, String> {
    let brs = jsok(line.query_selector_all("br"), "query br")?;
    if brs.length() == 0 {
        return Ok(false);
    }
    let brs = (0 .. brs.length()).filter_map(|i| brs.item(i)).collect::<Vec<_>>();
    let line_node: &Node = line;

    // Selection, relative to segments
    let (sel_anchor, sel_focus) = match document().get_selection() {
        Ok(Some(sel)) => (
            sel.anchor_node().and_then(|n| br_segment_pos(line_node, &brs, &n, sel.anchor_offset())),
            sel.focus_node().and_then(|n| br_segment_pos(line_node, &brs, &n, sel.focus_offset())),
        ),
        _ => (None, None),
    };

    // Segment texts
    fn walk(n: &Node, segments: &mut Vec<String>) {
        if n.node_type() == Node::TEXT_NODE {
            segments.last_mut().unwrap().push_str(&n.node_value().unwrap_or_default());
        } else if n.node_type() == Node::ELEMENT_NODE {
            if n.dyn_ref::<Element>().map_or(false, |e| e.tag_name().eq_ignore_ascii_case("br")) {
                segments.push(String::new());
                return;
            }
            let children = n.child_nodes();
            for i in 0 .. children.length() {
                if let Some(child) = children.item(i) {
                    walk(&child, segments);
                }
            }
        }
    }

    let mut segments = vec![String::new()];
    walk(line_node, &mut segments);
    if segments.len() > 1 && segments.last().map_or(false, |s| s.is_empty()) {
        // A trailing <br> is a placeholder for the (empty) last line, not a break
        segments.pop();
    }
    if segments.len() == 1 {
        // Just a placeholder (as the editor itself renders empty lines)
        return Ok(false);
    }

    // Rebuild as separate lines
    let parent = req(line.parent_node(), "line parent")?;
    let mut lines = vec![line.clone()];
    let mut place_before = line.next_sibling();
    for (i, segment) in segments.iter().enumerate() {
        if i == 0 {
            line.set_text_content(Some(segment));
            continue;
        }
        let new_line = jsok(document().create_element("p"), "create line")?;
        jsok(new_line.class_list().add_2(CLASS_NAMESPACE, CLASS_LINE), "line classes")?;
        new_line.set_text_content(Some(segment));
        jsok(parent.insert_before(&new_line, place_before.as_ref()), "insert split line")?;
        place_before = new_line.next_sibling();
        lines.push(new_line);
    }
    let locate = |pos: Option<(usize, usize)>| -> Option<(Node, u32)> {
        let (segment, offset) = pos?;
        let segment = segment.min(lines.len() - 1);
        let line: &Node = &lines[segment];
        return Some(dom::locate_text_offset(line, offset));
    };
    if let (Some((anchor_node, anchor_offset)), Some((focus_node, focus_offset))) =
        (locate(sel_anchor), locate(sel_focus)) {
        dom::select_range(&anchor_node, anchor_offset, &focus_node, focus_offset);
    }
    return Ok(true);
}

/// Width of the prefix column for `block`, taken from the nearest line at or before
/// `line` (within the block) that has text in that column (empty lines have no prefix).
fn continuation_width(line: &Node, block: &Element, indent_id: usize) -> usize {
    let mut at = Some(line.clone());
    while let Some(l) = at {
        if !block.contains(Some(&l)) {
            break;
        }
        if let Some(el) = l.dyn_ref::<Element>() {
            let children = el.children();
            for i in 0 .. children.length() {
                let Some(c) = children.item(i) else {
                    continue;
                };
                if c.get_attribute(ATTR_INDENT_ID).and_then(|x| x.parse::<usize>().ok()) != Some(indent_id) {
                    continue;
                }
                let len = c.text_content().unwrap_or_default().chars().count();
                if len > 0 {
                    return len;
                }
            }
        }
        at = line_scan(&l, ScanDirection::Backward, ScanInclusivity::Exclusive);
    }
    return 0;
}

fn recurse_get_text(n: &Node, out_text: &mut String) {
    if n.node_type() == Node::TEXT_NODE {
        out_text.push_str(&n.node_value().unwrap_or_default());
    } else if n.node_type() == Node::ELEMENT_NODE {
        let nodes = n.child_nodes();
        for i in 0 .. nodes.length() {
            if let Some(child) = nodes.item(i) {
                recurse_get_text(&child, out_text);
            }
        }
    }
}

/// Re-derives block structure and inline styling for `line` and following lines until
/// nothing changes. Errors indicate the DOM wasn't in the expected shape; the caller
/// should rebuild the editor from its text.
fn update_lines_starting_at(mut line: Node) -> Result<(), String> {
    let mut context_line = line_scan(&line, ScanDirection::Backward, ScanInclusivity::Exclusive);

    // Locate root
    let root;
    {
        let Some(mut parent_at) = line.parent_element() else {
            return Ok(());
        };
        while !parent_at.class_list().contains(CLASS_ROOT) {
            let Some(temp) = parent_at.parent_element() else {
                return Ok(());
            };
            parent_at = temp;
        }
        root = parent_at;
    }

    // Start walking
    loop {
        let mut changed = false;
        let mut indents = vec![];
        if let Some(line_el) = line.dyn_ref::<Element>() {
            if split_line_at_brs(line_el)? {
                changed = true;
            }
        }
        let (sel_anchor, sel_focus) = match document().get_selection() {
            Ok(Some(sel)) => (
                sel.anchor_node().and_then(|n| dom::text_offset_in(&line, &n, sel.anchor_offset())),
                sel.focus_node().and_then(|n| dom::text_offset_in(&line, &n, sel.focus_offset())),
            ),
            _ => (None, None),
        };
        let mut text = String::new();
        recurse_get_text(&line, &mut text);

        // Find default root
        let mut place_before = line.next_sibling();
        let mut place_parent = req(line.parent_element(), "line parent")?;
        while place_parent != root {
            place_before = place_parent.next_sibling();
            place_parent = req(place_parent.parent_element(), "block parent")?;
        }

        // If there's a context line
        let mut in_blockcode = false;
        let mut blockcode_is_nonfirst = false;

        // Prefix text generated for an empty line (continuing the enclosing blocks) isn't in
        // the line's text yet; the selection needs shifting past it after rendering
        let mut generated_prefix = 0;
        if let Some(context_line) = context_line {
            // Determine context by walking previous line(/context)'s block parents up to root
            let mut root_context_path = vec![];
            let mut at_parent = req(context_line.parent_element(), "context line parent")?;
            let mut next_child = context_line.next_sibling();
            while at_parent != root {
                let id = req(at_parent.get_attribute(ATTR_ID).and_then(|x| x.parse::<usize>().ok()), "block id")?;
                root_context_path.push((at_parent.clone(), next_child, LineIndent {
                    type_: BlockType::from_str(&req(at_parent.get_attribute(ATTR_BLOCK_TYPE), "block type")?)?,
                    source_indent: id,
                    first_text: None,
                    text: "".to_string(),
                }));
                next_child = at_parent.next_sibling();
                at_parent = req(at_parent.parent_element(), "block parent")?;
            }
            root_context_path.reverse();
            for (block, _, block_indent) in root_context_path.iter_mut() {
                block_indent.text = " ".repeat(continuation_width(&context_line, block, block_indent.source_indent));
            }

            // Find higest indent of previous/context this should be placed under by matching
            // indents to line prefixes
            for (parent, parent_next, mut indent) in root_context_path {
                let context_codeblock_end =
                    context_line.dyn_ref::<Element>().map_or(false, |e| e.has_attribute(ATTR_BLOCKCODE_END));
                let was_empty = text.is_empty();
                match match_block_prefix(&mut text, &mut indent, BlockMatch::Continue { context_codeblock_end }) {
                    BlockMatched::No => break,
                    BlockMatched::Yes => {
                        if was_empty {
                            generated_prefix += dom::utf16_len(&indent.text);
                        }
                    },
                    BlockMatched::CodeNonfirst => {
                        in_blockcode = true;
                        blockcode_is_nonfirst = true;
                    },
                    BlockMatched::CodeFirst => unreachable!(),
                }
                place_parent = parent;
                place_before = parent_next;
                indents.push(indent);
            }
            if let Some(place_before1) = &place_before {
                if place_before1 == &line {
                    // `place_before` from context is the next element which is the line or a parent
                    // of line, and we actually want the line after that.
                    place_before = place_before1.next_sibling();
                }
            }
        }

        // Check if in a subtree of the placement root (and figure out the subpath indents)
        let mut context_line_path = vec![];
        let mut lift_following = vec![];
        superif!({
            let mut at_parent = req(line.parent_element(), "line parent")?;
            let mut next_child = line.next_sibling();
            loop {
                if at_parent == place_parent {
                    break 'line_under_place_parent;
                }
                if at_parent == root {
                    break;
                }
                let id = req(at_parent.get_attribute(ATTR_ID).and_then(|x| x.parse::<usize>().ok()), "block id")?;
                context_line_path.push((at_parent.clone(), next_child, LineIndent {
                    type_: BlockType::from_str(&req(at_parent.get_attribute(ATTR_BLOCK_TYPE), "block type")?)?,
                    source_indent: id,
                    first_text: None,
                    text: "".to_string(),
                }));
                next_child = at_parent.next_sibling();
                at_parent = req(at_parent.parent_element(), "block parent")?;
            }
        } 'line_under_place_parent {
            context_line_path.reverse();

            // Move placement parent down current line's tree by matching prefixes
            for (parent, parent_next, mut indent) in context_line_path {
                match match_block_prefix(&mut text, &mut indent, BlockMatch::First) {
                    BlockMatched::No => break,
                    BlockMatched::Yes => { },
                    BlockMatched::CodeFirst => {
                        in_blockcode = true;
                    },
                    BlockMatched::CodeNonfirst => unreachable!(),
                }
                place_parent = parent;
                place_before = parent_next;
                indents.push(indent);
            }

            // Could be splitting a parent container; get list of following lines to drag
            // along to the (final) placement root (and the split parents will be recreated
            // later). This must happen after the placement root is settled above, otherwise
            // `place_before` could refer to a lifted (detached) node.
            shed!{
                let mut lift_at = line.clone();
                if req(lift_at.parent_element(), "lift parent")? == place_parent {
                    break;
                }
                loop {
                    // Move forward
                    loop {
                        let Some(lift_at_next) = lift_at.next_sibling() else {
                            break;
                        };
                        lift_at = lift_at_next;
                        lift_following.push(lift_at.clone());
                    };

                    // Can't move forward, try moving up (then resume moving forward)
                    lift_at = req(lift_at.parent_node(), "lift parent")?;
                    if req(lift_at.parent_element(), "lift parent")? == place_parent {
                        // Reached placement root, stop
                        place_before = lift_at.next_sibling();
                        break;
                    }
                }
            }
        });

        // Check if starting new prefixed blocks
        let mut create_parents = vec![];
        let mut blockcode_delim = None;
        loop {
            if in_blockcode {
                if text.starts_with(PREFIX_BLOCKCODE) {
                    blockcode_delim = Some(if blockcode_is_nonfirst {
                        BlockcodeDelim::End
                    } else {
                        BlockcodeDelim::Begin
                    });
                }
                break;
            }
            if let Some(suffix1) = text.strip_prefix(PREFIX_BLOCKQUOTE) {
                text = suffix1.to_string();
                let type_ = BlockType::Quote;
                let (parent_id, parent) = generate_el_block_indent(type_, ns!("_block_quote"));
                create_parents.push((parent, LineIndent {
                    source_indent: parent_id,
                    type_: BlockType::Quote,
                    first_text: None,
                    text: PREFIX_BLOCKQUOTE.to_string(),
                }));
            } else if let Some(suffix1) = text.strip_prefix(PREFIX_UL_FIRST) {
                text = suffix1.to_string();
                let type_ = BlockType::Ul;
                let (parent_id, parent) = generate_el_block_indent(type_, CLASS_UL);
                create_parents.push((parent, LineIndent {
                    source_indent: parent_id,
                    type_: type_,
                    first_text: Some(PREFIX_UL_FIRST.to_string()),
                    text: PREFIX_UL.to_string(),
                }));
            } else if let Some(suffix1) = text.strip_prefix(PREFIX_UL_FIRST2) {
                text = suffix1.to_string();
                let type_ = BlockType::Ul;
                let (parent_id, parent) = generate_el_block_indent(type_, CLASS_UL);
                create_parents.push((parent, LineIndent {
                    source_indent: parent_id,
                    type_: type_,
                    first_text: Some(PREFIX_UL_FIRST2.to_string()),
                    text: PREFIX_UL.to_string(),
                }));
            } else if let Some(parsed) = parse_ol_number_prefix(&text) {
                text = parsed.suffix;
                let type_ = BlockType::Ol;
                let (parent_id, parent) = generate_el_block_indent(type_, ns!("_block_ol"));
                create_parents.push((parent, LineIndent {
                    source_indent: parent_id,
                    type_: type_,
                    text: " ".repeat(parsed.number.len()),
                    first_text: Some(parsed.number),
                }));
            } else if text.starts_with(PREFIX_BLOCKCODE) {
                if blockcode_is_nonfirst {
                    blockcode_delim = Some(BlockcodeDelim::End);
                } else {
                    blockcode_delim = Some(BlockcodeDelim::Begin);
                }
                if !in_blockcode {
                    let type_ = BlockType::Code;
                    let (parent_id, parent) = generate_el_block_indent(type_, ns!("_block_code"));
                    create_parents.push((parent, LineIndent {
                        source_indent: parent_id,
                        type_: type_,
                        first_text: None,
                        text: "".to_string(),
                    }));
                    in_blockcode = true;
                }
                break;
            } else {
                break;
            }
        }

        // Move
        let original_parent = req(line.parent_element(), "line parent")?;
        if place_parent != original_parent || !create_parents.is_empty() {
            changed = true;

            // Lift line + any following elements between the line and the placement root
            jsok(req(line.parent_node(), "line parent")?.remove_child(&line), "remove line")?;
            for e in &lift_following {
                jsok(req(e.parent_node(), "lifted line parent")?.remove_child(&e), "remove lifted line")?;
            }

            // Create blocks and place this line
            {
                let mut place_parent = place_parent.clone();
                let mut place_before = place_before.clone();
                for (new_parent, indent) in create_parents.into_iter() {
                    jsok(place_parent.insert_before(&new_parent, place_before.as_ref()), "insert block")?;
                    place_parent = new_parent;
                    place_before = None;
                    indents.push(indent);
                }
                jsok(place_parent.insert_before(&line, place_before.as_ref()), "insert line")?;
            }

            // Place remaining lines
            for e in lift_following {
                jsok(place_parent.insert_before(&e, place_before.as_ref()), "insert lifted line")?;
            }

            // (Clean up empty left over blocks/containers)
            let mut clean_up_at = original_parent.clone();
            let mut clean_up_until = None;
            loop {
                if clean_up_at == place_parent {
                    break;
                }
                if clean_up_at.child_element_count() > 0 {
                    break;
                }
                clean_up_until = Some(clean_up_at.clone());
                clean_up_at = req(clean_up_at.parent_element(), "block parent")?;
            }
            if let Some(clean_up_until) = clean_up_until {
                clean_up_until.remove();
            }
        }
        // Browsers (Chrome at least) expect an empty block to hold a placeholder <br>; without
        // it, merging two empty lines produces one element with two breaks.
        fn ensure_placeholder(children: &mut Vec<ShadowDom>) {
            if children.iter().all(|c| match c {
                ShadowDom::Text(t) => t.is_empty(),
                ShadowDom::Element(_) => false,
            }) {
                *children = vec![ShadowDom::Element(ShadowDomElement {
                    tag: "br",
                    classes: Default::default(),
                    attrs: Default::default(),
                    children: vec![],
                })];
            }
        }

        let line_shadowdom = if in_blockcode {
            shed!{
                let Some(line) = line.dyn_ref::<Element>() else {
                    changed = true;
                    break;
                };
                let blockcode_was_delim = if line.get_attribute(ATTR_BLOCKCODE_BEGIN).is_some() {
                    Some(BlockcodeDelim::Begin)
                } else if line.get_attribute(ATTR_BLOCKCODE_END).is_some() {
                    Some(BlockcodeDelim::End)
                } else {
                    None
                };
                if blockcode_was_delim != blockcode_delim {
                    changed = true;
                    break;
                }
            }
            {
                let mut want_children = vec![];
                generate_aligned(&mut want_children, &mut indents);
                want_children.push(ShadowDom::Text(text));
                ensure_placeholder(&mut want_children);
                ShadowDom::Element(ShadowDomElement {
                    tag: "codeline",
                    classes: [CLASS_NAMESPACE.to_string(), CLASS_LINE.to_string()].into_iter().collect(),
                    attrs: {
                        let mut out = HashMap::new();
                        out.insert("spellcheck".to_string(), "false".to_string());
                        if let Some(delim) = blockcode_delim {
                            match delim {
                                BlockcodeDelim::Begin => {
                                    out.insert(ATTR_BLOCKCODE_BEGIN.to_string(), "1".to_string());
                                },
                                BlockcodeDelim::End => {
                                    out.insert(ATTR_BLOCKCODE_END.to_string(), "1".to_string());
                                },
                            }
                        }
                        out
                    },
                    children: want_children,
                })
            }
        } else {
            let heading_level = parse_heading_prefix(&text);

            // Parse inline
            let inline = linemarkdown::parse(&text);
            {
                let mut want_children = vec![];
                generate_aligned(&mut want_children, &mut indents);

                fn generate_delimited(
                    tag: &'static str,
                    begin_delim: String,
                    children: Vec<Inline>,
                    end_delim: Option<String>,
                ) -> ShadowDom {
                    return ShadowDom::Element(ShadowDomElement {
                        tag: tag,
                        classes: {
                            let mut out = HashSet::new();
                            out.insert(CLASS_NAMESPACE.to_string());
                            if end_delim.is_none() {
                                out.insert(CLASS_INCOMPLETE.to_string());
                            }
                            out
                        },
                        attrs: Default::default(),
                        children: {
                            let mut out = vec![];
                            out.push(ShadowDom::Text(begin_delim));
                            for c in children {
                                out.push(generate_inline(c));
                            }
                            if let Some(end_delim) = end_delim {
                                out.push(ShadowDom::Text(end_delim));
                            }
                            out
                        },
                    });
                }

                fn generate_inline(want: Inline) -> ShadowDom {
                    match want {
                        Inline::Text(want) => {
                            return ShadowDom::Text(want);
                        },
                        Inline::Strong(want) => {
                            return generate_delimited("strong", want.begin_delim, want.children, want.end_delim);
                        },
                        Inline::Emphasis(want) => {
                            return generate_delimited("em", want.begin_delim, want.children, want.end_delim);
                        },
                        Inline::Strikethrough(want) => {
                            return generate_delimited("s", want.begin_delim, want.children, want.end_delim);
                        },
                        Inline::Link(want) => {
                            return ShadowDom::Element(ShadowDomElement {
                                tag: "span",
                                classes: {
                                    let mut out = HashSet::new();
                                    out.insert(CLASS_NAMESPACE.to_string());
                                    out.insert(ns!("_inline_pseudo_a").to_string());
                                    if want.incomplete {
                                        out.insert(CLASS_INCOMPLETE.to_string());
                                    }
                                    out
                                },
                                attrs: Default::default(),
                                children: {
                                    let mut out = vec![];
                                    out.push(ShadowDom::Text(want.title_begin_delim));
                                    for c in want.title {
                                        out.push(generate_inline(c));
                                    }
                                    if let Some(continuation) = want.continuation {
                                        out.push(ShadowDom::Text(continuation.title_end_delim));
                                        if let Some(address) = continuation.address {
                                            out.push(ShadowDom::Element(ShadowDomElement {
                                                tag: "a",
                                                classes: [CLASS_NAMESPACE.to_string()].into_iter().collect(),
                                                attrs: {
                                                    let mut out = HashMap::new();
                                                    out.insert("spellcheck".to_string(), "false".to_string());
                                                    out.insert("href".to_string(), address.address.clone());
                                                    out
                                                },
                                                children: vec![
                                                    ShadowDom::Text(
                                                        format!(
                                                            "{}{}{}",
                                                            address.begin_delim,
                                                            address.address,
                                                            address.end_delim
                                                        ),
                                                    ),
                                                ],
                                            }));
                                        }
                                    }
                                    out
                                },
                            });
                        },
                        Inline::Code(want) => {
                            return ShadowDom::Element(ShadowDomElement {
                                tag: "code",
                                classes: {
                                    let mut out = HashSet::new();
                                    out.insert(CLASS_NAMESPACE.to_string());
                                    if want.incomplete {
                                        out.insert(CLASS_INCOMPLETE.to_string());
                                    }
                                    out
                                },
                                attrs: {
                                    let mut out = HashMap::new();
                                    out.insert("spellcheck".to_string(), "false".to_string());
                                    out
                                },
                                children: vec![ShadowDom::Text(want.text)],
                            });
                        },
                    }
                }

                let mut classes = HashSet::new();
                classes.insert(CLASS_NAMESPACE.to_string());
                classes.insert(CLASS_LINE.to_string());
                if heading_level.is_none() && inline.is_empty() {
                    classes.insert(ns!("_empty").to_string());
                }
                for inline in inline {
                    want_children.push(generate_inline(inline));
                }
                ensure_placeholder(&mut want_children);
                ShadowDom::Element(ShadowDomElement {
                    tag: match heading_level {
                        None => "p",
                        Some(1) => "h1",
                        Some(2) => "h2",
                        Some(3) => "h3",
                        Some(4) => "h4",
                        Some(5) => "h5",
                        Some(6) => "h6",
                        Some(_) => unreachable!(),
                    },
                    classes: classes,
                    attrs: Default::default(),
                    children: want_children,
                })
            }
        };
        let mut dom_changed = changed;
        line = sync_shadow_dom(&mut dom_changed, &line, line_shadowdom);
        if dom_changed && (sel_anchor.is_some() || sel_focus.is_some()) {
            shed!{
                let Ok(Some(sel)) = document().get_selection() else {
                    break;
                };
                let resolve = |offset: Option<usize>, node: Option<Node>, node_offset: u32| -> Option<(Node, u32)> {
                    match offset {
                        Some(offset) => return Some(dom::locate_text_offset(&line, offset)),
                        None => return Some((node?, node_offset)),
                    }
                };
                let Some((anchor_node, anchor_offset)) =
                    resolve(sel_anchor.map(|o| o + generated_prefix), sel.anchor_node(), sel.anchor_offset()) else {
                        break;
                    };
                let Some((focus_node, focus_offset)) =
                    resolve(sel_focus.map(|o| o + generated_prefix), sel.focus_node(), sel.focus_offset()) else {
                        break;
                    };
                dom::select_range(&anchor_node, anchor_offset, &focus_node, focus_offset);
            };
        }

        // Continue until nothing changes
        if !changed {
            break;
        }
        let Some(at1) = line_scan(&line, ScanDirection::Forward, ScanInclusivity::Exclusive) else {
            break;
        };
        context_line = Some(line);
        line = at1;
    }
    return Ok(());
}

fn load_text(root: &Element, text: &str) {
    load_lines(root, &textformat::import_lines(text));
}

fn load_lines(root: &Element, line_texts: &[String]) {
    root.set_inner_html("");
    let mut lines = vec![];
    for line_text in line_texts {
        let line = document().create_element("p").unwrap();
        line.class_list().add_2(CLASS_NAMESPACE, CLASS_LINE).unwrap();
        line.set_text_content(Some(line_text));
        _ = root.append_child(&line);
        lines.push(line);
    }
    for line in lines {
        if let Err(e) = update_lines_starting_at(line.into()) {
            console::error_1(&format!("malarkdowney: error structuring line: {}", e).into());
        }
    }
    _ = root.class_list().toggle_with_force(CLASS_BLANK, line_texts.iter().all(|l| l.is_empty()));
    realign(root);
}

fn all_lines(root: &Element) -> Vec<Node> {
    let mut out = vec![];
    let Some(first) = root.first_child() else {
        return out;
    };
    let mut at = line_scan(&first, ScanDirection::Forward, ScanInclusivity::Inclusive);
    while let Some(line) = at {
        at = line_scan(&line, ScanDirection::Forward, ScanInclusivity::Exclusive);
        out.push(line);
    }
    return out;
}

fn export_line(line: &Node) -> ExportLine {
    let mut text = String::new();
    recurse_get_text(line, &mut text);
    let line_el = line.dyn_ref::<Element>();
    let code = line_el.map_or(false, |e| e.tag_name().eq_ignore_ascii_case("codeline"));
    let mut prefix_lens = HashMap::new();
    let mut prefix_total = 0;
    if let Some(line_el) = line_el {
        let children = line_el.children();
        for i in 0 .. children.length() {
            let Some(child) = children.item(i) else {
                continue;
            };
            let Some(id) = child.get_attribute(ATTR_INDENT_ID).and_then(|x| x.parse::<usize>().ok()) else {
                continue;
            };
            let len = child.text_content().unwrap_or_default().chars().count();
            prefix_lens.insert(id, len);
            prefix_total += len;
        }
    }
    let mut blocks = vec![];
    let mut at = line.parent_element();
    while let Some(parent) = at {
        let classes = parent.class_list();
        if classes.contains(CLASS_ROOT) {
            break;
        }
        if classes.contains(CLASS_BLOCK) {
            if let Some(id) = parent.get_attribute(ATTR_ID).and_then(|x| x.parse::<usize>().ok()) {
                blocks.push(ExportBlock {
                    id: id,
                    quote: parent.get_attribute(ATTR_BLOCK_TYPE).as_deref() == Some(BLOCK_TYPE_QUOTE),
                    prefix_len: prefix_lens.get(&id).copied().unwrap_or(0),
                });
            }
        }
        at = parent.parent_element();
    }
    blocks.reverse();
    return ExportLine {
        content_empty: text.chars().count() <= prefix_total,
        text: text,
        code: code,
        blocks: blocks,
    };
}

pub fn get_text_lines(root: &Element) -> Vec<String> {
    flush(root);
    return raw_lines(root);
}

fn raw_lines(root: &Element) -> Vec<String> {
    return all_lines(root).iter().map(|line| {
        let mut out = String::new();
        recurse_get_text(line, &mut out);
        return out;
    }).collect();
}

struct PendingState {
    timeout: Option<Timeout>,
    lines: Vec<Node>,
}

fn queue_mutations(pending: &mut PendingState, mutations: Array) {
    for m in mutations.iter() {
        let m = MutationRecord::from(m);
        let mut starts = vec![];

        // Start from added elements
        let added_nodes = m.added_nodes();
        for i in 0 .. added_nodes.length() {
            if let Some(at) = added_nodes.item(i) {
                starts.push(at);
            }
        }

        // Then get first element after removed elements or use the target node as the
        // starting point
        let following = shed!{
            'start _;
            if m.type_() == "childList" {
                shed!{
                    let previous = if added_nodes.length() == 0 {
                        let Some(previous) = m.previous_sibling() else {
                            break;
                        };
                        previous
                    } else {
                        let Some(last_added) = added_nodes.item(added_nodes.length() - 1) else {
                            break;
                        };
                        last_added
                    };
                    if let Some(following) = previous.next_sibling() {
                        break 'start Some(following);
                    }
                };
            }
            // Otherwise maybe child of a line/block
            break 'start m.target();
        };
        starts.extend(following);
        for start in starts {
            let line = shed!{
                'found _;
                // Search for a line upward
                let mut at = start.clone();
                loop {
                    if let Some(at_el) = at.dyn_ref::<Element>() {
                        let classes = at_el.class_list();
                        if classes.contains(CLASS_BLOCK) || classes.contains(CLASS_ROOT) {
                            break;
                        }
                    }
                    if is_line(&at) {
                        break 'found Some(at);
                    }
                    let Some(parent) = at.parent_node() else {
                        break;
                    };
                    at = parent;
                }
                break 'found line_scan(&start, ScanDirection::Forward, ScanInclusivity::Inclusive);
            };
            if let Some(line) = line {
                pending.lines.push(line);
            }
        }
    }
}

struct EditorState {
    id: usize,
    newline_on_enter: bool,
    pending: RefCell<PendingState>,
    history: RefCell<History>,
}

thread_local!{
    static EDITORS: RefCell<HashMap<usize, Weak<EditorState>>> = RefCell::new(HashMap::new());
}

impl Drop for EditorState {
    fn drop(&mut self) {
        let id = self.id;
        _ = EDITORS.try_with(|e| e.borrow_mut().remove(&id));
    }
}

fn lookup(root: &Element) -> Option<Rc<EditorState>> {
    let id = root.get_attribute(ATTR_EDITOR_ID)?.parse::<usize>().ok()?;
    return EDITORS.with(|e| e.borrow().get(&id).and_then(|w| w.upgrade()));
}

fn root_observer(root: &Element) -> Option<MutationObserver> {
    let state = Reflect::get(root, &JsValue::from_str(PROP_STATE)).ok().filter(|s| !s.is_undefined())?;
    return Reflect::get(&state, &JsValue::from_str("mutationObserver"))
        .ok()
        .filter(|o| !o.is_undefined())
        .map(|o| o.unchecked_into());
}

fn process_pending(state: &EditorState) {
    let lines = {
        let mut pending = state.pending.borrow_mut();
        pending.timeout = None;
        std::mem::take(&mut pending.lines)
    };
    let seen = js_sys::Set::new(&JsValue::UNDEFINED);
    let mut roots: Vec<Element> = vec![];

    // Text (and selection) per root before any restructuring, so that if the DOM turns out
    // to be in an unexpected shape the editor can be rebuilt from it rather than left in
    // an inconsistent state.
    let mut snapshots: Vec<(Element, Vec<String>, Option<SelPos>)> = vec![];
    let mut failed: Vec<Element> = vec![];
    for line in lines {
        if seen.has(&line) {
            continue;
        }
        seen.add(&line);
        let Some(root) = find_root(&line) else {
            continue;
        };
        if failed.contains(&root) {
            continue;
        }
        if !snapshots.iter().any(|(r, _, _)| r == &root) {
            snapshots.push((root.clone(), raw_lines(&root), sel_pos(&root)));
        }
        if !roots.contains(&root) {
            roots.push(root.clone());
        }
        if let Err(e) = update_lines_starting_at(line) {
            console::error_1(
                &format!("malarkdowney: error updating editor structure, rebuilding from text: {}", e).into(),
            );
            let (lines, sel) =
                snapshots
                    .iter()
                    .find(|(r, _, _)| r == &root)
                    .map(|(_, l, s)| (l.clone(), *s))
                    .unwrap_or_default();
            load_lines(&root, &lines);
            if let Some(observer) = root_observer(&root) {
                observer.take_records();
            }
            restore_sel(&root, sel);
            failed.push(root);
        }
    }
    for root in roots {
        realign(&root);
        let recorded = state.history.borrow_mut().record(raw_lines(&root), sel_pos(&root));
        if recorded {
            text_changed(&root);
        }
    }
}

fn restore_sel(root: &Element, sel: Option<SelPos>) {
    let Some(sel) = sel else {
        return;
    };
    if !document().active_element().as_ref().map_or(false, |e| root.contains(Some(e))) {
        return;
    }
    let lines = all_lines(root);
    let locate = |(line, offset): (usize, usize)| lines.get(line).map(|l| dom::locate_text_offset(l, offset));
    if let (Some((anchor_node, anchor_offset)), Some((focus_node, focus_offset))) =
        (locate(sel.anchor), locate(sel.focus)) {
        dom::select_range(&anchor_node, anchor_offset, &focus_node, focus_offset);
    }
}

/// Called whenever the text may have changed (debounced edits, undo/redo, set_text).
fn text_changed(root: &Element) {
    let blank = raw_lines(root).iter().all(|l| l.is_empty());
    _ = root.class_list().toggle_with_force(CLASS_BLANK, blank);
    for name in ["malarkdowney-history", "malarkdowney-change"] {
        if let Ok(e) = Event::new(name) {
            _ = root.dispatch_event(&e);
        }
    }
}

fn sel_pos(root: &Element) -> Option<SelPos> {
    let sel = document().get_selection().ok()??;
    let anchor_node = sel.anchor_node()?;
    let focus_node = sel.focus_node()?;
    let mut anchor = None;
    let mut focus = None;
    for (i, line) in all_lines(root).iter().enumerate() {
        if anchor.is_none() {
            anchor = dom::text_offset_in(line, &anchor_node, sel.anchor_offset()).map(|o| (i, o));
        }
        if focus.is_none() {
            focus = dom::text_offset_in(line, &focus_node, sel.focus_offset()).map(|o| (i, o));
        }
        if anchor.is_some() && focus.is_some() {
            break;
        }
    }
    return Some(SelPos {
        anchor: anchor?,
        focus: focus?,
    });
}

impl EditorState {
    fn flush(&self, root: &Element) {
        if let Some(observer) = root_observer(root) {
            queue_mutations(&mut self.pending.borrow_mut(), observer.take_records());
        }
        process_pending(self);
    }

    fn discard_pending(&self, root: &Element) {
        if let Some(observer) = root_observer(root) {
            observer.take_records();
        }
        let mut pending = self.pending.borrow_mut();
        pending.timeout = None;
        pending.lines.clear();
    }

    fn begin(&self, root: &Element) {
        self.history.borrow_mut().begin(sel_pos(root));
    }

    fn undo_redo(&self, root: &Element, undo: bool) -> bool {
        self.flush(root);
        let restore_to = {
            let mut history = self.history.borrow_mut();
            if undo {
                history.undo()
            } else {
                history.redo()
            }
        };
        let Some(restore_to) = restore_to else {
            return false;
        };
        let focused = document().active_element().as_ref().map_or(false, |e| root.contains(Some(e)));
        load_lines(root, &restore_to.lines);
        self.discard_pending(root);
        if focused {
            restore_sel(root, restore_to.sel);
        }
        text_changed(root);
        return true;
    }

    fn can_undo(&self, root: &Element) -> bool {
        self.flush(root);
        return self.history.borrow().can_undo();
    }

    fn can_redo(&self, root: &Element) -> bool {
        self.flush(root);
        return self.history.borrow().can_redo();
    }

    /// Replaces the selection with `text` (as one undo step) and places the caret after it.
    /// If the selection isn't in the editor, appends at the end.
    fn insert_text(&self, root: &Element, text: &str) {
        self.flush(root);
        let lines = all_lines(root);
        let bounds = selection_line_bounds(root, &lines);
        let had_selection = bounds.is_some();
        let ((start_line, start), (end_line, end)) = match bounds {
            Some(bounds) => bounds,
            None => {
                let Some(last) = lines.len().checked_sub(1) else {
                    return;
                };
                let end = (last, dom::node_text_len(&lines[last]));
                (end, end)
            },
        };
        self.begin(root);
        let line_text = |i: usize| {
            let mut out = String::new();
            recurse_get_text(&lines[i], &mut out);
            out
        };
        let head = {
            let t = line_text(start_line);
            t[.. dom::utf16_to_byte(&t, start)].to_string()
        };
        let tail = {
            let t = line_text(end_line);
            t[dom::utf16_to_byte(&t, end) ..].to_string()
        };
        let mut new_lines = textformat::import_lines(text);
        new_lines[0].insert_str(0, &head);
        let caret_offset = dom::utf16_len(new_lines.last().unwrap());
        new_lines.last_mut().unwrap().push_str(&tail);

        // Replace lines start..=end with the new lines
        for line in &lines[start_line + 1 ..= end_line] {
            if let Some(parent) = line.parent_node() {
                _ = parent.remove_child(line);
            }
        }
        let first = &lines[start_line];
        first.set_text_content(Some(&new_lines[0]));
        let mut last = first.clone();
        if let Some(parent) = first.parent_node() {
            let mut place_before = first.next_sibling();
            for new_line in &new_lines[1 ..] {
                let el = document().create_element("p").unwrap();
                _ = el.class_list().add_2(CLASS_NAMESPACE, CLASS_LINE);
                el.set_text_content(Some(new_line));
                _ = parent.insert_before(&el, place_before.as_ref());
                place_before = el.next_sibling();
                last = el.into();
            }
        }
        if had_selection {
            // (Selecting would focus the editor, so don't if the selection wasn't here)
            let (caret_node, caret_node_offset) = dom::locate_text_offset(&last, caret_offset);
            dom::select(&caret_node, caret_node_offset as usize);
        }
        self.flush(root);
    }

    fn get_text(&self, root: &Element) -> String {
        self.flush(root);
        return textformat::export_lines(&all_lines(root).iter().map(export_line).collect::<Vec<_>>());
    }

    fn set_text(&self, root: &Element, markdown_text: &str) {
        self.flush(root);
        load_text(root, markdown_text);
        self.discard_pending(root);
        self.history.borrow_mut().reset(raw_lines(root), sel_pos(root));
        text_changed(root);
    }
}

/// Selection start and end as (line index, utf16 offset in line text), ordered.
fn selection_line_bounds(root: &Element, lines: &[Node]) -> Option<((usize, usize), (usize, usize))> {
    let sel = document().get_selection().ok()??;
    if sel.range_count() == 0 {
        return None;
    }
    let range = sel.get_range_at(0).ok()?;
    let locate = |container: Node, offset: u32| -> Option<(usize, usize)> {
        if !root.contains(Some(&container)) {
            return None;
        }
        for (i, line) in lines.iter().enumerate() {
            if let Some(o) = dom::text_offset_in(line, &container, offset) {
                return Some((i, o));
            }
        }

        // Not within a line - between lines/blocks (e.g. the root itself); use the next line
        let at = if container.node_type() == Node::TEXT_NODE {
            Some(container)
        } else {
            container.child_nodes().item(offset)
        };
        let Some(at) = at else {
            // At the very end
            let i = lines.len().checked_sub(1)?;
            return Some((i, dom::node_text_len(&lines[i])));
        };
        let line = line_scan(&at, ScanDirection::Forward, ScanInclusivity::Inclusive)?;
        let i = lines.iter().position(|l| l == &line)?;
        return Some((i, 0));
    };
    let start = locate(range.start_container().ok()?, range.start_offset().ok()?)?;
    let end = locate(range.end_container().ok()?, range.end_offset().ok()?)?;
    return Some((start, end.max(start)));
}

fn flush(root: &Element) {
    if let Some(state) = lookup(root) {
        state.flush(root);
    }
}

fn html_document() -> Option<HtmlDocument> {
    return document().dyn_into::<HtmlDocument>().ok();
}

fn event_root(event: &Event) -> Option<Element> {
    return event.current_target()?.dyn_into::<Element>().ok();
}

fn handle_copy(event: &Event, cut: bool) {
    let Some(root) = event_root(event) else {
        return;
    };
    let Some(data) = event.dyn_ref::<ClipboardEvent>().and_then(|e| e.clipboard_data()) else {
        return;
    };
    let Ok(Some(sel)) = document().get_selection() else {
        return;
    };
    if sel.is_collapsed() || sel.range_count() == 0 {
        return;
    }
    let Ok(range) = sel.get_range_at(0) else {
        return;
    };
    flush(&root);
    let mut lines = vec![];
    for line in all_lines(&root) {
        if !range.intersects_node(&line).unwrap_or(false) {
            continue;
        }
        let mut export = export_line(&line);
        let len = dom::utf16_len(&export.text);
        let start =
            range
                .start_container()
                .ok()
                .and_then(|n| dom::text_offset_in(&line, &n, range.start_offset().unwrap_or(0)))
                .unwrap_or(0);
        let end =
            range
                .end_container()
                .ok()
                .and_then(|n| dom::text_offset_in(&line, &n, range.end_offset().unwrap_or(0)))
                .unwrap_or(len);
        let start_byte = dom::utf16_to_byte(&export.text, start);
        let end_byte = dom::utf16_to_byte(&export.text, end).max(start_byte);
        export.text = export.text[start_byte .. end_byte].to_string();
        lines.push(export);
    }
    if lines.is_empty() {
        return;
    }
    if data.set_data("text/plain", &textformat::export_lines(&lines)).is_err() {
        return;
    }
    event.prevent_default();
    if cut {
        if let Some(state) = lookup(&root) {
            state.begin(&root);
        }
        if !html_document().and_then(|d| d.exec_command("delete").ok()).unwrap_or(false) {
            _ = sel.delete_from_document();
        }
        flush(&root);
    }
}

fn is_text(n: &Node) -> bool {
    return n.node_type() == Node::TEXT_NODE;
}

fn is_line_or_root(n: &Node) -> bool {
    if let Some(n) = n.dyn_ref::<Element>() {
        let classes = n.class_list();
        if classes.contains(CLASS_LINE) || classes.contains(CLASS_ROOT) {
            return true;
        }
    }
    return false;
}

fn create_root(markdown_text: &str, editable: bool) -> Element {
    // Create initial tree
    let root = document().create_element("div").unwrap();
    root.class_list().add_2(NAMESPACE, CLASS_ROOT).unwrap();
    if editable {
        root.set_attribute("contenteditable", "true").unwrap();
        root.set_attribute("role", "textbox").unwrap();
        root.set_attribute("aria-multiline", "true").unwrap();
    }
    load_text(&root, markdown_text);
    return root;
}

/// Keeps continuation lines aligned with their block prefixes as the layout changes
/// (width, font loading). Used by both editable and read-only roots.
fn observe_layout(root: &Element, owner: Option<&El>) {
    match owner {
        Some(owner) => {
            owner.ref_own(|_| {
                let last_width = Cell::new(-1.);
                let resize_observer = rooting::ResizeObserver::new(move |entries: Array| {
                    for entry in entries.iter() {
                        let entry = entry.unchecked_into::<ResizeObserverEntry>();
                        let width = entry.content_rect().width();
                        if last_width.replace(width) == width {
                            continue;
                        }
                        let root = entry.target();
                        _ =
                            window().request_animation_frame(
                                Closure::once_into_js(move || realign(&root)).unchecked_ref(),
                            );
                    }
                });
                let handle = resize_observer.observe(root);
                return (handle, resize_observer);
            });
        },
        None => {
            let resize_observer = ResizeObserver::new(Closure::<dyn Fn(Array)>::new(|entries: Array| {
                for entry in entries.iter() {
                    let entry = entry.unchecked_into::<ResizeObserverEntry>();
                    let root = entry.target();
                    let width = JsValue::from_f64(entry.content_rect().width());
                    let width_key = JsValue::from_str("width");
                    let Ok(state) = Reflect::get(&root, &JsValue::from_str(PROP_STATE)) else {
                        continue;
                    };
                    if Reflect::get(&state, &width_key).ok().as_ref() == Some(&width) {
                        continue;
                    }
                    _ = Reflect::set(&state, &width_key, &width);
                    _ =
                        window().request_animation_frame(
                            Closure::once_into_js(move || realign(&root)).unchecked_ref(),
                        );
                }
            }).into_js_value().unchecked_ref()).unwrap();
            resize_observer.observe(root);
            let state = Reflect::get(root, &JsValue::from_str(PROP_STATE)).unwrap_or(JsValue::UNDEFINED);
            let state = if state.is_object() {
                state
            } else {
                let state = Object::new().into();
                _ = Reflect::set(root, &JsValue::from_str(PROP_STATE), &state);
                state
            };
            _ = Reflect::set(&state, &JsValue::from_str("resizeObserver"), &resize_observer);
        },
    }
    if !FONT_LISTENER.with(|x| x.replace(true)) {
        let cb = Closure::<dyn Fn()>::new(|| {
            let roots = document().get_elements_by_class_name(CLASS_ROOT);
            for i in 0 .. roots.length() {
                if let Some(root) = roots.item(i) {
                    realign(&root);
                }
            }
        });
        _ = document().fonts().add_event_listener_with_callback("loadingdone", cb.as_ref().unchecked_ref());
        cb.forget();
    }
}

fn attach(root: &Element, owner: Option<&El>, options: Options) -> Rc<EditorState> {
    console_error_panic_hook::set_once();
    let root = root.clone();
    let state = Object::new();
    _ = Reflect::set(&root, &JsValue::from_str(PROP_STATE), &state);
    let id = NEXT_ID.with(|x| {
        let out = x.get();
        x.set(out + 1);
        return out;
    });
    let editor_state = Rc::new(EditorState {
        id: id,
        newline_on_enter: options.newline_on_enter,
        pending: RefCell::new(PendingState {
            timeout: None,
            lines: vec![],
        }),
        history: RefCell::new(History::new(raw_lines(&root), None)),
    });
    EDITORS.with(|e| e.borrow_mut().insert(id, Rc::downgrade(&editor_state)));
    _ = root.set_attribute(ATTR_EDITOR_ID, &id.to_string());
    let on_mutations = Closure::<dyn Fn(Array)>::new({
        let editor_state = editor_state.clone();
        move |mutations: Array| {
            let mut pending = editor_state.pending.borrow_mut();
            queue_mutations(&mut pending, mutations);
            if pending.lines.is_empty() {
                return;
            }
            pending.timeout = Some(Timeout::new(200, {
                let editor_state = editor_state.clone();
                move || process_pending(&editor_state)
            }));
        }
    });
    let on_mutations = match owner {
        Some(owner) => {
            let js = on_mutations.as_ref().clone();
            owner.ref_own(|_| on_mutations);
            js
        },
        None => on_mutations.into_js_value(),
    };
    let observer = MutationObserver::new(on_mutations.unchecked_ref()).unwrap();
    observer.observe_with_options(&root, &{
        let o = MutationObserverInit::new();
        o.set_child_list(true);
        o.set_character_data(true);
        o.set_subtree(true);
        o
    }).unwrap();
    _ = Reflect::set(&state, &JsValue::from_str("mutationObserver"), &observer);
    let listen = |type_: &'static str, f: fn(&Event)| {
        match owner {
            Some(owner) => {
                owner.ref_on(type_, move |e| f(e));
            },
            None => {
                let cb = Closure::<dyn Fn(Event)>::new(move |e: Event| f(&e)).into_js_value();
                _ = root.add_event_listener_with_callback(type_, cb.unchecked_ref());
            },
        }
    };
    listen("keydown", |event| {
        let Some(root) = event_root(event) else {
            return;
        };
        let Some(event) = event.dyn_ref::<KeyboardEvent>() else {
            return;
        };
        if event.default_prevented() {
            return;
        }
        if (event.ctrl_key() || event.meta_key()) && !event.alt_key() {
            let undo = match (event.key().to_lowercase().as_str(), event.shift_key()) {
                ("z", false) => Some(true),
                ("z", true) | ("y", false) => Some(false),
                _ => None,
            };
            if let Some(undo) = undo {
                event.prevent_default();
                if let Some(state) = lookup(&root) {
                    state.undo_redo(&root, undo);
                }
                return;
            }
        }
        let Ok(Some(sel)) = document().get_selection() else {
            return;
        };
        let Some(node) = sel.anchor_node() else {
            return;
        };
        let direction;
        let vertical;
        match event.key().as_str() {
            "ArrowLeft" | "Backspace" => {
                vertical = false;
                direction = ScanDirection::Backward;
            },
            "ArrowRight" | "Delete" => {
                vertical = false;
                direction = ScanDirection::Forward;
            },
            "ArrowUp" => {
                vertical = true;
                direction = ScanDirection::Backward;
            },
            "ArrowDown" => {
                vertical = true;
                direction = ScanDirection::Forward;
            },
            _ => {
                return;
            },
        }
        if event.shift_key() || event.alt_key() || event.ctrl_key() || event.meta_key() {
            return;
        }
        match vertical {
            false => {
                // This doubles up on event handling - this adjusts the starting position, then
                // the normal handler does an additional move. This clears the (extra) gap plus
                // the intended 1 movement.
                if !sel.is_collapsed() {
                    return;
                }
                if direction == ScanDirection::Backward && sel.anchor_offset() == 0 {
                    // Allow backspace at root of node, rather than last character of previous line
                    if is_line(&node) {
                        return;
                    }
                    let Some(parent) = node.parent_element() else {
                        return;
                    };
                    if is_line(&parent) && parent.first_child().as_ref() == Some(&node) {
                        return;
                    }
                    let Some(prev) =
                        dom::scan(
                            &node,
                            is_line_or_root,
                            is_text,
                            ScanDirection::Backward,
                            ScanInclusivity::Exclusive,
                        ) else {
                            return;
                        };
                    dom::select(&prev, dom::node_text_len(&prev));
                } else if direction == ScanDirection::Forward && is_text(&node) &&
                    sel.anchor_offset() as usize == dom::node_text_len(&node) {
                    let Some(next) =
                        dom::scan(
                            &node,
                            is_line_or_root,
                            is_text,
                            ScanDirection::Forward,
                            ScanInclusivity::Exclusive,
                        ) else {
                            return;
                        };
                    dom::select(&next, 0);
                }
            },
            true => {
                let Ok(range) = sel.get_range_at(0) else {
                    return;
                };
                let current_sel_box = range.get_bounding_client_rect();
                if current_sel_box.height() <= 0. {
                    // Multiline node, cursor not in top line
                    return;
                }
                let mut line = node;
                while !is_line(&line) {
                    if line.dyn_ref::<Element>().map_or(false, |e| e.class_list().contains(CLASS_ROOT)) {
                        // Multiline node, cursor not in bottom line
                        return;
                    }
                    let Some(parent) = line.parent_node() else {
                        return;
                    };
                    line = parent;
                }
                let Some(current_line_box) = dom::bounds(&line) else {
                    return;
                };
                let dest_line = match direction {
                    ScanDirection::Backward => {
                        if -(current_line_box.top() - current_sel_box.top()) / current_sel_box.height() > 0.4 {
                            return;
                        }
                        let Some(prev) = line_scan(&line, ScanDirection::Backward, ScanInclusivity::Exclusive) else {
                            return;
                        };
                        prev
                    },
                    ScanDirection::Forward => {
                        if (current_line_box.bottom() - current_sel_box.bottom()) / current_sel_box.height() > 0.4 {
                            return;
                        }
                        let Some(next) = line_scan(&line, ScanDirection::Forward, ScanInclusivity::Exclusive) else {
                            return;
                        };
                        next
                    },
                };
                if dom::node_text_len(&dest_line) == 0 {
                    // Empty line; caretPositionFromPoint resolves to neighboring text instead
                    dom::select(&dest_line, 0);
                    event.prevent_default();
                    return;
                }
                let x = current_sel_box.x();
                let Some(dest_box) = dom::bounds(&dest_line) else {
                    return;
                };
                let y = match direction {
                    ScanDirection::Backward => dest_box.bottom() - 5.,
                    ScanDirection::Forward => dest_box.top() + 5.,
                };
                let Some(caret) = document().caret_position_from_point(x as f32, y as f32) else {
                    return;
                };
                let Some(caret_node) = caret.offset_node() else {
                    return;
                };
                if let Some(mut caret_text_node) =
                    dom::scan(&caret_node, is_root, is_text, direction, ScanInclusivity::Inclusive) {
                    let mut offset = 0;
                    loop {
                        let len = dom::node_text_len(&caret_text_node);
                        if offset + len >= caret.offset() as usize {
                            dom::select(&caret_text_node, caret.offset() as usize - offset);
                            event.prevent_default();
                            break;
                        }
                        offset += len;
                        let Some(at_temp) =
                            dom::scan(
                                &caret_text_node,
                                is_root,
                                is_text,
                                ScanDirection::Forward,
                                ScanInclusivity::Exclusive,
                            ) else {
                                break;
                            };
                        caret_text_node = at_temp;
                    }
                } else {
                    dom::select(&caret_node, 0);
                    event.prevent_default();
                }
            },
        }
    });
    listen("copy", |e| handle_copy(e, false));
    listen("cut", |e| handle_copy(e, true));
    listen("paste", |event| {
        let Some(root) = event_root(event) else {
            return;
        };
        let Some(data) = event.dyn_ref::<ClipboardEvent>().and_then(|e| e.clipboard_data()) else {
            return;
        };
        // Only plain text is accepted; anything else (html, images) is ignored
        event.prevent_default();
        let Ok(text) = data.get_data("text/plain") else {
            return;
        };
        if text.is_empty() {
            return;
        }
        let Some(state) = lookup(&root) else {
            return;
        };
        state.insert_text(&root, &text);
    });
    listen("drop", |event| {
        let Some(root) = event_root(event) else {
            return;
        };
        let Some(drag) = event.dyn_ref::<DragEvent>() else {
            return;
        };
        let Some(data) = drag.data_transfer() else {
            return;
        };
        event.prevent_default();
        let Ok(text) = data.get_data("text/plain") else {
            return;
        };
        if text.is_empty() {
            return;
        }
        let Some(state) = lookup(&root) else {
            return;
        };
        if let Some(caret) = document().caret_position_from_point(drag.client_x() as f32, drag.client_y() as f32) {
            if let Some(node) = caret.offset_node() {
                if root.contains(Some(&node)) {
                    dom::select(&node, caret.offset() as usize);
                }
            }
        }
        state.insert_text(&root, &text);
    });
    listen("beforeinput", |event| {
        let Some(root) = event_root(event) else {
            return;
        };
        let Some(input) = event.dyn_ref::<InputEvent>() else {
            return;
        };
        let Some(state) = lookup(&root) else {
            return;
        };
        let undo = match input.input_type().as_str() {
            "historyUndo" => true,
            "historyRedo" => false,
            // Only Enter itself; Shift+Enter (`insertLineBreak`) still inserts a newline
            "insertParagraph" if !state.newline_on_enter => {
                event.prevent_default();
                return;
            },
            _ => {
                state.begin(&root);
                return;
            },
        };
        if event.default_prevented() {
            return;
        }
        event.prevent_default();
        state.undo_redo(&root, undo);
    });
    if let Some(owner) = owner {
        owner.ref_own(|_| defer({
            let root = root.clone();
            move || {
                observer.disconnect();
                _ = root.remove_attribute(ATTR_EDITOR_ID);
                _ = Reflect::delete_property(&root, &JsValue::from_str(PROP_STATE));
            }
        }));
    }
    observe_layout(&root, owner);
    return editor_state;
}

/// Options for `build`.
pub struct Options {
    /// When false, Enter doesn't insert a newline (Shift+Enter still does), so Enter
    /// can be handled with a normal `keydown` listener, e.g. to submit.
    pub newline_on_enter: bool,
}

impl Default for Options {
    fn default() -> Self {
        return Options { newline_on_enter: true };
    }
}

pub fn build(markdown_text: &str, options: Options) -> (El, Editor) {
    let root = create_root(markdown_text, true);
    let el = el_from_raw(root.clone());
    let state = attach(&root, Some(&el), options);
    return (el, Editor {
        root: root,
        state: state,
    });
}

/// Renders the markdown as a non-editable block: no event listeners, no editor state.
pub fn build_readonly(markdown_text: &str) -> El {
    let root = create_root(markdown_text, false);
    let el = el_from_raw(root.clone());
    observe_layout(&root, Some(&el));
    return el;
}

pub struct Editor {
    root: Element,
    state: Rc<EditorState>,
}

impl Editor {
    pub fn get_text(&self) -> String {
        return self.state.get_text(&self.root);
    }

    pub fn set_text(&self, markdown_text: &str) {
        self.state.set_text(&self.root, markdown_text);
    }

    pub fn undo(&self) -> bool {
        return self.state.undo_redo(&self.root, true);
    }

    pub fn redo(&self) -> bool {
        return self.state.undo_redo(&self.root, false);
    }

    pub fn can_undo(&self) -> bool {
        return self.state.can_undo(&self.root);
    }

    pub fn can_redo(&self) -> bool {
        return self.state.can_redo(&self.root);
    }

    /// Replaces the selection with `text` (one undo step), or appends it if the selection
    /// isn't in the editor. Doesn't change focus.
    pub fn insert_text(&self, text: &str) {
        self.state.insert_text(&self.root, text);
    }
}

#[wasm_bindgen(typescript_custom_section)]
const TS_OPTIONS: &str = r#"
export interface Options {
    /** Default true. When false, Enter doesn't insert a newline (Shift+Enter still does). */
    newlineOnEnter?: boolean;
}
"#;

#[wasm_bindgen(js_name = "build")]
pub fn build_js(
    markdown_text: &str,
    #[wasm_bindgen(unchecked_optional_param_type = "Options")]
    options: Option<Object>,
) -> Element {
    let mut parsed = Options::default();
    if let Some(options) = options {
        if let Some(v) = Reflect::get(&options, &JsValue::from_str("newlineOnEnter")).ok().and_then(|v| v.as_bool()) {
            parsed.newline_on_enter = v;
        }
    }
    let root = create_root(markdown_text, true);
    attach(&root, None, parsed);
    return root;
}

#[wasm_bindgen(js_name = "buildReadonly")]
pub fn build_readonly_js(markdown_text: &str) -> Element {
    let root = create_root(markdown_text, false);
    observe_layout(&root, None);
    return root;
}

#[wasm_bindgen(js_name = "getText")]
pub fn get_text_js(root: &Element) -> String {
    return lookup(root).map(|s| s.get_text(root)).unwrap_or_default();
}

#[wasm_bindgen(js_name = "setText")]
pub fn set_text_js(root: &Element, markdown_text: &str) {
    if let Some(state) = lookup(root) {
        state.set_text(root, markdown_text);
    }
}

#[wasm_bindgen(js_name = "undo")]
pub fn undo_js(root: &Element) -> bool {
    return lookup(root).map_or(false, |s| s.undo_redo(root, true));
}

#[wasm_bindgen(js_name = "redo")]
pub fn redo_js(root: &Element) -> bool {
    return lookup(root).map_or(false, |s| s.undo_redo(root, false));
}

#[wasm_bindgen(js_name = "canUndo")]
pub fn can_undo_js(root: &Element) -> bool {
    return lookup(root).map_or(false, |s| s.can_undo(root));
}

#[wasm_bindgen(js_name = "canRedo")]
pub fn can_redo_js(root: &Element) -> bool {
    return lookup(root).map_or(false, |s| s.can_redo(root));
}

#[wasm_bindgen(js_name = "insertText")]
pub fn insert_text_js(root: &Element, text: &str) {
    if let Some(state) = lookup(root) {
        state.insert_text(root, text);
    }
}

#[cfg(feature = "browser_tests")]
mod browser_tests {
    use {
        super::{
            build,
            Editor,
            EDITORS,
        },
        rooting::El,
        std::cell::RefCell,
        wasm_bindgen::prelude::wasm_bindgen,
        web_sys::Element,
    };

    thread_local!{
        static ROOTED: RefCell<Vec<(El, Editor)>> = RefCell::new(vec![]);
    }

    #[wasm_bindgen(js_name = "testRootedBuild")]
    pub fn test_rooted_build(markdown_text: &str) -> Element {
        let (el, editor) = build(markdown_text, Default::default());
        let raw = el.raw();
        ROOTED.with(|r| r.borrow_mut().push((el, editor)));
        return raw;
    }

    #[wasm_bindgen(js_name = "testRootedHandleUndo")]
    pub fn test_rooted_handle_undo() -> bool {
        return ROOTED.with(|r| r.borrow().last().map_or(false, |(_, editor)| editor.undo()));
    }

    #[wasm_bindgen(js_name = "testRootedDropAll")]
    pub fn test_rooted_drop_all() {
        ROOTED.with(|r| r.borrow_mut().clear());
    }

    #[wasm_bindgen(js_name = "testLiveEditorCount")]
    pub fn test_live_editor_count() -> usize {
        return EDITORS.with(|e| e.borrow().values().filter(|w| w.upgrade().is_some()).count());
    }
}
