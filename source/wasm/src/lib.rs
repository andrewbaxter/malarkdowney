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
    structre::structre,
    textformat::{
        ExportBlock,
        ExportLine,
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
        ClipboardEvent,
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

fn generate_aligned(out_want_children: &mut Vec<ShadowDom>, indents: &mut [LineIndent]) {
    for indent in indents {
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

#[structre("^(?<number>[0-9]+\\. )(?<suffix>.*)$")]
struct ReOlNumberPrefix {
    number: String,
    suffix: String,
}

#[structre("^(?<level>#{1,6}) ")]
struct ReHeadingPrefix {
    level: String,
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

fn update_lines_starting_at(mut line: Node) -> Option<Element> {
    let mut context_line = line_scan(&line, ScanDirection::Backward, ScanInclusivity::Exclusive);

    // Locate root
    let root;
    {
        let Some(mut parent_at) = line.parent_element() else {
            return None;
        };
        while !parent_at.class_list().contains(CLASS_ROOT) {
            let Some(temp) = parent_at.parent_element() else {
                return None;
            };
            parent_at = temp;
        }
        root = parent_at;
    }

    // Start walking
    loop {
        let mut changed = false;
        let mut indents = vec![];
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
        let mut place_parent = line.parent_element().unwrap();
        while place_parent != root {
            place_before = place_parent.next_sibling();
            place_parent = place_parent.parent_element().unwrap();
        }

        // If there's a context line
        let mut in_blockcode = false;
        let mut blockcode_is_nonfirst = false;
        if let Some(context_line) = context_line {
            // Determine context by walking previous line(/context)'s block parents up to root
            let mut root_context_path = vec![];
            let mut at_parent = context_line.parent_element().unwrap();
            let mut next_child = context_line.next_sibling();
            while at_parent != root {
                let id = at_parent.get_attribute(ATTR_ID).unwrap().parse::<usize>().unwrap();
                root_context_path.push((at_parent.clone(), next_child, LineIndent {
                    type_: BlockType::from_str(&at_parent.get_attribute(ATTR_BLOCK_TYPE).unwrap()).unwrap(),
                    source_indent: id,
                    first_text: None,
                    text: "".to_string(),
                }));
                next_child = at_parent.next_sibling();
                at_parent = at_parent.parent_element().unwrap();
            }
            root_context_path.reverse();
            let context_children = context_line.child_nodes();
            for (i, (_, _, block_indent)) in root_context_path.iter_mut().enumerate() {
                let Some(aligned) = context_children.item(i as u32) else {
                    continue;
                };
                block_indent.text = " ".repeat(aligned.text_content().unwrap().len());
            }

            // Find higest indent of previous/context this should be placed under by matching
            // indents to line prefixes
            for (parent, parent_next, mut indent) in root_context_path {
                match &mut indent.type_ {
                    BlockType::Quote => {
                        text = if text.is_empty() {
                            text
                        } else if let Some(suffix1) = text.strip_prefix(PREFIX_BLOCKQUOTE) {
                            suffix1.to_string()
                        } else {
                            break;
                        };
                        indent.text = PREFIX_BLOCKQUOTE.to_string();
                    },
                    BlockType::Ul => {
                        text = if text.is_empty() {
                            text
                        } else if let Some(suffix1) = text.strip_prefix(PREFIX_UL_FIRST) {
                            indent.first_text = Some(PREFIX_UL_FIRST.to_string());
                            indent.text = PREFIX_UL.to_string();
                            suffix1.to_string()
                        } else if let Some(suffix1) = text.strip_prefix(PREFIX_UL_FIRST2) {
                            indent.first_text = Some(PREFIX_UL_FIRST2.to_string());
                            indent.text = PREFIX_UL.to_string();
                            suffix1.to_string()
                        } else if let Some(suffix1) = text.strip_prefix(&indent.text) {
                            suffix1.to_string()
                        } else {
                            break;
                        };
                    },
                    BlockType::Ol => {
                        text = if text.is_empty() {
                            text
                        } else if let Ok(parsed) = ReOlNumberPrefix::from_str(&text) {
                            indent.text = " ".repeat(parsed.number.len());
                            indent.first_text = Some(parsed.number);
                            parsed.suffix
                        } else if let Some(suffix1) = text.strip_prefix(&indent.text) {
                            suffix1.to_string()
                        } else {
                            break;
                        };
                    },
                    BlockType::Code => {
                        let mut context_codeblock_end = false;
                        shed!{
                            let Some(context_line) = context_line.dyn_ref::<Element>() else {
                                break;
                            };
                            if context_line.has_attribute(ATTR_BLOCKCODE_END) {
                                context_codeblock_end = true;
                            }
                        };
                        if !context_codeblock_end {
                            in_blockcode = true;
                            blockcode_is_nonfirst = true;
                        } else {
                            break;
                        }
                    },
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
            let mut at_parent = line.parent_element().unwrap();
            let mut next_child = line.next_sibling();
            loop {
                if at_parent == place_parent {
                    break 'line_under_place_parent;
                }
                if at_parent == root {
                    break;
                }
                let id = at_parent.get_attribute(ATTR_ID).unwrap().parse::<usize>().unwrap();
                context_line_path.push((at_parent.clone(), next_child, LineIndent {
                    type_: BlockType::from_str(&at_parent.get_attribute(ATTR_BLOCK_TYPE).unwrap(),).unwrap(),
                    source_indent: id,
                    first_text: None,
                    text: "".to_string(),
                }));
                next_child = at_parent.next_sibling();
                at_parent = at_parent.parent_element().unwrap();
            }
        } 'line_under_place_parent {
            context_line_path.reverse();

            // Could be splitting a parent container; get list of following lines to drag
            // along to placement root (and the split parents will be recreated later)
            shed!{
                let mut lift_at = line.clone();
                if lift_at.parent_element().unwrap() == place_parent {
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
                    lift_at = lift_at.parent_node().unwrap();
                    if lift_at.parent_element().unwrap() == place_parent {
                        // Reached root, stop
                        place_before = lift_at.next_sibling();
                        break;
                    }
                }
            }

            // Move placement parent down current line's tree by matching prefixes
            for (parent, parent_next, mut indent) in context_line_path {
                match &mut indent.type_ {
                    BlockType::Quote => {
                        let Some(suffix1) = text.strip_prefix(PREFIX_BLOCKQUOTE) else {
                            break;
                        };
                        text = suffix1.to_string();
                        indent.text = PREFIX_BLOCKQUOTE.to_string();
                    },
                    BlockType::Ul => {
                        if let Some(suffix1) = text.strip_prefix(PREFIX_UL_FIRST) {
                            text = suffix1.to_string();
                            indent.first_text = Some(PREFIX_UL_FIRST.to_string());
                            indent.text = PREFIX_UL.to_string();
                        } else if let Some(suffix1) = text.strip_prefix(PREFIX_UL_FIRST2) {
                            text = suffix1.to_string();
                            indent.first_text = Some(PREFIX_UL_FIRST2.to_string());
                            indent.text = PREFIX_UL.to_string();
                        } else {
                            break;
                        }
                    },
                    BlockType::Ol => {
                        if let Ok(parsed) = ReOlNumberPrefix::from_str(&text) {
                            text = parsed.suffix;
                            indent.text = " ".repeat(parsed.number.len());
                            indent.first_text = Some(parsed.number);
                        } else {
                            break;
                        }
                    },
                    BlockType::Code => {
                        if text.starts_with(PREFIX_BLOCKCODE) {
                            // This must be the first line, otherwise it would be part of the context block
                            in_blockcode = true;
                        } else {
                            break;
                        }
                    },
                }
                place_parent = parent;
                place_before = parent_next;
                indents.push(indent);
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
            } else if let Ok(parsed) = ReOlNumberPrefix::from_str(&text) {
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
        let original_parent = line.parent_element().unwrap();
        if place_parent != original_parent || !create_parents.is_empty() {
            changed = true;

            // Lift line + any following elements between the line and the placement root
            line.parent_node().unwrap().remove_child(&line).unwrap();
            for e in &lift_following {
                e.parent_node().unwrap().remove_child(&e).unwrap();
            }

            // Create blocks and place this line
            {
                let mut place_parent = place_parent.clone();
                let mut place_before = place_before.clone();
                for (new_parent, indent) in create_parents.into_iter() {
                    place_parent.insert_before(&new_parent, place_before.as_ref()).unwrap();
                    place_parent = new_parent;
                    place_before = None;
                    indents.push(indent);
                }
                place_parent.insert_before(&line, place_before.as_ref()).unwrap();
            }

            // Place remaining lines
            for e in lift_following {
                place_parent.insert_before(&e, place_before.as_ref()).unwrap();
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
                clean_up_at = clean_up_at.parent_element().unwrap();
            }
            if let Some(clean_up_until) = clean_up_until {
                clean_up_until.remove();
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
            let heading_level = ReHeadingPrefix::from_str(&text).ok().map(|parsed| parsed.level.len());

            // Parse inline
            let inline = linemarkdown::parse(&text);
            {
                let mut want_children = vec![];
                generate_aligned(&mut want_children, &mut indents);

                fn generate_inline(want: Inline) -> ShadowDom {
                    match want {
                        Inline::Text(want) => {
                            return ShadowDom::Text(want);
                        },
                        Inline::Strong(want) => {
                            return ShadowDom::Element(ShadowDomElement {
                                tag: "strong",
                                classes: {
                                    let mut out = HashSet::new();
                                    out.insert(CLASS_NAMESPACE.to_string());
                                    if want.end_delim.is_none() {
                                        out.insert(CLASS_INCOMPLETE.to_string());
                                    }
                                    out
                                },
                                attrs: Default::default(),
                                children: {
                                    let mut out = vec![];
                                    out.push(ShadowDom::Text(want.begin_delim));
                                    for c in want.children {
                                        out.push(generate_inline(c));
                                    }
                                    if let Some(end_delim) = want.end_delim {
                                        out.push(ShadowDom::Text(end_delim));
                                    }
                                    out
                                },
                            });
                        },
                        Inline::Emphasis(want) => {
                            return ShadowDom::Element(ShadowDomElement {
                                tag: "em",
                                classes: {
                                    let mut out = HashSet::new();
                                    out.insert(CLASS_NAMESPACE.to_string());
                                    if want.end_delim.is_none() {
                                        out.insert(CLASS_INCOMPLETE.to_string());
                                    }
                                    out
                                },
                                attrs: Default::default(),
                                children: {
                                    let mut out = vec![];
                                    out.push(ShadowDom::Text(want.begin_delim));
                                    for c in want.children {
                                        out.push(generate_inline(c));
                                    }
                                    if let Some(end_delim) = want.end_delim {
                                        out.push(ShadowDom::Text(end_delim));
                                    }
                                    out
                                },
                            });
                        },
                        Inline::Strikethrough(want) => {
                            return ShadowDom::Element(ShadowDomElement {
                                tag: "s",
                                classes: {
                                    let mut out = HashSet::new();
                                    out.insert(CLASS_NAMESPACE.to_string());
                                    if want.end_delim.is_none() {
                                        out.insert(CLASS_INCOMPLETE.to_string());
                                    }
                                    out
                                },
                                attrs: Default::default(),
                                children: {
                                    let mut out = vec![];
                                    out.push(ShadowDom::Text(want.begin_delim));
                                    for c in want.children {
                                        out.push(generate_inline(c));
                                    }
                                    if let Some(end_delim) = want.end_delim {
                                        out.push(ShadowDom::Text(end_delim));
                                    }
                                    out
                                },
                            });
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
                    resolve(sel_anchor, sel.anchor_node(), sel.anchor_offset()) else {
                        break;
                    };
                let Some((focus_node, focus_offset)) = resolve(sel_focus, sel.focus_node(), sel.focus_offset()) else {
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
    return Some(root);
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
        update_lines_starting_at(line.into());
    }
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
    for line in lines {
        if seen.has(&line) {
            continue;
        }
        seen.add(&line);
        if let Some(root) = update_lines_starting_at(line) {
            if !roots.contains(&root) {
                roots.push(root);
            }
        }
    }
    for root in roots {
        realign(&root);
        let recorded = state.history.borrow_mut().record(raw_lines(&root), sel_pos(&root));
        if recorded {
            dispatch_history_event(&root);
        }
    }
}

fn dispatch_history_event(root: &Element) {
    if let Ok(e) = Event::new("malarkdowney-history") {
        _ = root.dispatch_event(&e);
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
            if let Some(sel) = restore_to.sel {
                let lines = all_lines(root);
                let locate =
                    |(line, offset): (usize, usize)| lines
                        .get(line)
                        .map(|l| dom::locate_text_offset(l, offset));
                if let (Some((anchor_node, anchor_offset)), Some((focus_node, focus_offset))) =
                    (locate(sel.anchor), locate(sel.focus)) {
                    dom::select_range(&anchor_node, anchor_offset, &focus_node, focus_offset);
                }
            }
        }
        dispatch_history_event(root);
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

    fn get_text(&self, root: &Element) -> String {
        self.flush(root);
        return textformat::export_lines(&all_lines(root).iter().map(export_line).collect::<Vec<_>>());
    }

    fn set_text(&self, root: &Element, markdown_text: &str) {
        self.flush(root);
        load_text(root, markdown_text);
        self.discard_pending(root);
        self.history.borrow_mut().reset(raw_lines(root), sel_pos(root));
        dispatch_history_event(root);
    }
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

fn create_root(markdown_text: &str) -> Element {
    // Create initial tree
    let root = document().create_element("div").unwrap();
    root.class_list().add_2(NAMESPACE, CLASS_ROOT).unwrap();
    root.set_attribute("contenteditable", "true").unwrap();
    load_text(&root, markdown_text);
    return root;
}

fn attach(root: &Element, owner: Option<&El>) -> Rc<EditorState> {
    let root = root.clone();
    let state = Object::new();
    let set = |k: &str, v: &JsValue| {
        _ = Reflect::set(&state, &JsValue::from_str(k), v);
    };
    let id = NEXT_ID.with(|x| {
        let out = x.get();
        x.set(out + 1);
        return out;
    });
    let editor_state = Rc::new(EditorState {
        id: id,
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
    set("mutationObserver", &observer);
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
        let Ok(text) = data.get_data("text/plain") else {
            return;
        };
        event.prevent_default();
        let Some(doc) = html_document() else {
            return;
        };
        let Some(state) = lookup(&root) else {
            return;
        };
        state.flush(&root);
        state.begin(&root);
        for (i, line) in textformat::import_lines(&text).into_iter().enumerate() {
            if i > 0 {
                _ = doc.exec_command("insertParagraph");
            }
            if !line.is_empty() {
                _ = doc.exec_command_with_show_ui_and_value("insertText", false, &line);
            }
        }
        state.flush(&root);
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
                let handle = resize_observer.observe(&root);
                return (handle, resize_observer);
            });
            owner.ref_own(|_| defer({
                let root = root.clone();
                move || {
                    observer.disconnect();
                    _ = root.remove_attribute(ATTR_EDITOR_ID);
                    _ = Reflect::delete_property(&root, &JsValue::from_str(PROP_STATE));
                }
            }));
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
            resize_observer.observe(&root);
            set("resizeObserver", &resize_observer);
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
    _ = Reflect::set(&root, &JsValue::from_str(PROP_STATE), &state);
    return editor_state;
}

pub fn build(markdown_text: &str) -> (El, Editor) {
    let root = create_root(markdown_text);
    let el = el_from_raw(root.clone());
    let state = attach(&root, Some(&el));
    return (el, Editor {
        root: root,
        state: state,
    });
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
}

#[wasm_bindgen(js_name = "build")]
pub fn build_js(markdown_text: &str) -> Element {
    let root = create_root(markdown_text);
    attach(&root, None);
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
        let (el, editor) = build(markdown_text);
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
