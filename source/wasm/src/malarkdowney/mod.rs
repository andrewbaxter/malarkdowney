use {
    const_format::formatcp,
    dom::{
        ScanDirection,
        ScanInclusivity,
    },
    flowcontrol::{
        shed,
        superif,
    },
    gloo::{
        events::{
            EventListener,
            EventListenerOptions,
        },
        timers::callback::Timeout,
        utils::{
            document,
            window,
        },
    },
    js_sys::Array,
    linemarkdown::Inline,
    rooting::{
        el,
        el_from_raw,
        El,
    },
    serde::{
        Deserialize,
        Serialize,
    },
    shadowdom::{
        generate_shadow_dom,
        sync_shadow_dom,
        ShadowDom,
        ShadowDomElement,
    },
    std::{
        borrow::Cow,
        cell::{
            Cell,
            RefCell,
        },
        collections::{
            HashMap,
            HashSet,
        },
        rc::Rc,
        str::FromStr,
    },
    structre::structre,
    wasm_bindgen::{
        closure::Closure,
        JsCast,
        JsValue,
    },
    web_sys::{
        CharacterData,
        CssStyleDeclaration,
        Element,
        HtmlElement,
        KeyboardEvent,
        MutationObserver,
        MutationObserverInit,
        MutationRecord,
        Node,
    },
};

pub mod dom;
pub mod shadowdom;
pub mod linemarkdown;

const NAMESPACE: &str = "malarkdowney";
const ATTR_ID: &str = formatcp!("{}_id", NAMESPACE);
const ATTR_INDENT_ID: &str = formatcp!("{}_indent_id", NAMESPACE);
const ATTR_BLOCK_TYPE: &str = formatcp!("{}_block_type", NAMESPACE);
const ATTR_BLOCKCODE_BEGIN: &str = formatcp!("{}_codeblock_begin", NAMESPACE);
const ATTR_BLOCKCODE_END: &str = formatcp!("{}_codeblock_end", NAMESPACE);
const CLASS_NAMESPACE: &str = NAMESPACE;
const CLASS_ROOT: &str = formatcp!("{}_root", NAMESPACE);
const CLASS_INCOMPLETE: &str = formatcp!("{}_incomplete", NAMESPACE);
const CLASS_ALIGNED: &str = formatcp!("{}_aligned", NAMESPACE);
const CLASS_BLOCK: &str = formatcp!("{}_block", NAMESPACE);
const CLASS_LINE: &str = formatcp!("{}_line", NAMESPACE);
const CLASS_BLOCKCODE: &str = formatcp!("{}_block_code", NAMESPACE);
const CLASS_BLOCKQUOTE: &str = formatcp!("{}_block_quote", NAMESPACE);
const CLASS_UL: &str = formatcp!("{}_block_ul", NAMESPACE);
const CLASS_OL: &str = formatcp!("{}_block_ol", NAMESPACE);
const CLASS_PSEUDO_A: &str = formatcp!("{}_inline_pseudo_a", NAMESPACE);
const PREFIX_UL_FIRST: &str = "* ";
const PREFIX_UL: &str = "   ";
const PREFIX_BLOCKQUOTE: &str = "> ";
const PREFIX_BLOCKCODE: &str = "```";

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
enum BlockType {
    BlockQuote,
    Ul,
    Ol,
    BlockCode,
}

enum LineType {
    Normal,
    Heading(usize),
}

fn class_aligned(id: usize) -> String {
    return format!("{}_aligned_{}", NAMESPACE, id);
}

fn css_id_block(id: usize) -> String {
    return format!("{}_indent_{}", NAMESPACE, id);
}

pub enum Block {
    // Level starts from 1
    Heading(usize, Vec<Inline>),
    Line(Vec<Inline>),
    Quote(Vec<Block>),
    Code(Vec<String>),
    Ul(Vec<Vec<Block>>),
    Ol(Vec<Vec<Block>>),
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
            classes: [CLASS_NAMESPACE.to_string(), CLASS_ALIGNED.to_string(), class_aligned(indent.source_indent)]
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

fn generate_line_code_shadowdom(
    indents: &mut [LineIndent],
    text: String,
    delim: Option<BlockcodeDelim>,
) -> ShadowDom {
    let mut want_children = vec![];
    generate_aligned(&mut want_children, indents);
    want_children.push(ShadowDom::Text(text));
    return ShadowDom::Element(ShadowDomElement {
        tag: "codeline",
        classes: [CLASS_NAMESPACE.to_string(), CLASS_LINE.to_string()].into_iter().collect(),
        attrs: {
            let mut out = HashMap::new();
            out.insert("spellcheck".to_string(), "false".to_string());
            if let Some(delim) = delim {
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
    });
}

fn generate_line_shadowdom(line_type: LineType, indents: &mut [LineIndent], inlines: Vec<Inline>) -> ShadowDom {
    let mut want_children = vec![];
    generate_aligned(&mut want_children, indents);

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
            Inline::Link(want) => {
                return ShadowDom::Element(ShadowDomElement {
                    tag: "span",
                    classes: {
                        let mut out = HashSet::new();
                        out.insert(CLASS_NAMESPACE.to_string());
                        out.insert(CLASS_PSEUDO_A.to_string());
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
                                            format!("{}{}{}", address.begin_delim, address.address, address.end_delim),
                                        )
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

    for inline in inlines {
        want_children.push(generate_inline(inline));
    }
    return ShadowDom::Element(ShadowDomElement {
        tag: match line_type {
            LineType::Normal => "p",
            LineType::Heading(level) => match level {
                1 => "h1",
                2 => "h2",
                3 => "h3",
                4 => "h4",
                5 => "h5",
                6 => "h6",
                _ => unreachable!(),
            },
        },
        classes: [CLASS_NAMESPACE.to_string(), CLASS_LINE.to_string()].into_iter().collect(),
        attrs: Default::default(),
        children: want_children,
    });
}

fn get_aligned(id: usize) -> Vec<Element> {
    let mut out = vec![];
    let aligned = document().get_elements_by_class_name(&class_aligned(id));
    for i in 0 .. aligned.length() {
        out.push(aligned.item(i).unwrap().dyn_into::<Element>().unwrap());
    }
    return out;
}

struct CalculateIndentAlignmentRes {
    have: f64,
    want: f64,
}

fn calculate_indent_alignment(
    indent_block: &Element,
    e: &Element,
    e_style: &CssStyleDeclaration,
) -> CalculateIndentAlignmentRes {
    let have_rel_left = superif!({
        let value = e_style.get_property_value("left").unwrap();
        let Some(value) = value.strip_suffix("px") else {
            break 'bad;
        };
        let Ok(value) = f64::from_str(&value) else {
            break 'bad;
        };
        value
    } 'bad {
        0.
    });
    let have_block_left = indent_block.get_bounding_client_rect().left();
    let have_e_left = e.get_bounding_client_rect().left();
    return CalculateIndentAlignmentRes {
        have: have_rel_left,
        want: have_block_left - have_e_left + have_rel_left,
    };
}

fn set_indent(root: &Element, indent_block: Element, aligned: &Vec<Element>, width: f32, recurse: bool) {
    let root_left = root.get_bounding_client_rect().left();
    let indent_block = indent_block.dyn_into::<HtmlElement>().unwrap();

    // Update block width
    indent_block.style().set_property("padding-left", &format!("{}px", width)).unwrap();

    // Recursively update child blocks and all line alignments
    fn do_recurse(root_left: f64, indent_block: &Element, aligned: Option<&Vec<Element>>, recurse: bool) {
        let aligned = match aligned {
            Some(a) => Cow::Borrowed(a),
            None => {
                let id = indent_block.get_attribute(ATTR_ID).unwrap().parse::<usize>().unwrap();
                Cow::Owned(get_aligned(id))
            },
        };
        for e in aligned.as_ref() {
            let e_style = e.dyn_ref::<HtmlElement>().unwrap().style();
            let left = calculate_indent_alignment(indent_block, e, &e_style).want;
            let want_prop = format!("{}px", left);
            e_style.set_property("left", &want_prop).unwrap();
        }
        let children = indent_block.children();
        for i in 0 .. children.length() {
            let Ok(child) = children.item(i).unwrap().dyn_into::<Element>() else {
                continue;
            };
            if recurse && child.class_list().contains(CLASS_BLOCK) {
                do_recurse(root_left, &child, None, recurse);
            }
        }
    }

    do_recurse(root_left, &indent_block, Some(aligned), recurse);
}

fn get_aligned_max_width(aligned: &Vec<Element>) -> f32 {
    let mut max_width = 0.;
    for e in aligned {
        // Get inner text that actually has width
        let Some(inner) = e.first_element_child() else {
            continue;
        };
        let width = inner.get_bounding_client_rect().width() as f32;
        if width > max_width {
            max_width = width;
        }
    }
    return max_width;
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
    return at.dyn_ref::<Element>().unwrap().class_list().contains(CLASS_ROOT);
}

fn line_scan(start: &Node, direction: ScanDirection, inclusivity: ScanInclusivity) -> Option<Node> {
    fn is_match(at: &Node) -> bool {
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
    }

    return dom::scan(start, is_root, is_match, direction, inclusivity);
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

fn generate_el_block_indent(ids: &mut usize, type_: BlockType, type_class: &str) -> (usize, Element) {
    let d = document();
    let e = d.create_element("div").unwrap();
    e.class_list().add_2(CLASS_BLOCK, type_class).unwrap();
    let id = *ids;
    *ids += 1;
    e.set_id(&css_id_block(id));
    e.set_attribute(ATTR_ID, &id.to_string()).unwrap();
    e.set_attribute(ATTR_BLOCK_TYPE, &serde_json::to_string(&type_).unwrap()).unwrap();
    return (id, e);
}

struct UpdateLinesStartingAtCtx {
    ids: Rc<RefCell<usize>>,
}

fn update_lines_starting_at(ctx: &mut UpdateLinesStartingAtCtx, mut line: Node) {
    let mut context_line = line_scan(&line, ScanDirection::Backward, ScanInclusivity::Exclusive);

    // Locate root
    let root;
    {
        let Some(mut parent_at) = line.parent_element() else {
            return;
        };
        while !parent_at.class_list().contains(CLASS_ROOT) {
            let Some(temp) = parent_at.parent_element() else {
                // Looking at unrooted element?
                return;
            };
            parent_at = temp;
        }
        root = parent_at;
    }

    // Start walking
    loop {
        let mut changed = false;
        let mut indents = vec![];

        // Get text + selection
        let sel = shed!{
            let Some(sel) = document().get_selection().unwrap() else {
                break None;
            };
            let Some(n) = sel.anchor_node() else {
                break None;
            };
            if n.node_type() == Node::TEXT_NODE {
                break Some((n, sel.anchor_offset() as usize));
            } else if n.node_type() == Node::ELEMENT_NODE {
                if sel.anchor_offset() == 0 {
                    break Some((n, 0));
                }

                fn find_last_text_node(at: &Node) -> Option<Node> {
                    let children = at.child_nodes();
                    for i in (0 .. children.length()).rev() {
                        let child = children.item(i).unwrap();
                        if let Some(_) = child.dyn_ref::<CharacterData>() {
                            return Some(child);
                        }
                        if let Some(found) = find_last_text_node(&child) {
                            return Some(found);
                        }
                    }
                    return None;
                }

                if let Some(child) = find_last_text_node(&n.child_nodes().item(sel.anchor_offset() - 1).unwrap()) {
                    let text_len = child.text_content().unwrap().len();
                    break Some((child, text_len));
                } else {
                    break Some((n, 0));
                }
            } else {
                break None;
            }
        };

        fn recurse_get_text(
            n: &Node,
            out_cursor: &mut Option<usize>,
            out_text: &mut String,
            sel: &Option<(Node, usize)>,
        ) {
            if let Some((sel_n, sel_offset)) = sel {
                if sel_n == n {
                    *out_cursor = Some(out_text.len() + sel_offset);
                }
            }
            if n.node_type() == Node::TEXT_NODE {
                out_text.push_str(&n.node_value().unwrap());
            } else if n.node_type() == Node::ELEMENT_NODE {
                let nodes = n.child_nodes();
                for i in 0 .. nodes.length() {
                    recurse_get_text(&nodes.item(i).unwrap(), out_cursor, out_text, sel);
                }
            }
        }

        let mut text = String::new();
        let mut cursor = None;
        recurse_get_text(&line, &mut cursor, &mut text, &sel);

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
                    type_: serde_json::from_str::<BlockType>(
                        &at_parent.get_attribute(ATTR_BLOCK_TYPE).unwrap(),
                    ).unwrap(),
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
                    BlockType::BlockQuote => {
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
                        } else if let Some(suffix1) = text.strip_prefix(&indent.text) {
                            text = suffix1.to_string();
                        } else {
                            break;
                        }
                    },
                    BlockType::Ol => {
                        if let Ok(parsed) = ReOlNumberPrefix::from_str(&text) {
                            text = parsed.suffix;
                            indent.text = " ".repeat(parsed.number.len());
                            indent.first_text = Some(parsed.number);
                        } else if let Some(suffix1) = text.strip_prefix(&indent.text) {
                            text = suffix1.to_string();
                        } else {
                            break;
                        }
                    },
                    BlockType::BlockCode => {
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
                    type_: serde_json::from_str::<BlockType>(
                        &at_parent.get_attribute(ATTR_BLOCK_TYPE).unwrap(),
                    ).unwrap(),
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
                    BlockType::BlockQuote => {
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
                    BlockType::BlockCode => {
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
            if let Some(suffix1) = text.strip_prefix(PREFIX_BLOCKQUOTE) {
                text = suffix1.to_string();
                let type_ = BlockType::BlockQuote;
                let (parent_id, parent) =
                    generate_el_block_indent(&mut ctx.ids.borrow_mut(), type_, CLASS_BLOCKQUOTE);
                create_parents.push((parent, LineIndent {
                    source_indent: parent_id,
                    type_: BlockType::BlockQuote,
                    first_text: None,
                    text: PREFIX_BLOCKQUOTE.to_string(),
                }));
            } else if let Some(suffix1) = text.strip_prefix(PREFIX_UL_FIRST) {
                text = suffix1.to_string();
                let type_ = BlockType::Ul;
                let (parent_id, parent) = generate_el_block_indent(&mut ctx.ids.borrow_mut(), type_, CLASS_UL);
                create_parents.push((parent, LineIndent {
                    source_indent: parent_id,
                    type_: type_,
                    first_text: Some(PREFIX_UL_FIRST.to_string()),
                    text: PREFIX_UL.to_string(),
                }));
            } else if let Ok(parsed) = ReOlNumberPrefix::from_str(&text) {
                text = parsed.suffix;
                let type_ = BlockType::Ol;
                let (parent_id, parent) = generate_el_block_indent(&mut ctx.ids.borrow_mut(), type_, CLASS_OL);
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
                    let type_ = BlockType::BlockCode;
                    let (parent_id, parent) =
                        generate_el_block_indent(&mut ctx.ids.borrow_mut(), type_, CLASS_BLOCKCODE);
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
            crate::cprintln!(
                "Move [{}] create parents {:?}; lifting {}",
                text,
                create_parents.iter().map(|x| x.1.type_).collect::<Vec<_>>(),
                lift_following.len()
            );
            changed = true;

            // Lift line + any following elements between the line and the placement root
            web_sys::console::log_2(&JsValue::from("lifting line"), &JsValue::from(&line));
            line.parent_node().unwrap().remove_child(&line).unwrap();
            for e in &lift_following {
                e.parent_node().unwrap().remove_child(&e).unwrap();
            }

            // Create blocks and place this line
            {
                let mut place_parent = place_parent.clone();
                let mut place_before = place_before.clone();
                for (new_parent, indent) in create_parents.into_iter() {
                    web_sys::console::log_4(
                        &JsValue::from("place new parent"),
                        &JsValue::from(&place_parent),
                        &JsValue::from(place_before.as_ref()),
                        &JsValue::from(&new_parent),
                    );
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

        // Parse text into inline and sync with line
        let offset = Cell::new(0usize);
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
            generate_line_code_shadowdom(&mut indents, text, blockcode_delim)
        } else {
            // Parse line type
            let line_type;
            if let Ok(parsed) = ReHeadingPrefix::from_str(&text) {
                let heading_level = parsed.level.len();
                line_type = LineType::Heading(heading_level);
            } else {
                line_type = LineType::Normal;
            }

            // Parse inline
            let inline = linemarkdown::parse(&text);

            // Generate and sync
            generate_line_shadowdom(line_type, &mut indents, inline)
        };
        line = sync_shadow_dom(&mut cursor, &offset, &line, line_shadowdom);

        // Update indents
        {
            let children = line.dyn_ref::<Element>().unwrap().children();
            for i in 0 .. children.length() {
                let child = children.item(i).unwrap();
                let Some(id) = child.get_attribute(ATTR_INDENT_ID) else {
                    break;
                };
                let Some(inner) = child.first_element_child() else {
                    continue;
                };
                let id = usize::from_str_radix(&id, 10).unwrap();
                let indent_block =
                    document().get_element_by_id(&css_id_block(id)).unwrap().dyn_into::<Element>().unwrap();
                let width = inner.get_bounding_client_rect().width() as f32;
                let set_width =
                    window()
                        .get_computed_style(&indent_block)
                        .unwrap()
                        .unwrap()
                        .get_property_value("padding-left")
                        .unwrap()
                        .strip_suffix("px")
                        .unwrap()
                        .parse::<f32>()
                        .unwrap();
                if width > set_width {
                    set_indent(&root, indent_block, &get_aligned(id), width, true);
                } else {
                    let style = child.dyn_ref::<HtmlElement>().unwrap().style();
                    let left = calculate_indent_alignment(&indent_block, &child, &style);
                    if (left.have - left.want).abs() > 0.5 {
                        style.set_property("left", &format!("{}px", left.want)).unwrap();
                    }
                }
            }
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
}

pub fn build(initial: impl IntoIterator<Item = Block>) -> El {
    let ids = Rc::new(RefCell::new(0usize));

    // Create initial tree
    let root = el("div").classes(&[NAMESPACE, CLASS_ROOT]).attr("contenteditable", "true");
    {
        struct GenerateContext {
            ids: Rc<RefCell<usize>>,
            indents: Vec<LineIndent>,
        }

        fn recursive_generate_block(ctx: &mut GenerateContext, parent_container: &mut Vec<Element>, block: Block) {
            match block {
                Block::Heading(level, block) => {
                    parent_container.push(
                        generate_shadow_dom(
                            &Cell::new(0usize),
                            generate_line_shadowdom(LineType::Heading(level), &mut ctx.indents, block),
                        )
                            .dyn_into()
                            .unwrap(),
                    );
                },
                Block::Line(block) => {
                    parent_container.push(
                        generate_shadow_dom(
                            &Cell::new(0usize),
                            generate_line_shadowdom(LineType::Normal, &mut ctx.indents, block),
                        )
                            .dyn_into()
                            .unwrap(),
                    );
                },
                Block::Code(block) => {
                    let (_, e_block) =
                        generate_el_block_indent(&mut ctx.ids.borrow_mut(), BlockType::BlockCode, CLASS_BLOCKCODE);
                    let mut children = vec![];
                    let block_len = block.len();
                    for (i, child) in block.into_iter().enumerate() {
                        children.push(
                            generate_shadow_dom(
                                &Cell::new(0usize),
                                generate_line_code_shadowdom(&mut ctx.indents, child, if i == 0 {
                                    Some(BlockcodeDelim::Begin)
                                } else if i + 1 == block_len {
                                    Some(BlockcodeDelim::End)
                                } else {
                                    None
                                }),
                            ),
                        );
                    }
                    e_block.append_with_node(&children.into_iter().map(|e| JsValue::from(e)).collect()).unwrap();
                    parent_container.push(e_block);
                },
                Block::Quote(block) => {
                    let (id, e_block) =
                        generate_el_block_indent(&mut ctx.ids.borrow_mut(), BlockType::BlockQuote, CLASS_BLOCKQUOTE);
                    ctx.indents.push(LineIndent {
                        source_indent: id,
                        type_: BlockType::BlockQuote,
                        first_text: None,
                        text: "> ".to_string(),
                    });
                    let mut children = vec![];
                    for child in block {
                        recursive_generate_block(ctx, &mut children, child);
                    }
                    e_block.append_with_node(&children.into_iter().map(|e| JsValue::from(e)).collect()).unwrap();
                    ctx.indents.pop();
                    parent_container.push(e_block);
                },
                Block::Ul(block) => {
                    let (id, e_block) =
                        generate_el_block_indent(&mut ctx.ids.borrow_mut(), BlockType::Ul, CLASS_UL);
                    let mut children = vec![];
                    for bullet in block {
                        ctx.indents.push(LineIndent {
                            source_indent: id,
                            type_: BlockType::Ul,
                            first_text: Some("* ".to_string()),
                            text: "  ".to_string(),
                        });
                        for child in bullet {
                            recursive_generate_block(ctx, &mut children, child);
                        }
                        ctx.indents.pop();
                    }
                    e_block.append_with_node(&children.into_iter().map(|e| JsValue::from(e)).collect()).unwrap();
                    parent_container.push(e_block);
                },
                Block::Ol(block) => {
                    let (id, e_block) =
                        generate_el_block_indent(&mut ctx.ids.borrow_mut(), BlockType::Ol, CLASS_OL);
                    let mut children = vec![];
                    for (i, bullet) in block.into_iter().enumerate() {
                        ctx.indents.push(LineIndent {
                            source_indent: id,
                            type_: BlockType::Ol,
                            first_text: Some(format!("{}. ", i)),
                            text: "   ".to_string(),
                        });
                        for child in bullet {
                            recursive_generate_block(ctx, &mut children, child);
                        }
                        ctx.indents.pop();
                    }
                    e_block.append_with_node(&children.into_iter().map(|e| JsValue::from(e)).collect()).unwrap();
                    parent_container.push(e_block);
                },
            }
        }

        let mut root_children = vec![];
        let mut ctx = GenerateContext {
            ids: ids.clone(),
            indents: vec![],
        };
        for block in initial {
            recursive_generate_block(&mut ctx, &mut root_children, block);
        }
        root.ref_extend(root_children.into_iter().map(el_from_raw).collect());
        root.ref_own(|_| Timeout::new(0, {
            let root = root.clone();
            move || {
                let indent_blocks = document().get_elements_by_class_name(CLASS_BLOCK);
                for i in 0 .. indent_blocks.length() {
                    let indent_block = indent_blocks.item(i).unwrap();
                    let id = usize::from_str_radix(&indent_block.get_attribute(ATTR_ID).unwrap(), 10).unwrap();
                    let aligned = get_aligned(id);
                    let max_width = get_aligned_max_width(&aligned);
                    set_indent(&root.raw(), indent_block, &aligned, max_width, false);
                }
            }
        }));
    }

    // Setup change handler
    root.ref_own(|self1| {
        let observer = MutationObserver::new(&Closure::<dyn Fn(Array) -> ()>::new({
            struct Change {
                line: Node,
            }

            struct DelayState {
                delay: Option<Timeout>,
                changes: Vec<Change>,
            }

            let delay = Rc::new(RefCell::new(DelayState {
                delay: None,
                changes: vec![],
            }));
            let ids = ids.clone();
            move |mutations: Array| {
                'next_mutation: for m in mutations {
                    let m = MutationRecord::from(m);
                    let mut starts = vec![];

                    // Start from added elements
                    let added_nodes = m.added_nodes();
                    for i in 0 .. added_nodes.length() {
                        let Some(at) = added_nodes.item(i) else {
                            continue;
                        };
                        starts.push(at);
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
                                    break 'start following;
                                }
                            };

                            // Otherwise maybe child of a line/block
                            break 'start m.target().unwrap();
                        }
                        else {
                            // Text within line
                            break 'start m.target().unwrap();
                        }
                    };
                    starts.push(following);

                    // For each start, check if it's a line (or get the next line otherwise that could
                    // be affected by the change)
                    let mut delay_mut = delay.borrow_mut();
                    for start in starts {
                        let line = shed!{
                            'found _;
                            // Search for a line upward
                            let mut at = start.clone();
                            loop {
                                if let Some(at_temp) = at.dyn_ref::<Element>() {
                                    let classes = at_temp.class_list();
                                    if classes.contains(CLASS_BLOCK) {
                                        break;
                                    }
                                    if classes.contains(CLASS_ROOT) {
                                        break;
                                    }
                                }
                                if is_line(&at) {
                                    break 'found at;
                                }
                                let Some(at_temp) = at.parent_node() else {
                                    break;
                                };
                                at = at_temp;
                            }
                            // Not within a line, so some child of a block? So find the next line to start from
                            let Some(line) = line_scan(&start, ScanDirection::Forward, ScanInclusivity::Inclusive) else {
                                continue 'next_mutation;
                            };
                            break 'found line;
                        };
                        delay_mut.changes.push(Change { line: line });
                    }

                    // Throttle change handling, don't cause lag while typing actively
                    delay_mut.delay = Some(Timeout::new(200, {
                        let delay = delay.clone();
                        let ids = ids.clone();
                        move || {
                            let mut delay_mut = delay.borrow_mut();
                            delay_mut.delay = None;
                            let changes = delay_mut.changes.split_off(0);
                            drop(delay_mut);
                            let seen = js_sys::Set::new(&JsValue::from(js_sys::Array::new()));
                            let mut ctx = UpdateLinesStartingAtCtx { ids: ids };
                            for change in changes {
                                let el_js_value = JsValue::from(&change.line);
                                if seen.has(&el_js_value) {
                                    continue;
                                }
                                seen.add(&el_js_value);
                                update_lines_starting_at(&mut ctx, change.line);
                            }
                        }
                    }));
                }
            }
        }).into_js_value().into()).unwrap();
        observer.observe_with_options(&self1.raw(), &{
            let o = MutationObserverInit::new();
            o.set_child_list(true);
            o.set_character_data(true);
            o.set_subtree(true);
            o
        }).unwrap();
        observer
    });

    // Set up contenteditable navigation issue workaround hack
    //
    // In contenteditable, there are cursor positions inbetween elements which makes
    // navigation unpleasant. Deletes in these positions delete elements without
    // touching content, then the change handler will recreate them. Up and down move
    // to the start/end of the block element not to the previous/next line in the
    // neighboring block elements.
    //
    // This intercepts keypresses at the start/end of a text node and translates them
    // to the expected movements/changes.
    root.ref_own(
        |e| EventListener::new_with_options(
            &e.raw(),
            "keydown",
            EventListenerOptions::enable_prevent_default(),
            |event| {
                let event = event.dyn_ref::<KeyboardEvent>().unwrap();
                let Some(sel) = document().get_selection().unwrap() else {
                    return;
                };
                let Some(node) = sel.anchor_node() else {
                    return;
                };
                let Ok(node) = node.dyn_into() else {
                    return;
                };
                let direction;

                enum KeyDimension {
                    Vert,
                    Horiz,
                }

                let dim;
                match event.key().as_str() {
                    "ArrowLeft" => {
                        dim = KeyDimension::Horiz;
                        direction = ScanDirection::Backward;
                    },
                    "ArrowRight" => {
                        dim = KeyDimension::Horiz;
                        direction = ScanDirection::Forward;
                    },
                    "ArrowUp" => {
                        dim = KeyDimension::Vert;
                        direction = ScanDirection::Backward;
                    },
                    "ArrowDown" => {
                        dim = KeyDimension::Vert;
                        direction = ScanDirection::Forward;
                    },
                    "Backspace" => {
                        dim = KeyDimension::Horiz;
                        direction = ScanDirection::Backward;
                    },
                    "Delete" => {
                        dim = KeyDimension::Horiz;
                        direction = ScanDirection::Forward;
                    },
                    _ => {
                        return;
                    },
                }

                fn is_text(n: &Node) -> bool {
                    return n.node_type() == Node::TEXT_NODE;
                }

                fn is_line_block_or_root(n: &Node) -> bool {
                    if let Some(n) = n.dyn_ref::<Element>() {
                        let classes = n.class_list();
                        if classes.contains(CLASS_LINE) || classes.contains(CLASS_ROOT) {
                            return true;
                        }
                    }
                    return false;
                }

                match dim {
                    KeyDimension::Horiz => {
                        // This doubles up on event handling - this adjusts the starting position, then
                        // the normal handler does an additional move. This clears the (extra) gap plus
                        // the intended 1 movement.
                        if !sel.is_collapsed() {
                            return;
                        }
                        if direction == ScanDirection::Backward && sel.anchor_offset() == 0 {
                            {
                                // Allow backspace at root of node, rather than last character of previous line
                                if is_line(&node) {
                                    return;
                                }
                                let parent = node.parent_element().unwrap();
                                if is_line(&parent) && parent.first_child().unwrap() == node {
                                    return;
                                }
                            }
                            let Some(prev) =
                                dom::scan(
                                    &node,
                                    is_line_block_or_root,
                                    is_text,
                                    ScanDirection::Backward,
                                    ScanInclusivity::Exclusive,
                                ) else {
                                    return;
                                };
                            dom::select(&prev, prev.text_content().unwrap().len());
                        } else if direction == ScanDirection::Forward &&
                            sel.anchor_offset() as usize == node.text_content().unwrap().len() {
                            let Some(next) =
                                dom::scan(
                                    &node,
                                    is_line_block_or_root,
                                    is_text,
                                    ScanDirection::Forward,
                                    ScanInclusivity::Exclusive,
                                ) else {
                                    return;
                                };
                            dom::select(&next, 0);
                        } else {
                            return;
                        }
                    },
                    KeyDimension::Vert => {
                        // This prevents default handling so this is the only handler; it moves to the
                        // exact desired location.
                        let current_sel_box = sel.get_range_at(0).unwrap().get_bounding_client_rect();

                        // Use the line instead of the current node because we care about moves between
                        // block elements and need the full line box
                        let mut node = node;
                        while !is_line(&node) {
                            node = node.parent_node().unwrap();
                        }
                        let next_cursor = shed!{
                            'found _;
                            match direction {
                                ScanDirection::Backward => {
                                    for i in 0 .. 3 {
                                        if let Some(c) =
                                            document().caret_position_from_point(
                                                current_sel_box.left() as f32,
                                                (current_sel_box.top() -
                                                    current_sel_box.height() * (0.5 + i as f64)) as
                                                    f32,
                                            ) {
                                            let n = c.offset_node().unwrap();

                                            // Check if caret is somehow on the same line, despite math, and only use it if
                                            // so. Check by the far edge to be particularly sure.
                                            let new_sel_box = {
                                                let range = document().create_range().unwrap();
                                                range.set_start(&n, c.offset()).unwrap();
                                                range.set_end(&n, c.offset()).unwrap();
                                                range.get_bounding_client_rect()
                                            };
                                            if (new_sel_box.top() - current_sel_box.top()) / new_sel_box.height() <
                                                -0.4 {
                                                break 'found c;
                                            }
                                        }
                                    }
                                },
                                ScanDirection::Forward => {
                                    for i in 0 .. 3 {
                                        if let Some(c) =
                                            document().caret_position_from_point(
                                                current_sel_box.left() as f32,
                                                (current_sel_box.bottom() +
                                                    current_sel_box.height() * (0.5 + i as f64)) as
                                                    f32,
                                            ) {
                                            let n = c.offset_node().unwrap();

                                            // Check if caret is somehow on the same line, despite math, and only use it if so
                                            let new_sel_box = {
                                                let range = document().create_range().unwrap();
                                                range.set_start(&n, c.offset()).unwrap();
                                                range.set_end(&n, c.offset()).unwrap();
                                                range.get_bounding_client_rect()
                                            };
                                            crate::cprintln!(
                                                "down, next: {} - {} l {}, sel {} - {} l {}",
                                                current_sel_box.top(),
                                                current_sel_box.bottom(),
                                                current_sel_box.left(),
                                                new_sel_box.top(),
                                                new_sel_box.bottom(),
                                                new_sel_box.left()
                                            );
                                            if (new_sel_box.bottom() - current_sel_box.bottom()) /
                                                new_sel_box.height() >
                                                0.4 {
                                                break 'found c;
                                            }
                                            crate::cprintln!(
                                                "down, overlaps current line: {} - {}, sel {} - {}, res {}",
                                                current_sel_box.top(),
                                                current_sel_box.bottom(),
                                                new_sel_box.top(),
                                                new_sel_box.bottom(),
                                                (new_sel_box.bottom() - current_sel_box.bottom()) /
                                                    new_sel_box.height()
                                            );
                                        }
                                    }
                                },
                            };
                            return;
                        };
                        let Some(next_cursor_node) = next_cursor.offset_node() else {
                            return;
                        };
                        dom::select(&next_cursor_node, next_cursor.offset() as usize);
                        event.prevent_default();
                    },
                }
            },
        ),
    );

    // Done
    return root;
}
