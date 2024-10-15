use {
    const_format::formatcp,
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
        WeakMap,
    },
    rooting::{
        el,
        el_from_raw,
        set_root,
    },
    rustemo::Parser,
    serde::{
        Deserialize,
        Serialize,
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
    wasm::shadowdom::{
        generate_shadow_dom,
        sync_shadow_dom,
        ShadowDom,
        ShadowDomElement,
    },
    wasm_bindgen::{
        closure::Closure,
        convert::IntoWasmAbi,
        JsCast,
        JsValue,
    },
    web_sys::{
        console,
        Element,
        HtmlElement,
        MutationObserver,
        MutationObserverInit,
        MutationRecord,
        Node,
        NodeList,
    },
};

macro_rules! eprintln{
    ($pat: literal) => {
        web_sys:: console:: log_1(& wasm_bindgen:: JsValue:: from_str($pat))
    };
    ($pat: literal, $($data: expr), *) => {
        web_sys::console::log_1(&wasm_bindgen::JsValue::from_str(&format!($pat, $($data,) *)))
    };
}

mod grammar {
    pub(crate) mod inline {
        include!(concat!(env!("OUT_DIR"), "/inline/inline.rs"));
    }

    pub(crate) mod inline_actions {
        include!(concat!(env!("OUT_DIR"), "/inline/inline_actions.rs"));
    }
}

const NAMESPACE: &str = "malarkdowney";
const ATTR_ID: &str = formatcp!("{}_id", NAMESPACE);
const ATTR_INDENT_ID: &str = formatcp!("{}_indent_id", NAMESPACE);
const ATTR_BLOCK_TYPE: &str = formatcp!("{}_block_type", NAMESPACE);
const CLASS_NAMESPACE: &str = NAMESPACE;
const CLASS_ROOT: &str = formatcp!("{}_root", NAMESPACE);
const CLASS_INCOMPLETE: &str = formatcp!("{}_incomplete", NAMESPACE);
const CLASS_ALIGNED: &str = formatcp!("{}_aligned", NAMESPACE);
const CLASS_BLOCK_INDENT: &str = formatcp!("{}_block_indent", NAMESPACE);
const CLASS_LINE: &str = formatcp!("{}_line", NAMESPACE);
const CLASS_BLOCKCODE: &str = formatcp!("{}_blockcode", NAMESPACE);
const CLASS_BLOCKQUOTE: &str = formatcp!("{}_blockquote", NAMESPACE);
const CLASS_UL: &str = formatcp!("{}_ul", NAMESPACE);
const CLASS_OL: &str = formatcp!("{}_ol", NAMESPACE);
const CLASS_PSEUDO_A: &str = formatcp!("{}_pseudo_a", NAMESPACE);
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
    Code,
}

fn class_aligned(id: usize) -> String {
    return format!("{}_aligned_{}", NAMESPACE, id);
}

fn css_id_block(id: usize) -> String {
    return format!("{}_indent_{}", NAMESPACE, id);
}

pub struct InlineStrong {
    pub incomplete: bool,
    pub children: Vec<Inline>,
}

pub struct InlineEmphasis {
    pub incomplete: bool,
    pub children: Vec<Inline>,
}

pub struct InlineLink {
    pub incomplete: bool,
    // `[..]`
    pub title: Vec<Inline>,
    pub address: Option<InlineLinkAddress>,
}

pub struct InlineLinkAddress {
    // `(`
    pub prefix: String,
    pub address: String,
    // `)`
    pub suffix: String,
}

pub struct InlineCode {
    pub incomplete: bool,
    pub text: String,
}

pub enum Inline {
    Text(String),
    Strong(InlineStrong),
    Emphasis(InlineEmphasis),
    Link(InlineLink),
    Code(InlineCode),
}

pub enum Block {
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

fn generate_line_shadowdom(line_type: LineType, indents: &mut [LineIndent], inlines: Vec<Inline>) -> ShadowDom {
    let mut want_children = vec![];
    for indent in indents {
        want_children.push(ShadowDom::Element(ShadowDomElement {
            tag: "span",
            classes: [CLASS_NAMESPACE.to_string(), CLASS_ALIGNED.to_string(), class_aligned(indent.source_indent)]
                .into_iter()
                .collect(),
            attrs: [(ATTR_INDENT_ID.to_string(), indent.source_indent.to_string())].into_iter().collect(),
            children: vec![ShadowDom::Text(indent.first_text.take().unwrap_or_else(|| indent.text.clone()))],
        }));
    }

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
                        if want.incomplete {
                            out.insert(CLASS_INCOMPLETE.to_string());
                        }
                        out
                    },
                    attrs: Default::default(),
                    children: {
                        let mut out = vec![];
                        for c in want.children {
                            out.push(generate_inline(c));
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
                        if want.incomplete {
                            out.insert(CLASS_INCOMPLETE.to_string());
                        }
                        out
                    },
                    attrs: Default::default(),
                    children: {
                        let mut out = vec![];
                        for c in want.children {
                            out.push(generate_inline(c));
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
                        for c in want.title {
                            out.push(generate_inline(c));
                        }
                        if let Some(address) = want.address {
                            out.push(ShadowDom::Element(ShadowDomElement {
                                tag: "a",
                                classes: [CLASS_NAMESPACE.to_string()].into_iter().collect(),
                                attrs: {
                                    let mut out = HashMap::new();
                                    out.insert("href".to_string(), address.address.clone());
                                    out
                                },
                                children: vec![
                                    ShadowDom::Text(format!("{}{}{}", address.prefix, address.address, address.suffix))
                                ],
                            }));
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
                    attrs: Default::default(),
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
            LineType::Code => "code",
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

fn set_indent(root: &Element, indent_block: Element, aligned: &Vec<Element>, width: f32, recurse: bool) {
    let root_left = root.get_bounding_client_rect().left();
    let indent_block = indent_block.dyn_into::<HtmlElement>().unwrap();

    // Update block width
    indent_block.style().set_property("padding-left", &format!("{}px", width)).unwrap();

    // Recursively update child blocks and all line alignments
    fn do_recurse(root_left: f64, indent_block: &Element, aligned: Option<&Vec<Element>>, recurse: bool) {
        let left = indent_block.get_bounding_client_rect().left() - root_left;
        let aligned = match aligned {
            Some(a) => Cow::Borrowed(a),
            None => {
                let id = indent_block.get_attribute(ATTR_ID).unwrap().parse::<usize>().unwrap();
                Cow::Owned(get_aligned(id))
            },
        };
        let want_prop = format!("{}px", left);
        for e in aligned.as_ref() {
            e.dyn_ref::<HtmlElement>().unwrap().style().set_property("left", &want_prop).unwrap();
        }
        let children = indent_block.children();
        for i in 0 .. children.length() {
            let Ok(child) = children.item(i).unwrap().dyn_into::<Element>() else {
                continue;
            };
            if recurse && child.class_list().contains(CLASS_BLOCK_INDENT) {
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
        let width = e.client_width() as f32;
        if width > max_width {
            max_width = width;
        }
    }
    return max_width;
}

#[derive(PartialEq)]
enum MoveMode {
    Inclusive,
    Exclusive,
}

fn get_line_backwards(n: &Node, mode: MoveMode) -> Option<Element> {
    let mut skip_first_next = mode == MoveMode::Inclusive;
    let mut at = n.clone();
    loop {
        loop {
            // Move backward
            if skip_first_next {
                skip_first_next = false;
            } else {
                let Some(at_temp) = at.previous_sibling() else {
                    // (move up)
                    break;
                };
                at = at_temp;
            }

            // Match or move down
            loop {
                let Some(at_el) = at.dyn_ref::<Element>() else {
                    break;
                };
                let class_list = at_el.class_list();

                // Match
                if class_list.contains(CLASS_LINE) {
                    return Some(at_el.clone());
                }

                // Or move down
                let Some(at_temp) = at_el.last_child() else {
                    // (no children, try moving backwards again)
                    break;
                };
                at = at_temp;
            }
        }

        // Move up
        {
            let Some(at_temp) = at.parent_node() else {
                return None;
            };
            at = at_temp;
        }
        if at.dyn_ref::<Element>().unwrap().class_list().contains(CLASS_ROOT) {
            return None;
        }
    }
}

fn get_line_forward(n: &Node, mode: MoveMode) -> Option<Element> {
    let mut skip_first_next = mode == MoveMode::Inclusive;
    let mut at = n.clone();
    loop {
        loop {
            // Move forward
            if skip_first_next {
                skip_first_next = false;
            } else {
                let Some(at_temp) = at.next_sibling() else {
                    // (move up)
                    break;
                };
                at = at_temp;
            }

            // Match or move down
            loop {
                let Some(at_el) = at.dyn_ref::<Element>() else {
                    break;
                };
                let class_list = at_el.class_list();

                // Match
                if class_list.contains(CLASS_LINE) {
                    return Some(at_el.clone());
                }

                // Or move down
                let Some(at_temp) = at.first_child() else {
                    // (no children, try moving backwards again)
                    break;
                };
                at = at_temp;
            }
        }

        // Move up
        {
            let Some(at_temp) = at.parent_node() else {
                return None;
            };
            at = at_temp;
        }
        if at.dyn_ref::<Element>().unwrap().class_list().contains(CLASS_ROOT) {
            return None;
        }
    }
}

#[structre("^(?<number>\\d+\\. )(?<suffix>.*)$")]
struct ReOlNumberPrefix {
    number: String,
    suffix: String,
}

#[structre("^(?<level>#{1,6}) (?<suffix>.*)$")]
struct ReHeadingPrefix {
    level: String,
    suffix: String,
}

fn generate_el_block_indent(ids: &mut usize, type_: BlockType, type_class: &str) -> (usize, Element) {
    let d = document();
    let e = d.create_element("div").unwrap();
    e.class_list().add_2(CLASS_BLOCK_INDENT, type_class).unwrap();
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

fn update_lines_starting_at(ctx: &mut UpdateLinesStartingAtCtx, mut line: Element) {
    let mut context_line = get_line_backwards(&line, MoveMode::Exclusive);

    // Locate root
    let root;
    {
        let mut parent_at = line.clone();
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
                eprintln!("no selection");
                break None;
            };
            let Some(n) = sel.anchor_node() else {
                eprintln!("no selection anchor node");
                break None;
            };
            if n.node_type() == Node::TEXT_NODE {
                break Some((n, sel.anchor_offset() as usize));
            } else if n.node_type() == Node::ELEMENT_NODE {
                console::log_2(
                    &JsValue::from(
                        &format!(
                            "sel anchor type node; offset {} children {}",
                            sel.anchor_offset(),
                            n.child_nodes().length()
                        ),
                    ),
                    &JsValue::from(&n),
                );
                let Some(child) = n.child_nodes().item(sel.anchor_offset()) else {
                    // Spec vague, not sure what happens to cause this
                    break None;
                };
                break (Some((child, 0)));
            } else {
                eprintln!("selection anchor node type {}", n.node_type());
                break None;
            }
        };

        fn recurse_get_text(
            out_cursor: &mut Option<usize>,
            out_text: &mut String,
            sel: &Option<(Node, usize)>,
            nodes: NodeList,
        ) {
            for i in 0 .. nodes.length() {
                let n = nodes.item(i).unwrap();
                if let Some((sel_n, sel_offset)) = sel {
                    if *sel_n == n {
                        *out_cursor = Some(out_text.len() + sel_offset);
                    }
                }
                if n.node_type() == Node::TEXT_NODE {
                    out_text.push_str(&n.node_value().unwrap());
                } else if n.node_type() == Node::ELEMENT_NODE {
                    recurse_get_text(out_cursor, out_text, sel, n.child_nodes());
                }
            }
        }

        let mut text = String::new();
        let mut cursor = None;
        recurse_get_text(&mut cursor, &mut text, &sel, line.child_nodes());
        eprintln!("Line! Text [{}]", text);

        // Find default root
        let mut place_before = line.next_sibling();
        let mut place_parent = line.parent_element().unwrap();
        while place_parent != root {
            place_before = place_parent.next_sibling();
            place_parent = place_parent.parent_element().unwrap();
        }

        // If there's a context line
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
            let context_children = context_line.children();
            for (i, (_, _, block_indent)) in root_context_path.iter_mut().enumerate() {
                let Some(aligned) = context_children.item(i as u32) else {
                    continue;
                };
                block_indent.text = " ".repeat(aligned.text_content().unwrap().len());
            }

            // Find higest indent of previous/context this should be placed under by matching
            // indents to line prefixes
            eprintln!(
                "matching context prefixes [{}] from root context path {:?}",
                text,
                root_context_path.iter().map(|x| (x.2.type_, &x.2.text)).collect::<Vec<_>>()
            );
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
                    BlockType::BlockCode => unreachable!(),
                }
                place_parent = parent;
                place_before = parent_next;
                indents.push(indent);
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
                    break 'under_place_parent;
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
        } 'under_place_parent {
            context_line_path.reverse();

            // Could be splitting a parent container; get list of following lines to drag
            // along to placement root (and the split parents will be recreated later)
            shed!{
                let mut lift_at = line.clone();
                'lift_end : loop {
                    // Find next element
                    loop {
                        // Try to move forward
                        if let Some(lift_at_next) = lift_at.next_element_sibling() {
                            lift_at = lift_at_next;
                            if lift_at.parent_element().unwrap() == place_parent {
                                // Reached root, stop
                                place_before = Some(lift_at.dyn_into().unwrap());
                                break 'lift_end;
                            }
                            break;
                        }
                        if lift_at.parent_element().unwrap() == place_parent {
                            // Reached root (no next element), stop
                            place_before = None;
                            break 'lift_end;
                        }

                        // Can't move forward, try moving up (then resume moving forward)
                        let lift_at_next = lift_at.parent_element().unwrap();
                        lift_at = lift_at_next;
                    };

                    // Remember
                    lift_following.push(lift_at.clone());
                }
            }

            // Move placement parent down current line's tree by matching prefixes
            eprintln!(
                "matching line prefixes [{}] from context line path {:?}",
                text,
                context_line_path.iter().map(|x| (x.2.type_, &x.2.text)).collect::<Vec<_>>()
            );
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
                    BlockType::BlockCode => unreachable!(),
                }
                place_parent = parent;
                place_before = parent_next;
                indents.push(indent);
            }
        });

        // Check if starting new prefixed blocks
        let mut create_parents = vec![];
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
            } else if let Some(suffix1) = text.strip_prefix(PREFIX_BLOCKCODE) {
                text = suffix1.to_string();
                let type_ = BlockType::BlockCode;
                let (parent_id, parent) =
                    generate_el_block_indent(&mut ctx.ids.borrow_mut(), type_, CLASS_BLOCKCODE);
                create_parents.push((parent, LineIndent {
                    source_indent: parent_id,
                    type_: type_,
                    first_text: None,
                    text: "".to_string(),
                }));
                break;
            } else {
                break;
            }
        }

        // Move
        let original_parent = line.parent_element().unwrap();
        eprintln!(
            "Remaining text [{}]\nplace parent same {}, matched indents {:?}, create indents {:?}",
            text,
            place_parent == original_parent,
            indents.iter().map(|p| format!("{:?}", p.type_)).collect::<Vec<_>>(),
            create_parents.iter().map(|p| format!("{:?}", p.1.type_)).collect::<Vec<_>>()
        );
        console::log_2(
            &JsValue::from("- place before"),
            &place_before.as_ref().map(|x| JsValue::from(x)).unwrap_or_else(|| JsValue::from("none")),
        );
        if place_parent != original_parent || !create_parents.is_empty() {
            changed = true;

            // Lift line + any following elements between the line and the placement root
            line.remove();
            for e in &lift_following {
                e.remove();
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

            // Create blocks and place this line
            if create_parents.is_empty() {
                place_parent.insert_before(&line, place_before.as_ref()).unwrap();
            } else {
                let mut place_parent = place_parent.clone();
                for (depth, (new_parent, indent)) in create_parents.into_iter().enumerate() {
                    place_parent.insert_before(&new_parent, if depth == 0 {
                        place_before.as_ref()
                    } else {
                        None
                    }).unwrap();
                    place_parent = new_parent;
                    indents.push(indent);
                }
                place_parent.append_child(&line).unwrap();
            }

            // Place remaining lines
            for e in lift_following {
                place_parent.insert_before(&e, place_before.as_ref()).unwrap();
            }
        }

        // Parse text into inline and sync with line
        {
            // Parse line type
            let line_type;
            if let Ok(parsed) = ReHeadingPrefix::from_str(&text) {
                text = parsed.suffix;
                let heading_level = parsed.level.len();
                line_type = LineType::Heading(heading_level);
            } else {
                line_type = LineType::Normal;
            }

            // Parse inline
            //.            let ast =
            //.                grammar::inline::InlineParser::new()
            //.                    .parse(&text)
            //.                    .unwrap()
            //.                    .get_first_tree()
            //.                    .unwrap()
            //.                    .build(&mut grammar::inline::DefaultBuilder::new())
            //.                    .unwrap_or_default();
            //. 
            //.            trait OriginalText {
            //.                fn orig_text(&self) -> String;
            //.            }
            //. 
            //.            impl OriginalText for grammar::inline_actions::StrongDelim {
            //.                fn orig_text(&self) -> String {
            //.                    return "*".to_string();
            //.                }
            //.            }
            //. 
            //.            impl OriginalText for grammar::inline_actions::EmphasisDelim {
            //.                fn orig_text(&self) -> String {
            //.                    return "_".to_string();
            //.                }
            //.            }
            //. 
            //.            impl OriginalText for grammar::inline_actions::CodeDelim {
            //.                fn orig_text(&self) -> String {
            //.                    return "`".to_string();
            //.                }
            //.            }
            //. 
            //.            impl OriginalText for grammar::inline_actions::LinkTitlePrefix {
            //.                fn orig_text(&self) -> String {
            //.                    return "[".to_string();
            //.                }
            //.            }
            //. 
            //.            impl OriginalText for grammar::inline_actions::LinkTitleSuffix {
            //.                fn orig_text(&self) -> String {
            //.                    return "]".to_string();
            //.                }
            //.            }
            //. 
            //.            impl OriginalText for grammar::inline_actions::LinkAddressPrefix {
            //.                fn orig_text(&self) -> String {
            //.                    return "(".to_string();
            //.                }
            //.            }
            //. 
            //.            impl OriginalText for grammar::inline_actions::LinkAddressSuffix {
            //.                fn orig_text(&self) -> String {
            //.                    return ")".to_string();
            //.                }
            //.            }
            //. 
            //.            impl OriginalText for grammar::inline_actions::EscapeChar {
            //.                fn orig_text(&self) -> String {
            //.                    return "\\".to_string();
            //.                }
            //.            }
            //. 
            //.            fn translate_ast(e: grammar::inline_actions::InlineEl) -> Inline {
            //.                match e {
            //.                    grammar::inline_actions::InlineEl::Strong(e) => {
            //.                        return Inline::Strong(InlineStrong {
            //.                            incomplete: e.suffix.is_none(),
            //.                            children: {
            //.                                let mut out = vec![Inline::Text(e.prefix.orig_text())];
            //.                                for c in e.children.unwrap_or_default() {
            //.                                    out.push(translate_ast(c));
            //.                                }
            //.                                if let Some(suffix) = e.suffix {
            //.                                    out.push(Inline::Text(suffix.orig_text()));
            //.                                }
            //.                                out
            //.                            },
            //.                        });
            //.                    },
            //.                    grammar::inline_actions::InlineEl::Emphasis(e) => {
            //.                        return Inline::Emphasis(InlineEmphasis {
            //.                            incomplete: e.suffix.is_none(),
            //.                            children: {
            //.                                let mut out = vec![Inline::Text(e.prefix.orig_text())];
            //.                                for c in e.children.unwrap_or_default() {
            //.                                    out.push(translate_ast(c));
            //.                                }
            //.                                if let Some(suffix) = e.suffix {
            //.                                    out.push(Inline::Text(suffix.orig_text()));
            //.                                }
            //.                                out
            //.                            },
            //.                        });
            //.                    },
            //.                    grammar::inline_actions::InlineEl::Link(e) => {
            //.                        let mut title = vec![];
            //.                        title.push(Inline::Text(e.title_prefix.orig_text()));
            //.                        for c in e.title.unwrap_or_default() {
            //.                            title.push(translate_ast(c));
            //.                        }
            //.                        let mut address = None;
            //.                        let mut incomplete = false;
            //.                        shed!{
            //.                            let Some(cont) = e.continuation else {
            //.                                incomplete = true;
            //.                                break;
            //.                            };
            //.                            title.push(Inline::Text(cont.title_suffix.orig_text()));
            //.                            let Some(addr) = cont.address else {
            //.                                incomplete = true;
            //.                                break;
            //.                            };
            //.                            let addr_suffix = if let Some(suffix) = addr.suffix {
            //.                                suffix.orig_text()
            //.                            } else {
            //.                                incomplete = true;
            //.                                "".to_string()
            //.                            };
            //.                            address = Some(InlineLinkAddress {
            //.                                prefix: addr.prefix.orig_text(),
            //.                                address: {
            //.                                    let mut out = String::new();
            //.                                    for c in addr.address.unwrap_or_default() {
            //.                                        match c {
            //.                                            grammar::inline_actions::LinkAddressChar::LinkAddressCharT(
            //.                                                c,
            //.                                            ) => out.push_str(
            //.                                                &c,
            //.                                            ),
            //.                                            grammar::inline_actions::LinkAddressChar::EscapedChar(c) => {
            //.                                                out.push_str(&c.prefix.orig_text());
            //.                                                out.push_str(&c.text);
            //.                                            },
            //.                                        }
            //.                                    }
            //.                                    out
            //.                                },
            //.                                suffix: addr_suffix,
            //.                            });
            //.                        };
            //.                        return Inline::Link(InlineLink {
            //.                            incomplete: incomplete,
            //.                            title: title,
            //.                            address: address,
            //.                        });
            //.                    },
            //.                    grammar::inline_actions::InlineEl::Code(e) => {
            //.                        return Inline::Code(InlineCode {
            //.                            incomplete: e.suffix.is_none(),
            //.                            text: {
            //.                                let mut out = String::new();
            //.                                for c in e.text.unwrap_or_default() {
            //.                                    match c {
            //.                                        grammar::inline_actions::CodeChar::CodeCharT(c) => out.push_str(&c),
            //.                                        grammar::inline_actions::CodeChar::EscapedChar(c) => {
            //.                                            out.push_str(&c.prefix.orig_text());
            //.                                            out.push_str(&c.text);
            //.                                        },
            //.                                    }
            //.                                }
            //.                                out
            //.                            },
            //.                        });
            //.                    },
            //.                    grammar::inline_actions::InlineEl::Text(e) => {
            //.                        let mut out = String::new();
            //.                        for c in e.text {
            //.                            match c {
            //.                                grammar::inline_actions::TextChar::TextCharT(c) => out.push_str(&c),
            //.                                grammar::inline_actions::TextChar::EscapedChar(c) => {
            //.                                    out.push_str(&c.prefix.orig_text());
            //.                                    out.push_str(&c.text);
            //.                                },
            //.                            }
            //.                        }
            //.                        return Inline::Text(out);
            //.                    },
            //.                }
            //.            }
            //. 
            //.             let mut inline = vec![];
            //.           for e in ast {
            //.           inline.push(translate_ast(e));
            //.     }
            eprintln!("syncing line, cursor {:?}", cursor);
            let inline = vec![Inline::Text(text)];
            {
                let offset = Cell::new(0usize);
                sync_shadow_dom(
                    &mut cursor,
                    &offset,
                    &line,
                    generate_line_shadowdom(line_type, &mut indents, inline),
                );
            }
        }

        // Update indents
        {
            let children = line.children();
            for i in 0 .. children.length() {
                let child = children.item(i).unwrap();
                let Some(id) = child.get_attribute(ATTR_INDENT_ID) else {
                    break;
                };
                let id = usize::from_str_radix(&id, 10).unwrap();
                let indent_block =
                    document().get_element_by_id(&css_id_block(id)).unwrap().dyn_into::<Element>().unwrap();
                let width = child.client_width() as f32;
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
                    let style = child.dyn_into::<HtmlElement>().unwrap().style();
                    let want_prop =
                        format!(
                            "{}px",
                            indent_block.get_bounding_client_rect().left() - root.get_bounding_client_rect().left()
                        );
                    if style.get_property_value("left").unwrap() != want_prop {
                        style.set_property("left", &want_prop).unwrap();
                    }
                }
            }
        }

        // Continue until nothing changes
        if !changed {
            break;
        }
        let Some(at1) = get_line_forward(&line, MoveMode::Exclusive) else {
            break;
        };
        context_line = Some(line);
        line = at1;
    }
}

fn main() {
    console_error_panic_hook::set_once();
    let ids = Rc::new(RefCell::new(0usize));

    // Create initial tree
    let root = el("div").classes(&[NAMESPACE, CLASS_ROOT]).attr("contenteditable", "true");
    set_root(vec![root.clone()]);
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
                    for child in block {
                        children.push(
                            generate_shadow_dom(
                                &Cell::new(0usize),
                                generate_line_shadowdom(LineType::Code, &mut ctx.indents, vec![Inline::Text(child)]),
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
        for block in [
            Block::Line(vec![Inline::Text("Hello".to_string())]),
            Block::Ul(
                vec![
                    vec![Block::Line(vec![Inline::Text("World 1".to_string())])],
                    vec![
                        Block::Line(vec![Inline::Text("World 2".to_string())]),
                        Block::Line(vec![Inline::Text("World 3".to_string())])
                    ]
                ],
            ),
        ] {
            recursive_generate_block(&mut ctx, &mut root_children, block);
        }
        root.ref_extend(root_children.into_iter().map(el_from_raw).collect());
        let indent_blocks = document().get_elements_by_class_name(CLASS_BLOCK_INDENT);
        for i in 0 .. indent_blocks.length() {
            let indent_block = indent_blocks.item(i).unwrap();
            let id = usize::from_str_radix(&indent_block.get_attribute(ATTR_ID).unwrap(), 10).unwrap();
            let aligned = get_aligned(id);
            let max_width = get_aligned_max_width(&aligned);
            set_indent(&root.raw(), indent_block, &aligned, max_width, false);
        }
    }

    // Setup change handler
    root.ref_own(|self1| {
        let observer = MutationObserver::new(&Closure::<dyn Fn(Array) -> ()>::new({
            struct Change {
                element: Element,
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
                                    if classes.contains(CLASS_LINE) {
                                        break 'found at_temp.clone();
                                    }
                                    if classes.contains(CLASS_BLOCK_INDENT) {
                                        break;
                                    }
                                    if classes.contains(CLASS_ROOT) {
                                        break;
                                    }
                                }
                                let Some(at_temp) = at.parent_node() else {
                                    break;
                                };
                                at = at_temp;
                            }
                            // Not within a line, so some child of a block? So find the next line to start from
                            let Some(line) = get_line_forward(&start, MoveMode::Inclusive) else {
                                continue 'next_mutation;
                            };
                            break 'found line;
                        };
                        delay_mut.changes.push(Change { element: line });
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
                                let el_js_value = JsValue::from(&change.element);
                                if seen.has(&el_js_value) {
                                    continue;
                                }
                                seen.add(&el_js_value);
                                update_lines_starting_at(&mut ctx, change.element);
                                eprintln!("change end");
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
}
