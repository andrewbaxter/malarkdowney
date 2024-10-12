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
    js_sys::Array,
    rooting::{
        el,
        el_from_raw,
        set_root,
    },
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
        mem::swap,
        rc::Rc,
        str::FromStr,
    },
    structre::structre,
    wasm_bindgen::{
        closure::Closure,
        convert::IntoWasmAbi,
        JsCast,
        JsValue,
    },
    web_sys::{
        CharacterData,
        Element,
        HtmlElement,
        MutationObserver,
        MutationObserverInit,
        MutationRecord,
        Node,
        NodeList,
        Text,
    },
};

mod grammar {
    mod inline {
        include!(concat!(env!("OUT_DIR"), "/inline/inline.rs"));
    }

    mod inline_actions {
        include!(concat!(env!("OUT_DIR"), "/inline/inline_actions.rs"));
    }
}

const NAMESPACE: &str = "malarkey";
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
const PREFIX_OL: &str = "  ";
const PREFIX_BLOCKQUOTE: &str = "> ";
const PREFIX_BLOCKCODE: &str = "```";

#[derive(Serialize, Deserialize)]
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
    // `[`
    pub prefix: String,
    pub title: Vec<Inline>,
    // `](`
    pub infix: String,
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

enum ShadowDom {
    Text(String),
    Element(ShadowDomElement),
}

struct ShadowDomElement {
    tag: &'static str,
    classes: HashSet<String>,
    attrs: HashMap<String, String>,
    children: Vec<ShadowDom>,
}

fn sync_shadow_dom(cursor: &mut Option<usize>, offset: &Cell<usize>, have: &Node, want: ShadowDom) {
    fn replace(old: &Node, new: &Node) {
        old.parent_node().unwrap().replace_child(old, new).unwrap();
    }

    let mut sync_cursor = |node: &Node| {
        if let Some(cursor) = cursor.take_if(|cursor| offset.get() < *cursor) {
            let rel_offset = cursor - offset.get();
            let sel = window().get_selection().unwrap().unwrap();
            sel.remove_all_ranges().unwrap();
            let range = document().create_range().unwrap();
            range.set_start(node, rel_offset as u32).unwrap();
            range.set_end(node, rel_offset as u32).unwrap();
            sel.add_range(&range).unwrap();
        }
    };
    match want {
        ShadowDom::Text(want) => {
            let have = match have.dyn_ref::<Text>() {
                Some(have) => {
                    if have.text_content().unwrap() != want {
                        have.set_text_content(Some(&want));
                    }
                    have.clone()
                },
                None => {
                    let new = document().create_text_node(&want);
                    replace(have, &new);
                    new
                },
            };
            sync_cursor(&have);
            offset.set(offset.get() + want.len());
        },
        ShadowDom::Element(want) => {
            let have = superif!({
                let Some(have) = have.dyn_ref::<Element>() else {
                    break 'mismatch;
                };
                if have.tag_name() != want.tag {
                    break 'mismatch;
                }
                break have.clone();
            } 'mismatch {
                let new = document().create_element(want.tag).unwrap();
                replace(have, &new);
                break new;
            });

            // Sync attrs
            let have_attrs = have.get_attribute_names();
            let mut remove_attrs = vec![];
            for k in have_attrs {
                let k = k.as_string().unwrap();
                if !want.attrs.contains_key(&k) {
                    remove_attrs.push(k);
                }
            }
            for k in remove_attrs {
                have.remove_attribute(&k).unwrap();
            }
            for (k, v) in want.attrs {
                have.set_attribute(&k, &v).unwrap();
            }

            // Sync classes
            let classes = have.class_list();
            let mut remove_classes = vec![];
            for k in classes.entries() {
                let k = k.unwrap().as_string().unwrap();
                if !want.classes.contains(&k) {
                    remove_classes.push(k);
                }
            }
            for k in remove_classes {
                classes.remove_1(&k).unwrap();
            }
            for k in want.classes {
                classes.add_1(&k).unwrap();
            }

            // Sync children
            let have_children = have.child_nodes();
            let have_children_len = have_children.length() as usize;
            let mut want_children_iter = want.children.into_iter();
            let mut i = 0;
            while i < have_children_len {
                let Some(want_child) = want_children_iter.next() else {
                    break;
                };
                let have_child = have_children.item(i as u32).unwrap();
                sync_shadow_dom(cursor, offset, &have_child, want_child);
                i += 1;
            }
            for _ in i .. have_children_len {
                have.remove_child(&have.last_child().unwrap()).unwrap();
            }
            for want_child in want_children_iter {
                let new = match &want_child {
                    ShadowDom::Text(want_child) => {
                        document().create_text_node(&want_child).dyn_into::<Node>().unwrap()
                    },
                    ShadowDom::Element(want_child) => {
                        document().create_element(&want_child.tag).unwrap().dyn_into::<Node>().unwrap()
                    },
                };
                sync_shadow_dom(cursor, offset, &new, want_child);
                have.append_child(&new).unwrap();
            }
        },
    }
}

fn sync_line(
    line: &Element,
    mut cursor: Option<usize>,
    line_type: LineType,
    indents: &mut [LineIndent],
    inlines: Vec<Inline>,
) {
    let mut want_children = vec![];
    for indent in indents {
        want_children.push(ShadowDom::Element(ShadowDomElement {
            tag: "span",
            classes: [CLASS_NAMESPACE.to_string(), CLASS_ALIGNED.to_string(), class_aligned(indent.source_indent)]
                .into_iter()
                .collect(),
            attrs: [(ATTR_INDENT_ID.to_string(), indent.source_indent.to_string())].into_iter().collect(),
            children: vec![ShadowDom::Element(ShadowDomElement {
                tag: "span",
                classes: [CLASS_NAMESPACE.to_string()].into_iter().collect(),
                attrs: Default::default(),
                children: vec![ShadowDom::Text(indent.first_text.take().unwrap_or_else(|| indent.text.clone()))],
            })],
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
                        out.push(ShadowDom::Text(want.prefix));
                        for c in want.title {
                            out.push(generate_inline(c));
                        }
                        out.push(ShadowDom::Text(want.infix));
                        out.push(ShadowDom::Element(ShadowDomElement {
                            tag: "a",
                            classes: [CLASS_NAMESPACE.to_string()].into_iter().collect(),
                            attrs: {
                                let mut out = HashMap::new();
                                out.insert("href".to_string(), want.address.clone());
                                out
                            },
                            children: vec![ShadowDom::Text(want.address)],
                        }));
                        out.push(ShadowDom::Text(want.suffix));
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
    let root = ShadowDom::Element(ShadowDomElement {
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
        classes: [CLASS_NAMESPACE.to_string()].into_iter().collect(),
        attrs: Default::default(),
        children: want_children,
    });
    let offset = Cell::new(0usize);
    sync_shadow_dom(&mut cursor, &offset, &line, root);
}

fn get_indent_block(id: usize) -> Element {
    return document().get_element_by_id(&css_id_block(id)).unwrap().dyn_into::<Element>().unwrap();
}

fn get_aligned(id: usize) -> Vec<Element> {
    let mut out = vec![];
    let aligned = document().get_elements_by_class_name(&class_aligned(id));
    for i in 0 .. aligned.length() {
        out.push(aligned.item(i).unwrap().dyn_into::<Element>().unwrap());
    }
    return out;
}

fn get_padding_left(e: &Element) -> f32 {
    return window()
        .get_computed_style(e)
        .unwrap()
        .unwrap()
        .get_property_value("padding-left")
        .unwrap()
        .strip_suffix("px")
        .unwrap()
        .parse::<f32>()
        .unwrap();
}

fn set_alignments_to_value(indent_block: Element, aligned: &Vec<Element>, width: f32) {
    let indent_block = indent_block.dyn_into::<HtmlElement>().unwrap();

    // Update widths
    indent_block.style().set_property("padding-left", &format!("{}px", width)).unwrap();

    // Recursively update alignments for child lines
    let root = {
        let mut at = indent_block.clone();
        loop {
            at = at.parent_element().unwrap().dyn_into::<HtmlElement>().unwrap();
            if at.class_list().contains(CLASS_ROOT) {
                break at;
            }
        }
    };

    fn recurse(root: &Element, indent_block: &Element, aligned: Option<&Vec<Element>>) {
        let right = indent_block.client_left() as f32 + get_padding_left(indent_block) - root.client_left() as f32;
        let aligned = match aligned {
            Some(a) => Cow::Borrowed(a),
            None => {
                let id = indent_block.get_attribute(ATTR_ID).unwrap().parse::<usize>().unwrap();
                Cow::Owned(get_aligned(id))
            },
        };
        for e in aligned.as_ref() {
            let style = e.dyn_ref::<HtmlElement>().unwrap().style();
            style.set_property("left", &format!("{}px", right)).unwrap();
        }
        let children = indent_block.children();
        for i in 0 .. children.length() {
            let Ok(child) = children.item(i).unwrap().dyn_into::<Element>() else {
                continue;
            };
            if child.class_list().contains(CLASS_BLOCK_INDENT) {
                recurse(root, &child, None);
            }
        }
    }

    recurse(&root, &indent_block, Some(aligned));
}

macro_rules! eprintln{
    ($pat: literal, $($data: expr), *) => {
        console::log_1(&JsValue::from_str(&format!($pat, $($data,) *)));
    };
}

fn get_aligned_max_width(aligned: &Vec<Element>) -> f32 {
    let mut max_width = 0.;
    for e in aligned {
        // Get inner text that actually has width
        let e = e.first_element_child().unwrap();
        let width = e.client_width() as f32;
        if width > max_width {
            max_width = width;
        }
    }
    return max_width;
}

fn get_previous_line(n: &Element) -> Option<Element> {
    let mut at = n.clone();
    loop {
        loop {
            // Move backward
            {
                let Some(at1) = n.previous_element_sibling() else {
                    break;
                };
                at = at1;
            }

            // Match or move down
            loop {
                let class_list = at.class_list();

                // Match
                if class_list.contains(CLASS_LINE) {
                    return Some(at.dyn_into().unwrap());
                }

                // Or move down
                if !class_list.contains(CLASS_BLOCK_INDENT) {
                    break;
                }
                let Some(at1) = at.last_element_child() else {
                    break;
                };
                at = at1;
            }
        }

        // Move up
        {
            let Some(at1) = at.parent_element() else {
                return None;
            };
            at = at1;
        }
    }
}

fn get_next_line(n: &Element) -> Option<Element> {
    let mut at = n.clone();
    loop {
        loop {
            // Move forward
            {
                let Some(at1) = n.next_element_sibling() else {
                    break;
                };
                at = at1;
            }

            // Match or move down
            loop {
                let class_list = at.class_list();

                // Match
                if class_list.contains(CLASS_LINE) {
                    return Some(at.dyn_into().unwrap());
                }

                // Or move down
                if !class_list.contains(CLASS_BLOCK_INDENT) {
                    break;
                }
                let Some(at1) = at.first_element_child() else {
                    break;
                };
                at = at1;
            }
        }

        // Move up
        {
            let Some(at1) = at.parent_element() else {
                return None;
            };
            at = at1;
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

fn heading_el_name(level: usize) -> &'static str {
    return match level {
        1 => "h1",
        2 => "h2",
        3 => "h3",
        4 => "h4",
        5 => "h5",
        6 => "h6",
        _ => panic!(),
    };
}

fn generate_el_block_line(type_: LineType) -> Element {
    let d = document();
    match type_ {
        LineType::Normal => {
            return d.create_element("p").unwrap();
        },
        LineType::Heading(level) => {
            return d.create_element(&heading_el_name(level)).unwrap();
        },
        LineType::Code => {
            return d.create_element("div").unwrap();
        },
    }
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

fn update_lines_starting_at(ids: &Rc<RefCell<usize>>, line: Element) {
    let mut at = line.clone();
    loop {
        let mut changed = false;

        // Determine context by walking previous line's block parents up to root
        let mut indents = vec![];
        let root;
        if let Some(previous) = get_previous_line(&at) {
            let mut context_at = previous.parent_element().unwrap();
            let mut parent_index =
                Array::index_of(&Array::from(&JsValue::from(context_at.children())), &JsValue::from(previous), 0);
            while !context_at.class_list().contains(CLASS_ROOT) {
                let id = context_at.get_attribute(ATTR_ID).unwrap().parse::<usize>().unwrap();
                indents.push(((context_at.clone(), parent_index as usize), LineIndent {
                    type_: serde_json::from_str::<BlockType>(
                        &context_at.get_attribute(ATTR_BLOCK_TYPE).unwrap(),
                    ).unwrap(),
                    source_indent: id,
                    first_text: None,
                    text: "".to_string(),
                }));
                let parent = context_at.parent_element().unwrap();
                parent_index =
                    Array::index_of(
                        &Array::from(&JsValue::from(parent.children())),
                        &JsValue::from(context_at),
                        0,
                    );
                context_at = parent;
            }
            root = context_at;
        } else {
            let mut context_ = line.clone();
            while !context_.class_list().contains(CLASS_ROOT) {
                context_ = context_.parent_element().unwrap();
            }
            root = context_;
        }
        indents.reverse();

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
                break (Some((n.child_nodes().item(sel.anchor_offset()).unwrap(), sel.anchor_offset() as usize)));
            } else {
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
        recurse_get_text(&mut cursor, &mut text, &sel, at.child_nodes());

        // Check how much we need to deindent
        let mut place_root = root;
        for ((indent_el, indent_el_index), indent) in &mut indents {
            match &mut indent.type_ {
                BlockType::BlockQuote => {
                    let Some(suffix1) = text.strip_prefix(PREFIX_BLOCKQUOTE) else {
                        break;
                    };
                    text = suffix1.to_string();
                    indent.text = PREFIX_BLOCKQUOTE.to_string();
                },
                BlockType::Ul => {
                    if *indent_el_index == 0 {
                        let Some(suffix1) = text.strip_prefix(PREFIX_UL_FIRST) else {
                            break;
                        };
                        text = suffix1.to_string();
                        indent.first_text = Some(PREFIX_UL_FIRST.to_string());
                        indent.text = PREFIX_UL.to_string();
                    } else {
                        let Some(suffix1) = text.strip_prefix(PREFIX_UL) else {
                            break;
                        };
                        text = suffix1.to_string();
                        indent.text = PREFIX_UL.to_string();
                    }
                },
                BlockType::Ol => {
                    if *indent_el_index == 0 {
                        let Ok(parsed) = ReOlNumberPrefix::from_str(&text) else {
                            break;
                        };
                        text = parsed.suffix;
                        indent.first_text = Some(parsed.number);
                        indent.text = PREFIX_OL.to_string();
                    } else {
                        let Some(suffix1) = text.strip_prefix(PREFIX_OL) else {
                            break;
                        };
                        text = suffix1.to_string();
                        indent.text = PREFIX_OL.to_string();
                    }
                },
                BlockType::BlockCode => unreachable!(),
            }
            place_root = indent_el.clone();
        }

        // Check if starting new prefixed block
        let mut create_parents = vec![];
        loop {
            if let Some(suffix1) = text.strip_prefix(PREFIX_BLOCKQUOTE) {
                text = suffix1.to_string();
                create_parents.push(BlockType::BlockQuote);
            } else if let Some(suffix1) = text.strip_prefix(PREFIX_UL_FIRST) {
                text = suffix1.to_string();
                create_parents.push(BlockType::Ul);
            } else if let Ok(parsed) = ReOlNumberPrefix::from_str(&text) {
                text = parsed.suffix;
                create_parents.push(BlockType::Ol);
            } else if let Some(suffix1) = text.strip_prefix(PREFIX_BLOCKCODE) {
                text = suffix1.to_string();
                create_parents.push(BlockType::BlockCode);
                break;
            } else {
                break;
            }
        }

        // * Lift up this line and following elements that are between this element and the
        //   placement root.
        //
        // * Find the following element in the placement root.
        let mut lifted_following = vec![];
        let mut place_before = None;
        let original_parent = at.parent_element().unwrap();
        shed!{
            let Some(mut lift_at) = at.next_element_sibling() else {
                break;
            };
            at.remove();
            'lift_end : loop {
                if lift_at.parent_element().unwrap() == place_root {
                    place_before = Some(lift_at);
                    break 'lift_end;
                }
                let mut lift_at1 = lift_at.clone();
                loop {
                    if let Some(lift_at1b) = lift_at1.next_element_sibling() {
                        lift_at1 = lift_at1b;
                        break;
                    }
                    lift_at1 = lift_at1.parent_element().unwrap();
                    if lift_at1.parent_element().unwrap() == place_root {
                        place_before = Some(lift_at.clone());
                        break 'lift_end;
                    }
                };
                changed = true;
                lift_at.remove();
                lifted_following.push(lift_at);
                lift_at = lift_at1;
            }
        }

        // (Clean up empty left over blocks/containers)
        let mut clean_up_at = original_parent;
        let mut clean_up_until = None;
        loop {
            if clean_up_at == place_root {
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
            if place_root != at.parent_element().unwrap() {
                if let Some(place_before) = &place_before {
                    place_root.insert_before(&place_before, Some(&at)).unwrap();
                } else {
                    place_root.append_child(&at).unwrap();
                }

                // Changed - current line placed elsewhere
                changed = true;
            }
        } else {
            let mut place_parent = place_root.clone();
            for create_parent in create_parents {
                let new_parent = match create_parent {
                    BlockType::BlockQuote => generate_el_block_indent(
                        &mut ids.borrow_mut(),
                        BlockType::BlockQuote,
                        CLASS_BLOCKQUOTE,
                    ).1,
                    BlockType::Ul => generate_el_block_indent(&mut ids.borrow_mut(), BlockType::Ul, CLASS_UL).1,
                    BlockType::Ol => generate_el_block_indent(&mut ids.borrow_mut(), BlockType::Ol, CLASS_OL).1,
                    BlockType::BlockCode => generate_el_block_indent(
                        &mut ids.borrow_mut(),
                        BlockType::BlockCode,
                        CLASS_BLOCKCODE,
                    ).1,
                };
                if let Some(place_before) = &place_before {
                    place_root.insert_before(&place_before, Some(&new_parent)).unwrap();
                } else {
                    place_root.append_child(&new_parent).unwrap();
                }
                place_parent = new_parent;
            }
            place_parent.append_child(&at).unwrap();

            // Changed - current line placed in new block
            changed = true;
        }

        // Place remaining lines
        for e in lifted_following {
            if let Some(place_before) = &place_before {
                place_root.insert_before(&place_before, Some(&e)).unwrap();
            } else {
                place_root.append_child(&e).unwrap();
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
            type InlineTypeSig = fn(bool, Vec<Inline>) -> Inline;

            struct StackEl {
                child_type: InlineTypeSig,
                inline: Vec<Inline>,
            }

            struct InlineParseState {
                // Text that's not yet a node
                top_text: String,
                // Node children that aren't yet an element
                top_inline: Vec<Inline>,
                // Parent WIP elements plus meta about the current top/child element
                stack: Vec<StackEl>,
            }

            impl InlineParseState {
                fn push_element(&mut self, type_: InlineTypeSig) {
                    self.flush_text();
                    self.stack.push(StackEl {
                        child_type: type_,
                        inline: self.top_inline.split_off(0),
                    });
                }

                fn flush_element(&mut self, complete: bool) {
                    self.flush_text();
                    let parent = self.stack.pop().unwrap();
                    let mut inline = parent.inline;
                    swap(&mut inline, &mut self.top_inline);
                    self.top_inline.push((parent.child_type)(complete, inline));
                }

                fn flush_text(&mut self) {
                    if self.top_text.is_empty() {
                        return;
                    }
                    let mut text = String::new();
                    swap(&mut text, &mut self.top_text);
                    self.top_inline.push(Inline::Text(text));
                }

                fn top_type(&self) -> Option<InlineTypeSig> {
                    let Some(parent) = self.stack.last() else {
                        return None;
                    };
                    return Some(parent.child_type);
                }
            }

            fn type_strong(complete: bool, children: Vec<Inline>) -> Inline {
                return Inline::Strong(InlineStrong {
                    incomplete: !complete,
                    children: children,
                });
            }

            fn type_emphasis(complete: bool, children: Vec<Inline>) -> Inline {
                return Inline::Emphasis(InlineEmphasis {
                    incomplete: !complete,
                    children: children,
                });
            }

            fn type_code(complete: bool, children: Vec<Inline>) -> Inline {
                let mut iter = children.into_iter();
                let Some(Inline::Text(child)) = iter.next() else {
                    panic!();
                };
                if iter.next().is_some() {
                    panic!();
                }
                return Inline::Code(InlineCode {
                    incomplete: !complete,
                    text: child,
                });
            }

            let mut parse_state = InlineParseState {
                top_inline: vec![],
                top_text: "".to_string(),
                stack: vec![],
            };

            enum Mode {
                Normal,
                // Start token repeats
                Code(usize),
            }

            let mut mode = Mode::Normal;
            let mut escape = false;
            let mut iter = text.chars().enumerate();
            loop {
                let Some((i, c)) = iter.next() else {
                    break;
                };
                match mode {
                    Mode::Normal => {
                        if escape {
                            parse_state.top_text.push(c);
                            escape = false;
                        } else {
                            match c {
                                '*' => {
                                    if parse_state.top_type() == Some(type_strong) {
                                        parse_state.top_text.push(c);
                                        parse_state.flush_element(true);
                                    } else {
                                        parse_state.push_element(type_strong);
                                        parse_state.top_text.push(c);
                                    }
                                },
                                '_' => {
                                    if parse_state.top_type() == Some(type_emphasis) {
                                        parse_state.top_text.push(c);
                                        parse_state.flush_element(true);
                                    } else {
                                        parse_state.push_element(type_emphasis);
                                        parse_state.top_text.push(c);
                                    }
                                },
                                '`' => {
                                    parse_state.push_element(type_code);
                                    let mut count = 0;
                                    for c in text.chars().skip(i) {
                                        if c != '`' {
                                            break;
                                        }
                                        parse_state.top_text.push(c);
                                        count += 1;
                                    }
                                    for _ in 0 .. count - 1 {
                                        iter.next();
                                    }
                                    mode = Mode::Code(count);
                                },
                                '\\' => {
                                    parse_state.top_text.push(c);
                                    escape = true;
                                },
                                _ => {
                                    parse_state.top_text.push(c);
                                },
                            }
                        }
                    },
                    Mode::Code(start_count) => {
                        match c {
                            '`' => {
                                parse_state.top_text.push(c);
                                let mut count = 0;
                                for c in text.chars().skip(i) {
                                    if c != '`' {
                                        break;
                                    }
                                    count += 1;
                                }
                                if count >= start_count {
                                    for _ in 0 .. start_count - 1 {
                                        parse_state.top_text.push(iter.next().unwrap().1);
                                    }
                                    mode = Mode::Normal;
                                    parse_state.flush_element(true);
                                }
                            },
                            _ => {
                                parse_state.top_text.push(c);
                            },
                        }
                    },
                }
            }
            while !parse_state.stack.is_empty() {
                parse_state.flush_element(false);
            }
            parse_state.flush_text();
            sync_line(
                &at,
                cursor,
                line_type,
                &mut indents.into_iter().map(|(_, x)| x).collect::<Vec<_>>(),
                parse_state.top_inline,
            );
        }

        // Continue until nothing changes
        if !changed {
            break;
        }
        let Some(at1) = get_next_line(&line) else {
            break;
        };
        at = at1;
    }
}

fn main() {
    console_error_panic_hook::set_once();
    let ids = Rc::new(RefCell::new(0usize));

    // Create root
    let e_root = el("div").classes(&[NAMESPACE, CLASS_ROOT]).attr("contenteditable", "true");
    set_root(vec![e_root.clone()]);

    // Setup change handler
    e_root.ref_own(|self1| {
        let observer = MutationObserver::new(&Closure::<dyn Fn(Array) -> ()>::new({
            enum ChangeMode {
                Add,
                Delete,
                Text,
            }

            struct Change {
                element: Element,
                mode: ChangeMode,
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
                for m in mutations {
                    let m = MutationRecord::from(m);
                    let mut delay_mut = delay.borrow_mut();
                    match m.type_().as_str() {
                        "childList" => {
                            let removed = m.removed_nodes();
                            for i in 0 .. removed.length() {
                                let Some(next) =
                                    get_next_line(&removed.item(i).unwrap().dyn_into::<Element>().unwrap()) else {
                                        continue;
                                    };
                                delay_mut.changes.push(Change {
                                    element: next,
                                    mode: ChangeMode::Delete,
                                });
                            }
                            let added = m.added_nodes();
                            for i in 0 .. added.length() {
                                delay_mut.changes.push(Change {
                                    element: added.item(i).unwrap().dyn_into().unwrap(),
                                    mode: ChangeMode::Add,
                                });
                            }
                        },
                        "characterData" => {
                            delay_mut.changes.push(Change {
                                element: JsValue::from(m.target().unwrap())
                                    .dyn_into::<CharacterData>()
                                    .unwrap()
                                    .parent_node()
                                    .unwrap()
                                    .dyn_into::<Element>()
                                    .unwrap(),
                                mode: ChangeMode::Text,
                            });
                        },
                        _ => panic!(),
                    }
                    delay_mut.delay = Some(Timeout::new(200, {
                        let delay = delay.clone();
                        let ids = ids.clone();
                        move || {
                            let mut delay_mut = delay.borrow_mut();
                            delay_mut.delay = None;
                            let changes = delay_mut.changes.split_off(0);
                            drop(delay_mut);
                            let mut seen = HashSet::new();
                            for change in changes {
                                if !seen.insert(JsValue::from(change.element.clone()).as_ref().into_abi()) {
                                    continue;
                                }
                                match change.mode {
                                    ChangeMode::Add => {
                                        // Line created, process to fix styling
                                        //
                                        // * todo, carry over context (indents) as a convenience?
                                        update_lines_starting_at(&ids, change.element);
                                    },
                                    ChangeMode::Delete => {
                                        // Line removed, try updating next sibling in case context had changed
                                        update_lines_starting_at(&ids, change.element);
                                    },
                                    ChangeMode::Text => {
                                        let mut at = change.element;
                                        let mut at_class_list = at.class_list();
                                        loop {
                                            if at_class_list.contains(CLASS_ALIGNED) {
                                                // Sync alignments due to indentation changes (line removed, line added with
                                                // larger number)
                                                let id = usize::from_str_radix(&at.get_attribute(ATTR_INDENT_ID).unwrap(), 10).unwrap();
                                                let indent_block = get_indent_block(id);
                                                let width = at.first_element_child().unwrap().client_width() as f32;
                                                let set_width = get_padding_left(&indent_block);
                                                if width > set_width {
                                                    set_alignments_to_value(indent_block, &get_aligned(id), width);
                                                } else if width < set_width * 0.2 {
                                                    let aligned = get_aligned(id);
                                                    let max_width = get_aligned_max_width(&aligned);
                                                    if max_width < set_width * 0.9 {
                                                        set_alignments_to_value(indent_block, &aligned, max_width);
                                                    }
                                                }
                                                break;
                                            } else if at_class_list.contains(CLASS_LINE) {
                                                // Markdown changed - replace line, modify document structure, sync line contents
                                                update_lines_starting_at(&ids, at);
                                                break;
                                            } else {
                                                at = at.parent_element().unwrap().dyn_into().unwrap();
                                                at_class_list = at.class_list();
                                            }
                                        }
                                    },
                                }
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

    // Populate initial data
    {
        struct GenerateContext {
            ids: Rc<RefCell<usize>>,
            indents: Vec<LineIndent>,
        }

        fn recursive_generate_block(ctx: &mut GenerateContext, parent_container: &mut Vec<Element>, block: Block) {
            match block {
                Block::Heading(level, block) => {
                    let e_line = generate_el_block_line(LineType::Heading(level));
                    e_line.class_list().add_1(CLASS_LINE).unwrap();
                    sync_line(&e_line, None, LineType::Heading(level), &mut ctx.indents, block);
                    parent_container.push(e_line);
                },
                Block::Line(block) => {
                    let e_line = generate_el_block_line(LineType::Normal);
                    e_line.class_list().add_1(CLASS_LINE).unwrap();
                    sync_line(&e_line, None, LineType::Normal, &mut ctx.indents, block);
                    parent_container.push(e_line);
                },
                Block::Code(block) => {
                    let (_, e_block) =
                        generate_el_block_indent(&mut ctx.ids.borrow_mut(), BlockType::BlockCode, CLASS_BLOCKCODE);
                    let mut children = vec![];
                    for child in block {
                        let e_line = generate_el_block_line(LineType::Code);
                        e_line.class_list().add_1(CLASS_LINE).unwrap();
                        e_line.set_text_content(Some(&child));
                        children.push(e_line);
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
            ids: ids,
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
        e_root.ref_extend(root_children.into_iter().map(el_from_raw).collect());
        for id in 0 .. *ctx.ids.borrow() {
            let aligned = get_aligned(id);
            let max_width = get_aligned_max_width(&aligned);
            let indent_block = get_indent_block(id);

            //. eprintln!("initial alignment; id {}, aligned count {}, max_width {}", id, aligned.len(), max_width);
            set_alignments_to_value(indent_block, &aligned, max_width);
        }
    }
}
