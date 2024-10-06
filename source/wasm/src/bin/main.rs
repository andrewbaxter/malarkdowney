use {
    const_format::formatcp,
    gloo::{
        console::{
            console,
            console_dbg,
        },
        timers::callback::Timeout,
        utils::{
            document,
            document_element,
            window,
        },
    },
    js_sys::Array,
    rooting::{
        el,
        el_from_raw,
        set_root,
    },
    std::{
        borrow::Cow,
        cell::RefCell,
        collections::HashSet,
        rc::Rc,
    },
    wasm_bindgen::{
        closure::Closure,
        convert::{
            IntoWasmAbi,
            ReturnWasmAbi,
        },
        JsCast,
        JsValue,
    },
    web_sys::{
        console,
        CharacterData,
        Element,
        HtmlElement,
        MutationObserver,
        MutationObserverInit,
        MutationRecord,
        Node,
    },
};

const NAMESPACE: &str = "malarkey";
const ATTR_ID: &str = formatcp!("{}_id", NAMESPACE);
const ATTR_INDENT_ID: &str = formatcp!("{}_indent_id", NAMESPACE);
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
    pub title: Vec<Inline>,
    pub address: String,
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
    Line(Vec<Inline>),
    Quote(Vec<Block>),
    Code(Vec<String>),
    Ul(Vec<Vec<Block>>),
    Ol(Vec<Vec<Block>>),
}

fn create_element(t: &str) -> Element {
    let out = document().create_element(t).unwrap();
    out.class_list().add_1(CLASS_NAMESPACE).unwrap();
    return out;
}

fn generate_inline(e: Inline) -> Node {
    match e {
        Inline::Text(e) => {
            return Node::from(document().create_text_node(&e));
        },
        Inline::Strong(e) => {
            let out = create_element("strong");
            if e.incomplete {
                out.class_list().add_1(CLASS_INCOMPLETE).unwrap();
            }
            for e in e.children {
                out.append_child(&generate_inline(e)).unwrap();
            }
            return Node::from(out);
        },
        Inline::Emphasis(e) => {
            let out = create_element("emphasis");
            if e.incomplete {
                out.class_list().add_1(CLASS_INCOMPLETE).unwrap();
            }
            for e in e.children {
                out.append_child(&generate_inline(e)).unwrap();
            }
            return Node::from(out);
        },
        Inline::Link(e) => {
            let out = create_element("a");
            if e.incomplete {
                out.class_list().add_1(CLASS_INCOMPLETE).unwrap();
            }
            out.set_attribute("href", &e.address).unwrap();
            for e in e.title {
                out.append_child(&generate_inline(e)).unwrap();
            }
            return Node::from(out);
        },
        Inline::Code(e) => {
            let out = create_element("code");
            if e.incomplete {
                out.class_list().add_1(CLASS_INCOMPLETE).unwrap();
            }
            out.set_text_content(Some(&e.text));
            return Node::from(out);
        },
    }
}

fn get_indent_block(id: usize) -> HtmlElement {
    return document().get_element_by_id(&css_id_block(id)).unwrap().dyn_into::<HtmlElement>().unwrap();
}

fn get_aligned(id: usize) -> Vec<HtmlElement> {
    let mut out = vec![];
    let aligned = document().get_elements_by_class_name(&class_aligned(id));
    for i in 0 .. aligned.length() {
        out.push(aligned.item(i).unwrap().dyn_into::<HtmlElement>().unwrap());
    }
    return out;
}

fn get_padding_left(e: &HtmlElement) -> f32 {
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

fn set_alignments_to_value(indent_block: HtmlElement, aligned: &Vec<HtmlElement>, width: f32) {
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

    fn recurse(root: &HtmlElement, indent_block: &HtmlElement, aligned: Option<&Vec<HtmlElement>>) {
        let right = indent_block.client_left() as f32 + get_padding_left(indent_block) - root.client_left() as f32;
        let aligned = match aligned {
            Some(a) => Cow::Borrowed(a),
            None => {
                let id = indent_block.get_attribute(ATTR_ID).unwrap().parse::<usize>().unwrap();
                Cow::Owned(get_aligned(id))
            },
        };
        for e in aligned.as_ref() {
            let style = e.style();
            style.set_property("left", &format!("{}px", right)).unwrap();
        }
        let children = indent_block.children();
        for i in 0 .. children.length() {
            let Ok(child) = children.item(i).unwrap().dyn_into::<HtmlElement>() else {
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

fn get_aligned_max_width(aligned: &Vec<HtmlElement>) -> f32 {
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

fn main() {
    console_error_panic_hook::set_once();

    // Create root
    let e_root = el("div").classes(&[NAMESPACE, CLASS_ROOT]).attr("contenteditable", "true");
    set_root(vec![e_root.clone()]);

    // Setup change handler
    e_root.ref_own(|self1| {
        let observer = MutationObserver::new(&Closure::<dyn Fn(Array) -> ()>::new({
            enum Change {
                Add(HtmlElement),
                Delete(HtmlElement),
                Text(HtmlElement),
            }

            struct DelayState {
                delay: Option<Timeout>,
                changes: Vec<Change>,
            }

            let delay = Rc::new(RefCell::new(DelayState {
                delay: None,
                changes: vec![],
            }));
            move |mutations: Array| {
                for m in mutations {
                    let m = MutationRecord::from(m);
                    let mut delay_mut = delay.borrow_mut();
                    match m.type_().as_str() {
                        "childList" => {
                            let removed = m.removed_nodes();
                            for i in 0 .. removed.length() {
                                delay_mut
                                    .changes
                                    .push(Change::Delete(removed.item(i).unwrap().dyn_into().unwrap()));
                            }
                            let added = m.added_nodes();
                            for i in 0 .. added.length() {
                                delay_mut.changes.push(Change::Add(added.item(i).unwrap().dyn_into().unwrap()));
                            }
                        },
                        "characterData" => {
                            delay_mut
                                .changes
                                .push(
                                    Change::Text(
                                        JsValue::from(m.target().unwrap())
                                            .dyn_into::<CharacterData>()
                                            .unwrap()
                                            .parent_node()
                                            .unwrap()
                                            .dyn_into::<HtmlElement>()
                                            .unwrap(),
                                    ),
                                );
                        },
                        _ => panic!(),
                    }
                    delay_mut.delay = Some(Timeout::new(200, {
                        let delay = delay.clone();
                        move || {
                            let mut delay_mut = delay.borrow_mut();
                            delay_mut.delay = None;
                            let changes = delay_mut.changes.split_off(0);
                            drop(delay_mut);
                            let mut seen_text = HashSet::new();
                            for change in changes {
                                match change {
                                    Change::Add(e) => { },
                                    Change::Delete(e) => { },
                                    Change::Text(e) => {
                                        if !seen_text.insert(JsValue::from(e.clone()).as_ref().into_abi()) {
                                            continue;
                                        }
                                        let mut at = e;
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
        struct ChildAlignment {
            source_indent: usize,
            first_text: Option<String>,
            text: String,
        }

        struct GenerateContext {
            ids: usize,
            alignments: Vec<ChildAlignment>,
        }

        fn generate_el_block_indent(ctx: &mut GenerateContext) -> (usize, Element) {
            let d = document();
            let e = d.create_element("div").unwrap();
            e.class_list().add_1(CLASS_BLOCK_INDENT).unwrap();
            let id = ctx.ids;
            ctx.ids += 1;
            e.set_id(&css_id_block(id));
            e.set_attribute(ATTR_ID, &id.to_string()).unwrap();
            return (id, e);
        }

        fn generate_el_block_inline() -> Element {
            let d = document();
            let e = d.create_element("div").unwrap();
            return e;
        }

        fn generate_block(ctx: &mut GenerateContext, parent_container: &mut Vec<Element>, block: Block) {
            match block {
                Block::Line(block) => {
                    let e_line = generate_el_block_inline();
                    e_line.class_list().add_1(CLASS_LINE).unwrap();
                    let mut children = vec![];
                    for alignment in &mut ctx.alignments {
                        let e_aligned = create_element("span");
                        e_aligned.set_text_content(
                            Some(
                                alignment
                                    .first_text
                                    .take()
                                    .as_ref()
                                    .map(|x| x.as_str())
                                    .unwrap_or(alignment.text.as_str()),
                            ),
                        );
                        let observe_opts = MutationObserverInit::new();
                        observe_opts.set_character_data(true);
                        let e_aligned_outer = create_element("span");
                        e_aligned_outer
                            .class_list()
                            .add_2(CLASS_ALIGNED, &class_aligned(alignment.source_indent))
                            .unwrap();
                        e_aligned_outer.set_attribute(ATTR_INDENT_ID, &alignment.source_indent.to_string()).unwrap();
                        e_aligned_outer.append_child(&e_aligned).unwrap();
                        children.push(Node::from(e_aligned_outer));
                    }
                    for span in block {
                        children.push(generate_inline(span));
                    }
                    e_line.append_with_node(&children.into_iter().map(|e| JsValue::from(e)).collect()).unwrap();
                    parent_container.push(e_line);
                },
                Block::Code(block) => {
                    let (_, e_block) = generate_el_block_indent(ctx);
                    e_block.class_list().add_1(CLASS_BLOCKCODE).unwrap();
                    let mut children = vec![];
                    for child in block {
                        let e_line = generate_el_block_inline();
                        e_line.class_list().add_1(CLASS_LINE).unwrap();
                        e_line.set_text_content(Some(&child));
                        children.push(e_line);
                    }
                    e_block.append_with_node(&children.into_iter().map(|e| JsValue::from(e)).collect()).unwrap();
                    parent_container.push(e_block);
                },
                Block::Quote(block) => {
                    let (id, e_block) = generate_el_block_indent(ctx);
                    e_block.class_list().add_1(CLASS_BLOCKQUOTE).unwrap();
                    ctx.alignments.push(ChildAlignment {
                        source_indent: id,
                        first_text: None,
                        text: "> ".to_string(),
                    });
                    let mut children = vec![];
                    for child in block {
                        generate_block(ctx, &mut children, child);
                    }
                    e_block.append_with_node(&children.into_iter().map(|e| JsValue::from(e)).collect()).unwrap();
                    ctx.alignments.pop();
                    parent_container.push(e_block);
                },
                Block::Ul(block) => {
                    let (id, e_block) = generate_el_block_indent(ctx);
                    e_block.class_list().add_1(CLASS_UL).unwrap();
                    let mut children = vec![];
                    for bullet in block {
                        ctx.alignments.push(ChildAlignment {
                            source_indent: id,
                            first_text: Some("* ".to_string()),
                            text: "  ".to_string(),
                        });
                        for child in bullet {
                            generate_block(ctx, &mut children, child);
                        }
                        ctx.alignments.pop();
                    }
                    e_block.append_with_node(&children.into_iter().map(|e| JsValue::from(e)).collect()).unwrap();
                    parent_container.push(e_block);
                },
                Block::Ol(block) => {
                    let (id, e_block) = generate_el_block_indent(ctx);
                    e_block.class_list().add_1(CLASS_OL).unwrap();
                    let mut children = vec![];
                    for (i, bullet) in block.into_iter().enumerate() {
                        ctx.alignments.push(ChildAlignment {
                            source_indent: id,
                            first_text: Some(format!("{}. ", i)),
                            text: "   ".to_string(),
                        });
                        for child in bullet {
                            generate_block(ctx, &mut children, child);
                        }
                        ctx.alignments.pop();
                    }
                    e_block.append_with_node(&children.into_iter().map(|e| JsValue::from(e)).collect()).unwrap();
                    parent_container.push(e_block);
                },
            }
        }

        let mut root_children = vec![];
        let mut ctx = GenerateContext {
            ids: 0usize,
            alignments: vec![],
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
            generate_block(&mut ctx, &mut root_children, block);
        }
        e_root.ref_extend(root_children.into_iter().map(el_from_raw).collect());
        for id in 0 .. ctx.ids {
            let aligned = get_aligned(id);
            let max_width = get_aligned_max_width(&aligned);
            let indent_block = get_indent_block(id);

            //. eprintln!("initial alignment; id {}, aligned count {}, max_width {}", id, aligned.len(), max_width);
            set_alignments_to_value(indent_block, &aligned, max_width);
        }
    }
}
