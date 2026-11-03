use {
    gloo::utils::{
        document,
        window,
    },
    wasm_bindgen::JsCast,
    web_sys::{
        DomRect,
        Node,
    },
};

pub fn utf16_len(s: &str) -> usize {
    return s.encode_utf16().count();
}

pub fn utf16_to_byte(s: &str, utf16_offset: usize) -> usize {
    let mut seen = 0;
    for (i, c) in s.char_indices() {
        if seen >= utf16_offset {
            return i;
        }
        seen += c.len_utf16();
    }
    return s.len();
}

pub fn node_text_len(n: &Node) -> usize {
    return n.text_content().map(|t| utf16_len(&t)).unwrap_or(0);
}

pub fn bounds(n: &Node) -> Option<DomRect> {
    let r = document().create_range().ok()?;
    r.select_node(n).ok()?;
    return Some(r.get_bounding_client_rect());
}

pub fn text_offset_in(container: &Node, node: &Node, offset: u32) -> Option<usize> {
    if !container.contains(Some(node)) {
        return None;
    }
    let r = document().create_range().ok()?;
    r.set_start(container, 0).ok()?;
    r.set_end(node, offset).ok()?;
    return Some(r.unchecked_ref::<js_sys::Object>().to_string().length() as usize);
}

fn text_nodes(at: &Node, out: &mut Vec<Node>) {
    if at.node_type() == Node::TEXT_NODE {
        out.push(at.clone());
        return;
    }
    let children = at.child_nodes();
    for i in 0 .. children.length() {
        if let Some(child) = children.item(i) {
            text_nodes(&child, out);
        }
    }
}

pub fn locate_text_offset(container: &Node, offset: usize) -> (Node, u32) {
    let mut nodes = vec![];
    text_nodes(container, &mut nodes);
    let mut seen = 0;
    let mut last = None;
    for n in nodes {
        let len = node_text_len(&n);
        if len == 0 {
            continue;
        }
        if offset < seen + len {
            return (n, (offset - seen) as u32);
        }
        seen += len;
        last = Some((n, len as u32));
    }
    if let Some(last) = last {
        return last;
    }
    return (container.clone(), 0);
}

pub fn select(n: &Node, rel_offset: usize) {
    select_range(n, rel_offset as u32, n, rel_offset as u32);
}

pub fn select_range(anchor: &Node, anchor_offset: u32, focus: &Node, focus_offset: u32) {
    let Ok(Some(sel)) = window().get_selection() else {
        return;
    };
    _ = sel.set_base_and_extent(anchor, anchor_offset, focus, focus_offset);
}

#[derive(PartialEq, Debug)]
pub enum ScanInclusivity {
    Inclusive,
    Exclusive,
}

#[derive(PartialEq, Debug)]
pub enum ScanDirection {
    Forward,
    Backward,
}

pub fn scan(
    start: &Node,
    root_predicate: fn(&Node) -> bool,
    match_predicate: fn(&Node) -> bool,
    direction: ScanDirection,
    inclusivity: ScanInclusivity,
) -> Option<Node> {
    let mut skip_advance_once = inclusivity == ScanInclusivity::Inclusive;
    let fn_advance: fn(&Node) -> Option<Node>;
    let fn_descend: fn(&Node) -> Option<Node>;
    match direction {
        ScanDirection::Forward => {
            fn_advance = Node::next_sibling;
            fn_descend = Node::first_child;
        },
        ScanDirection::Backward => {
            fn_advance = Node::previous_sibling;
            fn_descend = Node::last_child;
        },
    }
    let mut at = start.clone();
    loop {
        loop {
            // Advance
            if skip_advance_once {
                skip_advance_once = false;
            } else {
                let Some(at_temp) = fn_advance(&at) else {
                    // (move up)
                    break;
                };
                at = at_temp;
            }

            // Match or move down
            loop {
                // Match
                if match_predicate(&at) {
                    return Some(at);
                }

                // Or move down
                let Some(at_temp) = fn_descend(&at) else {
                    // (no children, try advancing again)
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
        if root_predicate(&at) {
            return None;
        }
    }
}
