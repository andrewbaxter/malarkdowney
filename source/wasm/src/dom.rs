use {
    gloo::utils::{
        document,
        window,
    },
    web_sys::{
        DomRect,
        Node,
    },
};

pub fn bounds(n: &Node) -> DomRect {
    let r = document().create_range().unwrap();
    r.select_node(n).unwrap();
    return r.get_bounding_client_rect();
}

pub fn select(n: &Node, rel_offset: usize) {
    let sel = window().get_selection().unwrap().unwrap();
    sel.remove_all_ranges().unwrap();
    let range = document().create_range().unwrap();
    range.set_start(n, rel_offset as u32).unwrap();
    range.set_end(n, rel_offset as u32).unwrap();
    sel.add_range(&range).unwrap();
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
