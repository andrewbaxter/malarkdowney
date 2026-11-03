use {
    flowcontrol::superif,
    gloo::utils::document,
    std::collections::{
        HashMap,
        HashSet,
    },
    wasm_bindgen::JsCast,
    web_sys::{
        Element,
        Node,
        Text,
    },
};

pub enum ShadowDom {
    Text(String),
    Element(ShadowDomElement),
}

pub struct ShadowDomElement {
    pub tag: &'static str,
    pub classes: HashSet<String>,
    pub attrs: HashMap<String, String>,
    pub children: Vec<ShadowDom>,
}

pub fn sync_shadow_dom(changed: &mut bool, have: &Node, want: ShadowDom) -> Node {
    fn replace(changed: &mut bool, old: &Node, new: &Node) {
        *changed = true;
        if let Some(parent) = old.parent_node() {
            _ = parent.replace_child(new, old);
        }
    }

    match want {
        ShadowDom::Text(want) => {
            match have.dyn_ref::<Text>() {
                Some(have) => {
                    if have.data() != want {
                        *changed = true;
                        have.set_data(&want);
                    }
                    return have.clone().into();
                },
                None => {
                    let new = document().create_text_node(&want);
                    replace(changed, have, &new);
                    return new.into();
                },
            }
        },
        ShadowDom::Element(want) => {
            let have = superif!({
                let Some(have) = have.dyn_ref::<Element>() else {
                    break 'mismatch;
                };
                if !have.tag_name().eq_ignore_ascii_case(want.tag) {
                    break 'mismatch;
                }
                break have.clone();
            } 'mismatch {
                let have1 = document().create_element(want.tag).unwrap();
                replace(changed, have, &have1);
                break have1;
            });

            // Sync attrs
            {
                let mut remove_attrs = vec![];
                for k in have.get_attribute_names() {
                    let Some(k) = k.as_string() else {
                        continue;
                    };
                    if k == "class" || k == "style" {
                        continue;
                    }
                    if !want.attrs.contains_key(&k) {
                        remove_attrs.push(k);
                    }
                }
                for k in remove_attrs {
                    *changed = true;
                    _ = have.remove_attribute(&k);
                }
            }
            for (k, v) in want.attrs {
                if have.get_attribute(&k).as_ref() != Some(&v) {
                    *changed = true;
                    _ = have.set_attribute(&k, &v);
                }
            }

            // Sync classes
            let classes = have.class_list();
            {
                let mut remove_classes = vec![];
                for i in 0 .. classes.length() {
                    let Some(k) = classes.item(i) else {
                        continue;
                    };
                    if !want.classes.contains(&k) {
                        remove_classes.push(k);
                    }
                }
                for k in remove_classes {
                    *changed = true;
                    _ = classes.remove_1(&k);
                }
            }
            for k in want.classes {
                if !classes.contains(&k) {
                    *changed = true;
                    _ = classes.add_1(&k);
                }
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
                let Some(have_child) = have_children.item(i as u32) else {
                    break;
                };
                sync_shadow_dom(changed, &have_child, want_child);
                i += 1;
            }
            for _ in i .. have_children_len {
                let Some(child) = have.last_child() else {
                    break;
                };
                *changed = true;
                _ = have.remove_child(&child);
            }
            for want_child in want_children_iter {
                let child = match &want_child {
                    ShadowDom::Text(want_child) => {
                        document().create_text_node(&want_child).dyn_into::<Node>().unwrap()
                    },
                    ShadowDom::Element(want_child) => {
                        document().create_element(&want_child.tag).unwrap().dyn_into::<Node>().unwrap()
                    },
                };
                *changed = true;
                _ = have.append_child(&child);
                sync_shadow_dom(changed, &child, want_child);
            }
            return have.into();
        },
    }
}
