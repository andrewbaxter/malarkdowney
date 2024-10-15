use {
    flowcontrol::{
        shed,
        superif,
    },
    gloo::utils::{
        document,
        window,
    },
    std::{
        cell::Cell,
        collections::{
            HashMap,
            HashSet,
        },
    },
    wasm_bindgen::{
        JsCast,
        JsValue,
    },
    web_sys::{
        console,
        Element,
        Node,
        Text,
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

pub fn generate_shadow_dom(offset: &Cell<usize>, want: ShadowDom) -> Node {
    match want {
        ShadowDom::Text(want) => {
            let new = document().create_text_node(&want).dyn_into().unwrap();
            offset.set(offset.get() + want.len());
            return new;
        },
        ShadowDom::Element(want) => {
            let have = document().create_element(want.tag).unwrap();
            for (k, v) in want.attrs {
                have.set_attribute(&k, &v).unwrap();
            }
            let classes = have.class_list();
            for k in want.classes {
                classes.add_1(&k).unwrap();
            }
            for want_child in want.children {
                have.append_child(&generate_shadow_dom(offset, want_child)).unwrap();
            }
            return have.dyn_into().unwrap();
        },
    }
}

pub fn sync_shadow_dom(cursor: &mut Option<usize>, offset: &Cell<usize>, have: &Node, want: ShadowDom) {
    fn replace(old: &Node, new: &Node) {
        old.parent_node().unwrap().replace_child(new, old).unwrap();
    }

    match want {
        ShadowDom::Text(want) => {
            match have.dyn_ref::<Text>() {
                Some(have) => {
                    if have.text_content().unwrap() != want {
                        console::log_2(
                            &JsValue::from(
                                &format!(
                                    "sync dom - changing text node text from [{}] to [{}]",
                                    have.text_content().unwrap(),
                                    want
                                ),
                            ),
                            &JsValue::from(have),
                        );
                        have.set_text_content(Some(&want));
                    }
                    shed!{
                        let Some(cursor) = cursor.take_if(|cursor| offset.get() + want.len() >= *cursor) else {
                            break;
                        };
                        let rel_offset = cursor - offset.get();
                        console::log_2(
                            &JsValue::from(&format!("sync dom - set sel at rel offset {}", rel_offset)),
                            &JsValue::from(have),
                        );
                        let sel = window().get_selection().unwrap().unwrap();
                        sel.remove_all_ranges().unwrap();
                        let range = document().create_range().unwrap();
                        range.set_start(&have, rel_offset as u32).unwrap();
                        range.set_end(&have, rel_offset as u32).unwrap();
                        sel.add_range(&range).unwrap();
                    };
                    offset.set(offset.get() + want.len());
                },
                None => {
                    eprintln!("sync dom - not text; replacing text (was not text) [{}]", want);
                    replace(have, &generate_shadow_dom(offset, ShadowDom::Text(want)));
                },
            };
        },
        ShadowDom::Element(want) => {
            let have = superif!({
                let Some(have) = have.dyn_ref::<Element>() else {
                    break 'mismatch;
                };
                if have.tag_name().to_ascii_lowercase() != want.tag {
                    break 'mismatch;
                }
                break have.clone();
            } 'mismatch {
                eprintln!("sync dom - not el or wrong tag; replacing el, wanted tag {}", want.tag);
                replace(have, &generate_shadow_dom(offset, ShadowDom::Element(want)));
                return;
            });

            // Sync attrs
            {
                let mut remove_attrs = vec![];
                for k in have.get_attribute_names() {
                    let k = k.as_string().unwrap();
                    if k == "class" || k == "style" {
                        continue;
                    }
                    if !want.attrs.contains_key(&k) {
                        remove_attrs.push(k);
                    }
                }
                for k in remove_attrs {
                    console::log_2(
                        &JsValue::from(&format!("sync dom - removing attr [{}]", k)),
                        &JsValue::from(&have),
                    );
                    have.remove_attribute(&k).unwrap();
                }
            }
            for (k, v) in want.attrs {
                if have.get_attribute(&k).as_ref() != Some(&v) {
                    console::log_2(
                        &JsValue::from(&format!("sync dom - setting attr [{}] [{}]", k, v)),
                        &JsValue::from(&have),
                    );
                    have.set_attribute(&k, &v).unwrap();
                }
            }

            // Sync classes
            let classes = have.class_list();
            {
                let mut remove_classes = vec![];
                for k in classes.values() {
                    let k = k.unwrap();
                    let Some(k) = k.as_string() else {
                        console::log_2(&JsValue::from("error current class as str"), &k);
                        continue;
                    };
                    if !want.classes.contains(&k) {
                        remove_classes.push(k);
                    }
                }
                for k in remove_classes {
                    console::log_2(
                        &JsValue::from(&format!("sync dom - removing class [{}]", k)),
                        &JsValue::from(&have),
                    );
                    classes.remove_1(&k).unwrap();
                }
            }
            for k in want.classes {
                if !classes.contains(&k) {
                    console::log_2(
                        &JsValue::from(&format!("sync dom - setting class [{}]", k)),
                        &JsValue::from(&have),
                    );
                    classes.add_1(&k).unwrap();
                }
            }

            // Sync children
            let have_children = have.child_nodes();
            let have_children_len = have_children.length() as usize;

            //. console::log_2(
            //.     &JsValue::from(&format!("sync dom - have children {}", have_children_len)),
            //. &JsValue::from(&have),
            //. );
            let mut want_children_iter = want.children.into_iter();
            let mut i = 0;
            while i < have_children_len {
                let Some(want_child) = want_children_iter.next() else {
                    break;
                };

                //. eprintln!("sync dom - sync el {}", i);
                let have_child = have_children.item(i as u32).unwrap();
                sync_shadow_dom(cursor, offset, &have_child, want_child);
                i += 1;
            }
            for _ in i .. have_children_len {
                let child = have.last_child().unwrap();
                console::log_3(
                    &JsValue::from("sync dom - removing extra child"),
                    &JsValue::from(&child),
                    &JsValue::from(&have),
                );
                have.remove_child(&child).unwrap();
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
                have.append_child(&child).unwrap();
                console::log_3(
                    &JsValue::from("sync dom - adding missing child"),
                    &JsValue::from(&child),
                    &JsValue::from(&have),
                );
                sync_shadow_dom(cursor, offset, &child, want_child);
            }
        },
    }
}
