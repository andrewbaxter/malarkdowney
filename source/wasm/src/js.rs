use {
    crate::{
        ast::{
            Block,
            Inline,
            ListItem,
            self,
        },
        syntax::Syntax,
    },
    js_sys::{
        Array,
        Object,
        Reflect,
    },
    wasm_bindgen::{
        JsCast,
        JsValue,
    },
};

pub fn ast_from_js(v: &JsValue) -> Result<Vec<Block>, JsValue> {
    if !Array::is_array(v) {
        return Err(err("expected an array of blocks".to_string()));
    }
    let mut out = vec![];
    for b in v.unchecked_ref::<Array>().iter() {
        let type_ = get_string(&b, "type")?;
        out.push(match type_.as_str() {
            "paragraph" => Block::Paragraph(inlines_from_js(&b)?),
            "heading" => Block::Heading {
                level: get(&b, "level")
                    .as_f64()
                    .filter(|l| (1. ..= 6.).contains(l) && l.fract() == 0.)
                    .ok_or_else(|| err("expected heading `level` 1 to 6".to_string()))? as
                    usize,
                children: inlines_from_js(&b)?,
            },
            "quote" => Block::Quote(ast_from_js(&get(&b, "children"))?),
            "list" => Block::List {
                ordered: get(&b, "ordered").as_bool().unwrap_or(false),
                items: get_array(&b, "items")?.iter().map(|item| {
                    return Ok(ListItem {
                        marker: get_optional_string(&item, "marker")?,
                        children: ast_from_js(&get(&item, "children"))?,
                    });
                }).collect::<Result<Vec<_>, JsValue>>()?,
            },
            "code" => Block::Code {
                info: get_optional_string(&b, "info")?,
                lines: get_array(&b, "lines")?
                    .iter()
                    .map(|l| l.as_string().ok_or_else(|| err("expected code `lines` to be strings".to_string())))
                    .collect::<Result<Vec<_>, _>>()?,
                closed: get(&b, "closed").as_bool().unwrap_or(true),
            },
            _ => return Err(err(format!("unknown block type [{}]", type_))),
        });
    }
    return Ok(out);
}

pub fn ast_to_js(blocks: &[Block]) -> JsValue {
    return blocks.iter().map(|b| match b {
        Block::Paragraph(c) => return object("paragraph", &[("children", inlines_to_js(c))]),
        Block::Heading { level, children } => return object(
            "heading",
            &[("level", JsValue::from_f64(*level as f64)), ("children", inlines_to_js(children))],
        ),
        Block::Quote(c) => return object("quote", &[("children", ast_to_js(c))]),
        Block::List { ordered, items } => return object(
            "list",
            &[("ordered", JsValue::from_bool(*ordered)), ("items", items.iter().map(|item| {
                let out = Object::new();
                _ = Reflect::set(&out, &JsValue::from_str("marker"), &JsValue::from_str(&item.marker));
                _ = Reflect::set(&out, &JsValue::from_str("children"), &ast_to_js(&item.children));
                return JsValue::from(out);
            }).collect::<Array>().into())],
        ),
        Block::Code { info, lines, closed } => return object(
            "code",
            &[
                ("info", JsValue::from_str(info)),
                ("lines", lines.iter().map(|s| JsValue::from_str(s)).collect::<Array>().into()),
                ("closed", JsValue::from_bool(*closed)),
            ],
        ),
    }).collect::<Array>().into();
}

fn err(message: String) -> JsValue {
    return js_sys::TypeError::new(&format!("malarkdowney: {}", message)).into();
}

fn get(o: &JsValue, key: &str) -> JsValue {
    return Reflect::get(o, &JsValue::from_str(key)).unwrap_or(JsValue::UNDEFINED);
}

fn get_array(o: &JsValue, key: &str) -> Result<Array, JsValue> {
    let v = get(o, key);
    if !Array::is_array(&v) {
        return Err(err(format!("expected an array `{}`", key)));
    }
    return Ok(v.unchecked_into());
}

fn get_optional_string(o: &JsValue, key: &str) -> Result<String, JsValue> {
    let v = get(o, key);
    if v.is_undefined() {
        return Ok(String::new());
    }
    return v.as_string().ok_or_else(|| err(format!("expected `{}` to be a string", key)));
}

fn get_string(o: &JsValue, key: &str) -> Result<String, JsValue> {
    return get(o, key).as_string().ok_or_else(|| err(format!("expected a string `{}`", key)));
}

fn inlines_from_js(v: &JsValue) -> Result<Vec<Inline>, JsValue> {
    let children = get_array(v, "children")?;
    let mut out = vec![];
    for i in children.iter() {
        if let Some(t) = i.as_string() {
            out.push(Inline::Text(t));
            continue;
        }
        let type_ = get_string(&i, "type")?;
        out.push(match type_.as_str() {
            "text" => Inline::Text(get_string(&i, "text")?),
            "strong" => Inline::Strong(inlines_from_js(&i)?),
            "emphasis" => Inline::Emphasis(inlines_from_js(&i)?),
            "strikethrough" => Inline::Strikethrough(inlines_from_js(&i)?),
            "code" => Inline::Code(get_string(&i, "text")?),
            "link" => Inline::Link {
                children: inlines_from_js(&i)?,
                address: get_string(&i, "address")?,
            },
            "delimited" => Inline::Delimited {
                kind: get_string(&i, "kind")?,
                children: inlines_from_js(&i)?,
            },
            "prefixed" => Inline::Prefixed {
                kind: get_string(&i, "kind")?,
                value: get_string(&i, "value")?,
            },
            _ => return Err(err(format!("unknown inline type [{}]", type_))),
        });
    }
    return Ok(out);
}

fn inlines_to_js(inlines: &[Inline]) -> JsValue {
    return inlines.iter().map(|i| match i {
        Inline::Text(t) => return object("text", &[("text", JsValue::from_str(t))]),
        Inline::Strong(c) => return object("strong", &[("children", inlines_to_js(c))]),
        Inline::Emphasis(c) => return object("emphasis", &[("children", inlines_to_js(c))]),
        Inline::Strikethrough(c) => return object("strikethrough", &[("children", inlines_to_js(c))]),
        Inline::Code(t) => return object("code", &[("text", JsValue::from_str(t))]),
        Inline::Link { children, address } => return object(
            "link",
            &[("children", inlines_to_js(children)), ("address", JsValue::from_str(address))],
        ),
        Inline::Delimited { kind, children } => return object(
            "delimited",
            &[("kind", JsValue::from_str(kind)), ("children", inlines_to_js(children))],
        ),
        Inline::Prefixed { kind, value } => return object(
            "prefixed",
            &[("kind", JsValue::from_str(kind)), ("value", JsValue::from_str(value))],
        ),
    }).collect::<Array>().into();
}

pub fn markdown_from_js(v: &JsValue, syntax: &Syntax) -> Result<String, JsValue> {
    if let Some(text) = v.as_string() {
        return Ok(text);
    }
    return Ok(ast::ast_to_text(&ast_from_js(v)?, syntax));
}

fn object(type_: &str, fields: &[(&str, JsValue)]) -> JsValue {
    let out = Object::new();
    _ = Reflect::set(&out, &JsValue::from_str("type"), &JsValue::from_str(type_));
    for (k, v) in fields {
        _ = Reflect::set(&out, &JsValue::from_str(k), v);
    }
    return out.into();
}

pub fn syntax_from_js(v: &JsValue) -> Result<Syntax, JsValue> {
    let mut syntax = Syntax::default();
    if v.is_undefined() || v.is_null() {
        return Ok(syntax);
    }
    if !get(v, "prefixes").is_undefined() {
        for p in get_array(v, "prefixes")?.iter() {
            let prefix = get_string(&p, "prefix")?;
            let mut chars = prefix.chars();
            let (Some(prefix), None) = (chars.next(), chars.next()) else {
                return Err(err(format!("prefix [{}] must be one character", prefix)));
            };
            syntax =
                syntax
                    .with_prefix(&get_string(&p, "kind")?, prefix, &get_optional_string(&p, "chars")?)
                    .map_err(err)?;
        }
    }
    if !get(v, "delimiters").is_undefined() {
        for d in get_array(v, "delimiters")?.iter() {
            let open = get_string(&d, "open")?;
            let close = get(&d, "close").as_string().unwrap_or_else(|| open.clone());
            syntax = syntax.with_delimiter(&get_string(&d, "kind")?, &open, &close).map_err(err)?;
        }
    }
    return Ok(syntax);
}
