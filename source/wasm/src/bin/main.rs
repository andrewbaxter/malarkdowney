use {
    gloo::utils::document,
    malarkdowney::{
        self,
        get_text,
    },
    wasm_bindgen::JsValue,
    web_sys::console,
};

fn main() {
    console_error_panic_hook::set_once();
    let m = malarkdowney::build(include_str!("../main.md"));
    let body = document().body().unwrap();
    body.append_child(&m).unwrap();
    body.append_child(&{
        let e = document().create_element("hr").unwrap();
        e
    }.into()).unwrap();
    body.append_child(&{
        let e = document().create_element("p").unwrap();
        e.class_list().add_1("footnote").unwrap();
        e.append_child(&{
            let e = document().create_element("span").unwrap();
            e.set_text_content(Some("From "));
            e
        }.into()).unwrap();
        e.append_child(&{
            let e = document().create_element("a").unwrap();
            e.set_attribute("href", "https://github.com/andrewbaxter/malarkdowney").unwrap();
            e.set_text_content(Some("andrewbaxter/malarkdowney"));
            e
        }.into()).unwrap();
        e.append_child(&{
            let e = document().create_element("span").unwrap();
            e.set_text_content(Some(", with love and bountiful markdowns"));
            e
        }.into()).unwrap();
        e
    }.into()).unwrap();
    console::log_1(&JsValue::from(format!("get_text:\n{}", get_text(&m))));
}
