use {
    rooting::{
        el,
        set_root,
    },
    wasm_bindgen::JsValue,
    web_sys::console,
};

fn main() {
    console_error_panic_hook::set_once();
    let (m, editor) = malarkdowney::build(include_str!("../main.md"), Default::default());
    let text = editor.get_text();
    set_root(
        vec![
            m,
            el("hr"),
            el("p")
                .classes(&["footnote"])
                .push(el("span").text("From "))
                .push(
                    el("a")
                        .attr("href", "https://github.com/andrewbaxter/malarkdowney")
                        .text("andrewbaxter/malarkdowney"),
                )
                .push(el("span").text(", with love and bountiful markdowns")),
        ],
    );
    console::log_1(&JsValue::from(format!("get_text:\n{}", text)));
}
