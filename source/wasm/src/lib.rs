pub mod malarkdowney;

#[macro_export]
macro_rules! cprintln{
    ($pat: literal) => {
        web_sys:: console:: log_1(& wasm_bindgen:: JsValue:: from_str($pat))
    };
    ($pat: literal, $($data: expr), *) => {
        web_sys::console::log_1(&wasm_bindgen::JsValue::from_str(&format!($pat, $($data,) *)))
    };
}
