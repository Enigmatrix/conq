use std::sync::Mutex;

use js_sys::{self, BigInt};
use js_sys::{Function, Map, Object, Reflect, WebAssembly, WebAssembly::Memory, JSON::stringify};
use wasm_bindgen::prelude::*;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen_futures::JsFuture;
use web_sys::CanvasRenderingContext2d;

static STD_OUT: Mutex<String> = Mutex::new(String::new());

async fn execute(binary: Vec<u8>) -> Result<JsValue, JsValue> {
    let result = JsFuture::from(WebAssembly::instantiate_buffer(
        &binary.as_slice(),
        &make_imports().unwrap(),
    ))
    .await
    .unwrap();
    let instance: WebAssembly::Instance = Reflect::get(&result, &"instance".into())
        .unwrap()
        .dyn_into()
        .unwrap();
    // let memory = Reflect::get(&instance.exports(), &"memory".into()).unwrap().dyn_into::<WebAssembly::Memory>().expect("memory export wasn't a `WebAssembly.Memory");
    // memory.grow(2);
    // log!("Instance", &instance);
    let start = Reflect::get(&instance.exports(), &"_start".into()) // TODO: Export Start
        .unwrap()
        .dyn_into::<Function>()
        .expect("entrypoint function start not found");
    start.call1(&JsValue::undefined(), &0.into())
}

pub async fn exec_n_get_output(binary: Vec<u8>) -> String {
    let mut result = b_stringify(&execute(binary).await.unwrap());
    result.insert_str(0, &STD_OUT.lock().unwrap());
    STD_OUT.lock().unwrap().clear();
    result
}

fn b_stringify(a: &JsValue) -> String {
    if a.is_undefined() {
        "undefined".to_string()
    } else if a.is_null() {
        "null".to_string()
    } else if let Some(s) = a.as_string() {
        s
    } else if let Some(b) = a.dyn_ref::<BigInt>() {
        b.to_string(10).unwrap().into()
    } else {
        stringify(&a).unwrap_throw().as_string().unwrap()
    }
}

#[wasm_bindgen]
pub struct Env {
    _canvas: web_sys::HtmlCanvasElement,
    _context: CanvasRenderingContext2d,
    _memory: Memory,
    _mem_current: i64,
}

#[wasm_bindgen]
impl Env {

    pub fn new() -> Env {
        let mem_descriptor = Object::new();
        Reflect::set(
            &mem_descriptor,
            &JsValue::from("initial"),
            &JsValue::from(10),
        )
        .unwrap();
        Reflect::set(
            &mem_descriptor,
            &JsValue::from("maximum"),
            &JsValue::from(256),
        )
        .unwrap();
        Env {
            _canvas: JsValue::undefined().into(),
            _context: JsValue::undefined().into(),
            _memory: Memory::new(&mem_descriptor).unwrap(),
            _mem_current: 0,
        }
    }

    fn mem_size(&self) -> i64 {
        self._memory
            .buffer()
            .dyn_into::<js_sys::ArrayBuffer>()
            .unwrap()
            .byte_length() as i64
    }

    pub fn malloc(&mut self, size: i64) -> i32 {
        let mem_current = self._mem_current;
        self._mem_current += size;
        if self._mem_current > self.mem_size() {
            self._memory.grow(1);
        };
        mem_current as i32
    }

    pub fn print(&self, a: JsValue) {
        let str = b_stringify(&a);
        STD_OUT.lock().unwrap().push_str(&str);
    }

    pub fn init_canvas(&mut self) {
        let document = web_sys::window()
            .unwrap()
            .open()
            .unwrap()
            .unwrap()
            .document()
            .unwrap();
        self._canvas = document
            .create_element("canvas")
            .unwrap()
            .unchecked_into::<web_sys::HtmlCanvasElement>();
        self._context = self
            ._canvas
            .get_context("2d")
            .unwrap()
            .unwrap()
            .unchecked_into::<web_sys::CanvasRenderingContext2d>();
        self._context.begin_path();
    }

    pub fn clear_canvas(&self) {
        self._context.clear_rect(
            0.0,
            0.0,
            self._canvas.width() as f64,
            self._canvas.height() as f64,
        );
        self._context.begin_path();
    }

    pub fn line_to(&self, x: f64, y: f64) {
        self._context.line_to(x, y);
    }

    pub fn move_to(&self, x: f64, y: f64) {
        self._context.move_to(x, y);
    }

    pub fn close_path(&self) {
        self._context.close_path();
    }

    pub fn stroke_style(&self, style: &str) {
        self._context.set_stroke_style(&JsValue::from_str(style));
    }

    pub fn stroke(&self) {
        self._context.stroke();
    }

    pub fn fill_style(&self, style: &str) {
        self._context.set_fill_style(&JsValue::from_str(style));
    }

    pub fn fill(&self) {
        self._context.fill();
    }

    pub fn arc(&self, x: f64, y: f64, radius: f64, start_angle: f64, end_angle: f64) {
        self._context
            .arc(x, y, radius, start_angle, end_angle)
            .unwrap();
    }

    pub fn quadratic_curve_to(&self, cpx: f64, cpy: f64, x: f64, y: f64) {
        self._context.quadratic_curve_to(cpx, cpy, x, y);
    }

    pub fn bezier_curve_to(&self, cp1x: f64, cp1y: f64, cp2x: f64, cp2y: f64, x: f64, y: f64) {
        self._context.bezier_curve_to(cp1x, cp1y, cp2x, cp2y, x, y);
    }
}

fn bind(this: &JsValue, func_name: &str) -> Result<(), JsValue> {
    let property_key = JsValue::from(func_name);
    let orig_func = Reflect::get(this, &property_key)?.dyn_into::<Function>()?;
    let func = orig_func.bind(this);
    if !Reflect::set(this, &property_key, &func)? {
        return Err(JsValue::from("failed to set property"));
    }
    Ok(())
}

pub fn make_imports() -> Result<Object, JsValue> {
    let map = Map::new();
    let env: JsValue = Env::new().into();

    // Reflect::set(&imports, &JsValue::from("__linear_memory"), &memory).unwrap();

    // let canvas: JsValue = Canvas::new().into();
    // Reflect::set(&imports, &JsValue::from("canvas"), &canvas).unwrap();

    bind(&env, "malloc")?;
    bind(&env, "print")?;
    bind(&env, "init_canvas")?;
    bind(&env, "clear_canvas")?;
    bind(&env, "line_to")?;
    bind(&env, "move_to")?;
    bind(&env, "close_path")?;
    bind(&env, "stroke_style")?;
    bind(&env, "stroke")?;
    bind(&env, "fill_style")?;
    bind(&env, "fill")?;
    bind(&env, "arc")?;
    bind(&env, "quadratic_curve_to")?;
    bind(&env, "bezier_curve_to")?;

    map.set(&JsValue::from("env"), &env);
    Object::from_entries(&map.into())
}
