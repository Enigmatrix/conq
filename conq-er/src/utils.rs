use yew::Callback;
use wasm_bindgen::closure::Closure;

pub fn attach_listener<T>(
    target: &web_sys::EventTarget,
    event_name: &str,
    callback: &Callback<T>,
) -> Closure<dyn FnMut(web_sys::Event)>
where
    T: 'static,
{
    let callback = callback.clone();
    let closure = Closure::wrap(Box::new(move |event: T| {
        callback.emit(event);
    }) as Box<dyn FnMut(_)>);
    target
        .add_event_listener_with_callback(event_name, closure.as_ref().unchecked_ref())
        .unwrap();
    closure
}

pub fn detach_listener<T>(
    target: &web_sys::EventTarget,
    event_name: &str,
    closure: &Closure<dyn FnMut(web_sys::Event)>,
) where
    T: 'static,
{
    target
        .remove_event_listener_with_callback(event_name, closure.as_ref().unchecked_ref())
        .unwrap();
}