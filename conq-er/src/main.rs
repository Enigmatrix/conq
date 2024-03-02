// https://blog.theodo.com/2020/11/react-resizeable-split-panels/

use gloo_console::log;
use stylist::yew::{styled_component, Global};
use wasm_bindgen::{closure::Closure, JsCast};
use web_sys::{console::log, HtmlElement};
use yew::prelude::*;
use yew_autoprops::autoprops;

// mod utils;

#[styled_component]
pub fn Editor() -> Html {
    html! {
        <div class={css!(r#"
            background: #1e1e1e;
            padding: 15px;
            box-sizing: border-box;
            color: #d4d4d4;
            height: 100%;
        "#)}/>
    }
}

#[styled_component]
pub fn Output() -> Html {
    html! {
        <div class={css!(r#"
            background: #222;
            padding: 15px;
            box-sizing: border-box;
            color: #f2f2f2;
            height: 100%;
        "#)}/>
    }
}

#[autoprops]
#[styled_component]
pub fn Divider(on_mouse_down: Callback<web_sys::MouseEvent>) -> Html {
    html! {
        <div class={css!(r#"
            background: #888;
            width: 5px;
        "#)}
            onmousedown={on_mouse_down}
        />
    }
}

#[autoprops]
#[function_component]
pub fn LeftPane(
    left_width: Option<f64>,
    set_left_width: Callback<f64>,
    children: &Children,
) -> Html {
    let left_ref = use_node_ref();

    {
        let left_width = left_width.clone();
        let set_left_width = set_left_width.clone();
        let left_ref = left_ref.clone();

        use_effect_with(
            (left_ref.clone(), left_width, set_left_width.clone()),
            move |_| {
                if let Some(left_div) = left_ref.cast::<HtmlElement>() {
                    if left_width.is_none() {
                        set_left_width.emit(left_div.client_width() as f64);
                        left_div.style().set_property("flex", "0 0 auto").unwrap();
                    } else {
                        left_div
                            .style()
                            .set_property("width", &format!("{}px", left_width.unwrap()))
                            .unwrap();
                    }
                }
                || {}
            },
        );
    }

    html! {
        <div style="flex: 1;" ref={left_ref.clone()}>
            {children.clone()}
        </div>
    }
}

#[autoprops]
#[styled_component]
pub fn SplitPane(left: &Html, right: &Html) -> Html {
    // TODO: use_state_eq?
    let left_width = use_state(|| None);

    let divider_x = use_state(|| None);
    let is_dragging = use_state(|| false);

    let set_left_width = {
        let left_width = left_width.clone();
        Callback::from(move |value: f64| {
            left_width.set(Some(value));
        })
    };

    let on_mouse_down = {
        let divider_x = divider_x.clone();
        let is_dragging = is_dragging.clone();
        Callback::from(move |event: web_sys::MouseEvent| {
            event.prevent_default();
            divider_x.set(Some(event.client_x() as f64));
            log!("down");
            is_dragging.set(true);
        })
    };

    let on_mouse_move = {
        let left_width = left_width.clone();
        let divider_x = divider_x.clone();
        let is_dragging = is_dragging.clone();
        let set_left_width = set_left_width.clone();
        Callback::from(move |event: web_sys::MouseEvent| {
            log!(*is_dragging);
            if *is_dragging {
                if divider_x.is_some() {
                    if let Some(left_width) = *left_width {
                        set_left_width
                            .emit(left_width + event.client_x() as f64 - divider_x.unwrap());
                        divider_x.set(Some(event.client_x() as f64));
                    }
                }
            }
        })
    };

    let on_mouse_up = {
        let is_dragging = is_dragging.clone();
        Callback::from(move |_| {
            is_dragging.set(false);
        })
    };

    {
        let on_mouse_move = on_mouse_move.clone();
        let on_mouse_up = on_mouse_up.clone();
        use_effect_with((), move |_| {
            let window = web_sys::window().unwrap();
            let closure = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                on_mouse_move.emit(event);
            }) as Box<dyn FnMut(web_sys::MouseEvent)>);
            let closure_move_ref = closure.as_ref();
            window
                .add_event_listener_with_callback("mousemove", closure_move_ref.unchecked_ref())
                .unwrap();
            closure.forget();
            let closure = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                on_mouse_up.emit(event);
            }) as Box<dyn FnMut(web_sys::MouseEvent)>);
            let closure_up_ref = closure.as_ref();
            window
                .add_event_listener_with_callback("mouseup", closure_up_ref.unchecked_ref())
                .unwrap();
            closure.forget();
        });
    }

    html! {
        <div class={css!(r#"
            display: flex;
            flex-direction: row;
            height: 100%;
            width: 100%;
            align-items: stretch;
        "#)}>
            <LeftPane
                left_width={&*left_width}
                set_left_width={set_left_width.clone()}
            >
                {left.clone()}
            </LeftPane>
            <Divider on_mouse_down={on_mouse_down.clone()} />
            <div style="flex: 1;" >
                {right.clone()}
            </div>
        </div>
    }
}

#[styled_component]
pub fn App() -> Html {
    html! {
        <>
            // Global Styles can be applied with <Global /> component.
            <Global css={css!(r#"
                    html, body {
                        font-family: monospace;
                        margin: 0;
                        padding: 0;
                        height: 100%;
                    }
                "#)} />
            <SplitPane
                left={html!{<Editor />}}
                right={html!{<Output />}}
            />
        </>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
