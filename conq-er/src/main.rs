// https://blog.theodo.com/2020/11/react-resizeable-split-panels/

use std::rc::Rc;

// use gloo_console::log;
use stylist::yew::{styled_component, Global};
use web_sys::HtmlElement;
use yew::prelude::*;
use yew_autoprops::autoprops;

use monaco::{
    api::CodeEditorOptions,
    sys::editor::BuiltinTheme,
    yew::CodeEditor,
};

const MIN_WIDTH: f64 = 200.0;

const CONTENT: &str = include_str!("main.rs");

fn get_options() -> CodeEditorOptions {
    CodeEditorOptions::default()
        .with_language("rust".to_owned())
        .with_value(CONTENT.to_owned())
        .with_builtin_theme(BuiltinTheme::VsDark)
        .with_automatic_layout(true)
}

#[styled_component]
pub fn Editor() -> Html {
    let options = Rc::new(get_options());
    html! {
        <CodeEditor classes={"full-height"} options={ options.to_sys_options() } />
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
pub fn Divider(on_mouse_down: Callback<PointerEvent>) -> Html {
    html! {
        <div class={css!(r#"
            cursor: col-resize;
            padding: 0 1rem;
            margin: 0 -1rem;
        "#)}
            onpointerdown={on_mouse_down.clone()}
        >
            <div class={css!(r#"
                width: 5px;
                background: #888;
                height: 100%;
            "#)} />
        </div>
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

        use_effect_with(left_width.clone(), move |_| {
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
        });
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

    let split_pane_ref = use_node_ref();

    let set_left_width = {
        let left_width = left_width.clone();
        Callback::from(move |value: f64| {
            left_width.set(Some(value));
        })
    };

    let on_mouse_down = {
        let divider_x = divider_x.clone();
        let is_dragging = is_dragging.clone();
        Callback::from(move |event: PointerEvent| {
            divider_x.set(Some(event.client_x() as f64));
            is_dragging.set(true);
        })
    };

    let on_mouse_move = {
        let left_width = left_width.clone();
        let divider_x = divider_x.clone();
        let is_dragging = is_dragging.clone();
        let split_pane_ref = split_pane_ref.clone();
        Callback::from(move |event: PointerEvent| {
            if *is_dragging {
                if left_width.is_some() {
                    let new_width =
                        left_width.unwrap() + (event.client_x() as f64 - divider_x.unwrap());
                    divider_x.set(Some(event.client_x() as f64));
                    if new_width < MIN_WIDTH {
                        left_width.set(Some(MIN_WIDTH));
                        return;
                    }
                    if let Some(left_div) = split_pane_ref.cast::<HtmlElement>() {
                        if new_width > (left_div.client_width() as f64) - MIN_WIDTH {
                            left_width.set(Some(left_div.client_width() as f64 - MIN_WIDTH));
                            return;
                        }
                    }
                    left_width.set(Some(new_width));
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

    html! {
        <div class={css!(r#"
            display: flex;
            flex-direction: row;
            height: 100%;
            width: 100%;
            align-items: stretch;
        "#)}
            onpointermove={on_mouse_move.clone()}
            onpointerup={on_mouse_up.clone()}
            onpointerleave={on_mouse_up.clone()}
            ref={split_pane_ref.clone()}
        >
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
                left={html!{<Editor />} }
                right={html!{<Output />}}
            />
        </>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
