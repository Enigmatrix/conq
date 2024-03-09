// https://blog.theodo.com/2020/11/react-resizeable-split-panels/

use std::rc::Rc;

use gloo::utils::window;
use gloo::{/* console::log, */ events::EventListener};
use stylist::yew::{styled_component, Global};
use web_sys::HtmlElement;
use yew::prelude::*;
use yew_autoprops::autoprops;

use monaco::{api::CodeEditorOptions, sys::editor::BuiltinTheme, yew::CodeEditor};

#[derive(Clone, PartialEq)]
enum SplitAxis {
    Horizontal,
    Vertical,
}

const MIN_AXIS: i32 = 200;

const CONTENT: &str = include_str!("../static/main.rs");

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
        <CodeEditor classes={"full"} options={ options.to_sys_options() } />
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
            width: 100%;
        "#)}>
            <pre>{"test out please ignore"}</pre>
        </div>
    }
}

#[autoprops]
#[styled_component]
pub fn Divider(on_mouse_down: Callback<PointerEvent>) -> Html {
    let axis = use_context::<SplitAxis>().expect("No SplitAxis context found");
    html! {
        <div
        class={css!(r#"
            z-index: 1;
        "#)}
        style={match axis {
            SplitAxis::Horizontal => "padding: 0 1rem; margin: 0 -1rem; cursor: col-resize;",
            SplitAxis::Vertical => "padding: 1rem 0; margin: -1rem 0; cursor: row-resize;",
        }}
            onpointerdown={on_mouse_down.clone()}
        >
            <div class={css!(r#"
                background: #888;
            "#)}
            style={match axis {
                SplitAxis::Horizontal => "height: 100%; width: 4px;",
                SplitAxis::Vertical => "width: 100%; height: 4px;",
            }}
            />
        </div>
    }
}

#[autoprops]
#[function_component]
pub fn LeftPane(
    left_width: Option<i32>,
    set_left_width: Callback<i32>,
    children: &Children,
) -> Html {
    let left_ref = use_node_ref();
    let axis = use_context::<SplitAxis>().expect("No SplitAxis context found");

    {
        let left_width = left_width.clone();
        let set_left_width = set_left_width.clone();
        let left_ref = left_ref.clone();

        use_effect_with((left_width.clone(), axis.clone()), move |_| {
            if let Some(left_div) = left_ref.cast::<HtmlElement>() {
                if left_width.is_none() {
                    set_left_width.emit(match axis {
                        SplitAxis::Horizontal => left_div.client_width(),
                        SplitAxis::Vertical => left_div.client_height(),
                    });
                    left_div.style().set_property("flex", "0 0 auto").unwrap();
                } else {
                    left_div
                        .style()
                        .set_property(
                            match axis {
                                SplitAxis::Horizontal => "width",
                                SplitAxis::Vertical => "height",
                            },
                            &format!("{}px", left_width.unwrap()),
                        )
                        .unwrap();
                    left_div
                        .style()
                        .set_property(
                            match axis {
                                SplitAxis::Horizontal => "height",
                                SplitAxis::Vertical => "width",
                            },
                            "unset",
                        )
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
    let left_width = use_state_eq(|| None);
    let left_frac = use_state_eq(|| 0.5);
    let divider_x = use_state_eq(|| None);
    let is_dragging = use_state_eq(|| false);
    let axis_ctx = use_state_eq(|| SplitAxis::Horizontal);

    let split_pane_ref = use_node_ref();

    let set_left_width = {
        let left_width = left_width.clone();
        Callback::from(move |value: i32| {
            left_width.set(Some(value));
        })
    };

    let on_mouse_down = {
        let divider_x = divider_x.clone();
        let is_dragging = is_dragging.clone();
        let axis_ctx = axis_ctx.clone();
        Callback::from(move |event: PointerEvent| {
            let pos = match *axis_ctx {
                SplitAxis::Horizontal => event.client_x(),
                SplitAxis::Vertical => event.client_y(),
            };
            divider_x.set(Some(pos));
            is_dragging.set(true);
        })
    };

    let set_width_bounded = {
        let left_width = left_width.clone();
        let left_frac = left_frac.clone();
        let split_pane_ref = split_pane_ref.clone();
        let axis_ctx = axis_ctx.clone();
        Callback::from(move |value: i32| {
            if let Some(split_pane_div) = split_pane_ref.cast::<HtmlElement>() {
                let new_width;
                let parent_len = match *axis_ctx {
                    SplitAxis::Horizontal => split_pane_div.client_width(),
                    SplitAxis::Vertical => split_pane_div.client_height(),
                };
                if value < MIN_AXIS {
                    new_width = MIN_AXIS;
                } else if value > parent_len - MIN_AXIS {
                    new_width = parent_len - MIN_AXIS;
                } else {
                    new_width = value;
                }
                left_width.set(Some(new_width));
                left_frac.set(new_width as f64 / parent_len as f64);
            }
        })
    };

    let on_mouse_move = {
        let left_width = left_width.clone();
        let divider_x = divider_x.clone();
        let is_dragging = is_dragging.clone();
        let set_width_bounded = set_width_bounded.clone();
        let axis_ctx = axis_ctx.clone();
        Callback::from(move |event: PointerEvent| {
            if *is_dragging {
                if left_width.is_some() {
                    let drag_len = match *axis_ctx {
                        SplitAxis::Horizontal => event.client_x(),
                        SplitAxis::Vertical => event.client_y(),
                    };
                    let new_width = left_width.unwrap() + (drag_len - divider_x.unwrap());
                    divider_x.set(Some(drag_len));
                    set_width_bounded.emit(new_width);
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

    use_effect_with((), {
        let axis_ctx = axis_ctx.clone();
        let left_frac = left_frac.clone();
        let left_width = left_width.clone();
        let split_pane_ref = split_pane_ref.clone();
        move |_| {
            EventListener::new(&window(), "resize", move |_| {
                let new_axis;
                if window().inner_width().unwrap().as_f64().unwrap()
                    < window().inner_height().unwrap().as_f64().unwrap()
                {
                    new_axis = SplitAxis::Vertical;
                } else {
                    new_axis = SplitAxis::Horizontal;
                }
                axis_ctx.set(new_axis.clone());
                if let Some(split_pane_div) = split_pane_ref.cast::<HtmlElement>() {
                    let parent_len = match new_axis {
                        SplitAxis::Horizontal => split_pane_div.client_width(),
                        SplitAxis::Vertical => split_pane_div.client_height(),
                    };
                    let new_width = (*left_frac * parent_len as f64) as i32;
                    left_width.set(Some(new_width));
                }
            })
            .forget();
            || {}
        }
    });

    html! {
        <ContextProvider<SplitAxis> context={(*axis_ctx).clone()}>
        <div class={css!(r#"
            display: flex;
            height: 100%;
            width: 100%;
            align-items: stretch;
        "#)}
            style={format!("flex-direction: {};", match *axis_ctx {
                SplitAxis::Horizontal => "row",
                SplitAxis::Vertical => "column",
            })}
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
        </ContextProvider<SplitAxis>>
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
