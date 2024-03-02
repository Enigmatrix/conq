use stylist::yew::{styled_component, Global};
use yew::prelude::*;
use web_sys::HtmlElement;
use gloo_console::log;

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

#[styled_component]
pub fn Divider() -> Html {
    html! {
        <div class={css!(r#"
            background: #888;
            width: 5px;
        "#)}/>
    }
}

#[derive(Properties, PartialEq)]
pub struct LeftPaneProps {
    pub left_width: Option<f64>,
    pub set_left_width: Callback<f64>,
    pub children: Children,
}

#[function_component]
pub fn LeftPane(props: &LeftPaneProps) -> Html {
    let left_ref = use_node_ref();

    let left_width = props.left_width.clone();
    let set_left_width = props.set_left_width.clone();

    {
        let left_ref = left_ref.clone();

        use_effect_with((left_ref.clone(), left_width, set_left_width.clone()), move |_| {
            if let Some(left_div) = left_ref.cast::<HtmlElement>() {
                if left_width.is_none() {
                    set_left_width.emit(left_div.client_width() as f64);
                    left_div.style().set_property("flex", "0 0 auto").unwrap();
                } else {
                    left_div.style().set_property("width", &format!("{}px", left_width.unwrap())).unwrap();
                }
            }
            || {}
        });
    }

    html! {
        <div style="flex: 1;" ref={left_ref.clone()}>
            {props.children.clone()}
        </div>
    }
}

#[derive(Properties, PartialEq)]
pub struct SplitPaneProps {
    pub left: Html,
    pub right: Html,
}

#[styled_component]
pub fn SplitPane(props: &SplitPaneProps) -> Html {
    let left_width = use_state(|| None);


    let _left_width = left_width.clone();
    let slw = Callback::from(move |value: f64| {
        _left_width.set(Some(value));
    });

    html! {
        <div class={css!(r#"
            display: flex;
            flex-direction: row;
            height: 100%;
            width: 100%;
            align-items: stretch;
        "#)}>
            <LeftPane
                left_width={*left_width}
                set_left_width={slw}
            >
                {props.left.clone()}
            </LeftPane>
            <Divider />
            <div style="flex: 1;">
                {props.right.clone()}
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