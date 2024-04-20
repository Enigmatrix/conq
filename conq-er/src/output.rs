use stylist::yew::styled_component;
use yew::prelude::*;
use yew_autoprops::autoprops;

#[autoprops]
#[styled_component]
pub fn Output(on_run: Callback<MouseEvent>, children: &Children) -> Html {
    html! {
        <div class={css!(r#"
                display: flex;
                flex-direction: column;
                color: #f2f2f2;
                height: 100%;
                width: 100%;
            "#)}
        >
            <div class={css!(r#"
                background: #444;
                display: flex;
                flex-direction: row;
                gap: 10px;
            "#)}>
                <input type="button" value="Run"
                    class={css!(r#"
                        background: #222;
                        color: #f2f2f2;
                        font-weight: bold;
                        border: none;
                        padding: 10px;
                        cursor: pointer;

                        &:hover {
                            background: #333;
                        }
                    "#)}
                    onclick={on_run}
                />
                <p style="color:#888;">{"or Press CTRL+Enter / Command+Enter"}</p>
            </div>
            <div class={css!(r#"
                background: #222;
                padding: 10px;
                box-sizing: border-box;
                flex: 1;
            "#)}>
                <pre>{children.clone()}</pre>
            </div>
        </div>
    }
}
