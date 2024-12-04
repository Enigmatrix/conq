use monaco::{
    api::{CodeEditorOptions, TextModel},
    sys::editor::BuiltinTheme,
    yew::{CodeEditor, CodeEditorLink},
};
use yew::prelude::*;

use crate::{monaco_conq, CONTENT};

fn get_options() -> CodeEditorOptions {
    CodeEditorOptions::default()
        .with_language(monaco_conq::ID.to_string())
        .with_value(CONTENT.to_owned())
        .with_builtin_theme(BuiltinTheme::VsDark)
        .with_automatic_layout(true)
}

#[derive(PartialEq, Properties)]
pub struct EditorProps {
    pub on_editor_created: Callback<CodeEditorLink>,
    pub text_model: TextModel,
}

#[function_component]
pub fn Editor(props: &EditorProps) -> Html {
    let EditorProps {
        on_editor_created,
        text_model,
    } = props;

    html! {
        <CodeEditor classes={"full"} options={ get_options().to_sys_options() } {on_editor_created} model={text_model.clone()} />
    }
}
