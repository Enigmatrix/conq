// use melior::{
//     dialect::{arith, func, DialectRegistry},
//     ir::{
//         attribute::{StringAttribute, TypeAttribute},
//         r#type::FunctionType,
//         *,
//     },
//     utility::register_all_dialects,
//     Context,
// };

use codegen::gen_mlir;
use parser::parse;

mod ast;
mod codegen;
mod parser;

fn main() {
    let _ = gen_mlir(
        parse("fn main() { let x = 1; }"),
        true,
    );
}
