use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
};

use melior::{
    dialect::{arith::constant, func, DialectRegistry},
    ir::{
        attribute::IntegerAttribute,
        block::{self, Block},
        r#type::{IntegerType, Type},
        Location, Module, Region,
    },
    utility::{register_all_dialects, register_all_llvm_translations},
    Context, ExecutionEngine,
};

use crate::ast::*;

use anyhow::Result;

struct CodeGen<'ctx, 'module> {
    context: &'ctx Context,
    module: &'module Module<'ctx>,
    annon_string_counter: RefCell<usize>,
    // type_store: BTreeMap<Ident, Expr>,
    // var_to_type: HashMap<Ident, Expr>,
}

impl<'ctx, 'module> CodeGen<'ctx, 'module> {
    fn gen_ast_code(&'ctx self, ast: Ast, _emit_mlir: bool) -> Result<()> {
        let location = Location::unknown(&self.context);

        match ast {
            Ast::Expr(expr) => {
                let block = Block::new(&[(Type::index(&self.context), location)]);
                let region = Region::new();

                let value = self.gen_expr_code(expr, &block)?.unwrap();
                block.append_operation(func::r#return(&[value], location));

                // todo append block
            }
            _ => todo!(),
        };

        Ok(())
    }

    fn gen_expr_code<'a>(
        &'ctx self,
        expr: Expr,
        block: &'a Block<'ctx>,
    ) -> Result<Option<melior::ir::Value<'ctx, 'a>>> {
        let location = Location::unknown(&self.context);

        let val = match expr {
            Expr::Literal(Value::Int(val)) => Some(
                block
                    .append_operation(constant(
                        &self.context,
                        IntegerAttribute::new(
                            IntegerType::new(&self.context, 64).into(),
                            val as i64,
                        )
                        .into(),
                        location,
                    ))
                    .result(0)?
                    .into(),
            ),
            _ => todo!(),
        };
        Ok(val)
    }
}

pub fn gen_mlir<'c>(ast: Ast, emit: bool) -> ExecutionEngine {
    let registry = DialectRegistry::new();
    register_all_dialects(&registry);

    let context = Context::new();
    context.append_dialect_registry(&registry);
    context.load_all_available_dialects();
    register_all_llvm_translations(&context);

    context.attach_diagnostic_handler(|diagnostic| {
        eprintln!("{}", diagnostic);
        true
    });

    // let index_type = Type::index(&context);

    // module.body().append_operation(func::func(
    //     &context,
    //     StringAttribute::new(&context, "add"),
    //     TypeAttribute::new(
    //         FunctionType::new(&context, &[index_type, index_type], &[index_type]).into(),
    //     ),
    //     {
    //         let block = Block::new(&[(index_type, location), (index_type, location)]);

    //         let sum = block.append_operation(arith::addi(
    //             block.argument(0).unwrap().into(),
    //             block.argument(1).unwrap().into(),
    //             location,
    //         ));

    //         block.append_operation(func::r#return(&[sum.result(0).unwrap().into()], location));

    //         let region = Region::new();
    //         region.append_block(block);
    //         region
    //     },
    //     &[],
    //     location,
    // ));

    let mut module = Module::new(melior::ir::Location::unknown(&context));

    let code_gen = CodeGen {
        context: &context,
        module: &module,
        annon_string_counter: RefCell::new(0),
        // type_store: BTreeMap::new(),
        // var_to_type: HashMap::new(),
    };

    code_gen.gen_ast_code(ast, emit).unwrap();

    if !emit {
        assert!(module.as_operation().verify());
    }

    if emit {
        println!("{}", module.as_operation());
    }

    let engine = ExecutionEngine::new(&module, 0, &[], true);
    engine
}
