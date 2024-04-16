use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, ops::DerefMut, rc::Rc, sync::Arc};

use melior::{dialect::*, ir::{self, block, r#type::MemRefType, ValueLike}, Context};

use crate::{ast::{Expr, Ident, Stmt}, compile_expr::val};

pub struct Frame<'c, 'a> {
    inner: HashMap<String, ir::Value<'c, 'a>>,
    block: &'a ir::Block<'c>,
}

impl<'c, 'a> Frame<'c, 'a> {
    pub fn new(block: &'a ir::Block<'c>) -> Self {
        Self {
            inner: HashMap::new(),
            block
        }
    }

    pub fn decl(&mut self, name: String, val: ir::Value<'c, 'a>) -> bool {
        self.inner.insert(name, val).is_none()
    }
}

pub struct Environment<'c, 'a> {
    frames: Vec<Rc<Frame<'c, 'a>>>,
}

impl<'c, 'a> Environment<'c, 'a> {
    pub fn new(block: &'a ir::Block<'c>) -> Self {
        Self {
            frames: vec![Rc::new(Frame::new(block))], // global frame
        }
    }
    
    pub fn extend<'a2: 'a>(&self, block: &'a2 ir::Block<'c>) -> Self {
        let mut frames = self.frames.clone();
        frames.push(Rc::new(Frame::new(block)));
        Self { frames }
    }
    
    pub fn block(&self) -> &'a ir::Block<'c> {
        self.frames.last().unwrap().block
    }

    pub fn decl(&mut self, name: String, val: ir::Value<'c, 'a>) -> bool {
        let rc = self.frames.last_mut().unwrap();
        // this is safe since only the last block (which has no other owners) is being mutated
        Rc::get_mut(rc).unwrap().deref_mut().decl(name, val)
    }
}

pub struct Compiler<'a> {
    pub ctx: &'a Context,
    pub loc: ir::Location<'a>,
    pub module: ir::Module<'a>,
}

impl<'c> Compiler<'c> {
    pub fn new(ctx: &'c Context) -> Self {
        let loc = ir::Location::unknown(&ctx);
        let module = ir::Module::new(loc);

        Self { ctx, loc, module }
    }
    
    pub fn compile_stmt<'a>(&self, env: &mut Environment<'c, 'a>, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                let _ = self.compile_expr(env, expr);
            },
            Stmt::Let { name: Ident(name), expr } => {
                let v = self.compile_expr(env, *expr);
                let alloca = memref::alloca(&self.ctx, MemRefType::new(v.r#type(), &[], None, None), &[], &[], None, self.loc);
                let alloc_v = val(env.block().append_operation(alloca));
                if !env.decl(name.clone(), alloc_v) {
                    panic!("redeclared variable {name}");
                }
                let store_v = memref::store(v, alloc_v, &[], self.loc);
                env.block().append_operation(store_v);
            },
            Stmt::Break => todo!(),
            Stmt::Continue => todo!(),
            Stmt::Return(expr) => {
                let v = self.compile_expr(env, *expr);
                env.block().append_operation(func::r#return(&[v], self.loc));
            },
            Stmt::While { pred, body } => {
                let before_region = {
                    let loc = self.loc.clone();
                    let region = ir::Region::new();
                    let block = region.append_block(ir::Block::new(&[]));
                    let mut env = env.extend(&block);
                    let val = self.compile_expr_block_optim(&mut env, *pred);
                    block.append_operation(scf::condition(val, &[], loc));
                    region
                };
                let after_region = {
                    let region = ir::Region::new();
                    let block = region.append_block(ir::Block::new(&[]));
                    let mut env = env.extend(&block);
                    let _val = self.compile_expr_block_optim(&mut env, *body);
                    block.append_operation(scf::r#yield(&[], self.loc));
                    region
                };

                let whl = scf::r#while(&[], &[], before_region, after_region, self.loc);
                env.block().append_operation(whl);
            },
            Stmt::Fn { name: Ident(name), params, expr } => {
                let arg_types = params.iter().map(|_| self.int_type()).collect::<Vec<_>>();
                let arg_types_with_loc = arg_types.iter().cloned().map(|t| (t, self.loc)).collect::<Vec<_>>();

                // TODO currently only int params
                // TODO currently int return (not even void)
                let fn_type = ir::attribute::TypeAttribute::new(ir::r#type::FunctionType::new(&self.ctx, &arg_types, &[self.int_type()]).into());
                let fn_attr = ir::attribute::StringAttribute::new(&self.ctx, &name.clone());
                let fn_op = func::func(&self.ctx, fn_attr, fn_type, {
                    let region = ir::Region::new();
                    let block = region.append_block(ir::Block::new(&arg_types_with_loc));
                    let mut env = env.extend(&block);
                    let mut i = 0;
                    for Ident(p) in params {
                        let v = block.argument(i).unwrap().into();
                        if !env.decl(p.clone(), v) {
                            panic!("redeclared argument {p}");
                        }
                        i+=1;
                    }
                    let val = self.compile_expr_block_optim(&mut env, *expr);
                    // TODO temporary return
                    block.append_operation(func::r#return(&[val], self.loc));
                    region
                }, &[], self.loc);
                env.block().append_operation(fn_op);
            },
        }
    }
}


#[cfg(test)]
pub mod tests {
    use std::{io::Read, ops::Deref};

    use melior::{dialect::DialectRegistry, ir::{Block, Operation}, utility::{register_all_dialects, register_all_llvm_translations}};

    use crate::{ast::Value, translate};

    use super::*;

    pub fn setup_ctx() -> Context {
        let registry = DialectRegistry::new();
        register_all_dialects(&registry);
        let ctx = Context::new();
        ctx.append_dialect_registry(&registry);
        ctx.load_all_available_dialects();
        register_all_llvm_translations(&ctx);
        ctx
    }

    #[test]
    fn test_name() {
        let ctx = setup_ctx();

        let mut compiler = Compiler::new(&ctx);
        let blkref = compiler.module.body();
        let mut env = Environment::new(&blkref);
        compiler.compile_stmt(&mut env, Stmt::While {
            pred: Box::new(Expr::Unary{ op: crate::ast::UnaryOp::Not, expr: Box::new(Expr::Literal(Value::Bool(true)))}),
            body: Box::new(Expr::Body { stmts: vec![], val: Box::new(Some(Expr::Literal(Value::Int(1)))) }),
        });
        assert!(compiler.module.as_operation().verify());
        compiler.module.as_operation().dump();

        let mut wasm_file = translate::compile_program_text(&ctx, &mut compiler.module).unwrap();
        let mut wasm = String::new();
        wasm_file.read_to_string(&mut wasm).unwrap();

        eprintln!("{}", wasm);
    }


    #[test]
    fn test_decl_int() {
        let ctx = setup_ctx();

        let mut compiler = Compiler::new(&ctx);
        let blkref = compiler.module.body();
        let mut env = Environment::new(&blkref);
        let let_a = Stmt::Let { name: Ident("a".to_string()), expr: Box::new(Expr::Literal(Value::Int(1))) };
        let ret_32 = Stmt::Return(Box::new(Expr::Literal(Value::Int(32))));
        compiler.compile_stmt(&mut env,  Stmt::Fn { name: Ident("_start".to_string()), params: vec![Ident("b".to_string())], expr: Box::new(Expr::Body { stmts: vec![let_a, ret_32], val: None }) });
        assert!(compiler.module.as_operation().verify());
        compiler.module.as_operation().dump();

        let mut output = translate::compile_program_text(&ctx, &mut compiler.module).unwrap();
        let mut o = String::new();
        output.read_to_string(&mut o).unwrap();


        eprintln!("{}", o);
        let mut output = translate::compile_and_run_program(&ctx, &mut compiler.module).unwrap();
        eprintln!("{:?}", output);
    }
}