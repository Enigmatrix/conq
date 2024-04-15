use std::collections::HashMap;

use melior::{dialect::*, ir::{self, Block, Module, Region}, Context};

use crate::ast::{Expr, Stmt};

pub struct Frame<'c, 'a> {
    inner: HashMap<String, ir::Value<'c, 'a>>,
    block: &'a ir::Block<'a>,
}

impl<'c, 'a> Frame<'c, 'a> {
    pub fn new(block: &'a ir::Block<'a>) -> Self {
        Self {
            inner: HashMap::new(),
            block
        }
    }
}

pub struct Environment<'c, 'a> {
    frames: Vec<Frame<'c, 'a>>,
}

impl<'c, 'a> Environment<'c, 'a> {
    pub fn new(block: &'a ir::Block<'a>) -> Self {
        Self {
            frames: vec![Frame::<'c, 'a>::new(block)], // global frame
        }
    }
    
    pub fn block(&self) -> &'a ir::Block<'a> {
        self.frames.last().unwrap().block
    }
    
    pub fn extend(&mut self, block: &'a ir::Block<'a>) {
        self.frames.push(Frame::<'c, 'a>::new(block));
    }
    
    pub fn pop(&mut self) {
        self.frames.pop();
    }
}

pub struct Compiler<'a> {
    pub ctx: &'a Context,
    pub loc: ir::Location<'a>,
    pub module: ir::Module<'a>,
}

impl<'c> Compiler<'c> {
    fn new(ctx: &'c Context) -> Self {
        let loc = ir::Location::unknown(&ctx);
        let module = ir::Module::new(loc);

        Self { ctx, loc, module }
    }
    
    fn compile_expr_block_optim(&self, env: &mut Environment<'c, 'c>, region: &Region<'c>, expr: Expr) {
        match expr {
            Expr::Body{ stmts, val } => {

            },
            expr => todo!(),
        }
    }
    
    pub fn compile_stmt(&'c self, env: &mut Environment<'c, 'c>, stmt: Stmt) {
        match stmt {
            Stmt::Expr(_) => todo!(),
            Stmt::Let { name, expr } => todo!(),
            Stmt::Break => todo!(),
            Stmt::Continue => todo!(),
            Stmt::Return(_) => todo!(),
            Stmt::While { pred, body } => {
                let before_region = {
                    let loc = self.loc.clone();
                    let region = ir::Region::new();
                    let block = region.append_block(ir::Block::new(&[]));
                    let val = self.compile_expr(env, *pred);
                    block.append_operation(scf::condition(val, &[], loc));
                    region
                };
                let after_region = {
                    let region = ir::Region::new();
                    let block = region.append_block(ir::Block::new(&[]));
                    block.append_operation(scf::r#yield(&[], self.loc));
                    region
                };

                let whl = scf::r#while(&[], &[], before_region, after_region, self.loc);
                env.block().append_operation(whl);
            },
            Stmt::Fn { name, params, expr } => todo!(),
        }
    }
}


#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use melior::{dialect::DialectRegistry, ir::{Block, Operation}, utility::register_all_dialects};

    use crate::{ast::Value};

    use super::*;

    #[test]
    fn test_name() {
        let registry = DialectRegistry::new();
        register_all_dialects(&registry);
        let ctx = Context::new();
        ctx.append_dialect_registry(&registry);
        ctx.load_all_available_dialects();

        let compiler = Compiler::new(&ctx);
        let blkref = compiler.module.body();
        let mut env = Environment::new(&blkref);
        compiler.compile_stmt(&mut env, Stmt::While {
            pred: Box::new(Expr::Unary{ op: crate::ast::UnaryOp::Not, expr: Box::new(Expr::Literal(Value::Bool(true)))}),
            body: Box::new(Expr::Body { stmts: vec![], val: Box::new(Expr::Literal(Value::Int(1))) }),
        });
        assert!(compiler.module.as_operation().verify());
        compiler.module.as_operation().dump();
    }
}