use std::any::Any;

use melior::ir::{Block, TypeLike, ValueLike};
use melior::{dialect::*, ir, Context};
use crate::compile::{Compiler, Environment};

use crate::ast::{Expr, Ident, Stmt};

fn val<'a, 'i>(x: ir::OperationRef<'a, 'i>) -> ir::Value<'a, 'i> {
    x.result(0)
        .unwrap_or_else(|_| panic!("this operation has no value: {x}"))
        .into()
}

impl<'c> Compiler<'c> {
    
    fn int_type(&self) -> ir::Type<'c> {
        ir::r#type::IntegerType::new(&self.ctx, 64).into()
    }

    fn bool_type(&self) -> ir::Type<'c> {
        ir::r#type::IntegerType::new(&self.ctx, 1).into()
    }
    
    pub fn compile_literal_int(&self, cur_block: &mut Environment<'c, 'c>, i: i64) -> ir::Value<'c, 'c> {
        val(cur_block.block().append_operation(arith::constant(
            &self.ctx,
            ir::attribute::IntegerAttribute::new(
                self.int_type(),
                i).into(),
            self.loc)))
    }
    
    pub fn compile_literal_bool(&self, cur_block: &mut Environment<'c, 'c>, b: bool) -> ir::Value<'c, 'c> {
        val(cur_block.block().append_operation(arith::constant(
            &self.ctx,
            ir::attribute::IntegerAttribute::new(
                self.bool_type(),
                if b { 1 } else { 0 }).into(),
            self.loc)))
    }

    pub fn compile_unary_not(&self, env: &mut Environment<'c, 'c>, expr: Expr) -> ir::Value<'c, 'c> {
        let btrue = self.compile_literal_bool(env, true);
        let v = self.compile_expr(env, expr);
        if !v.r#type().eq(&self.bool_type()) {
            panic!("expected bool, got {v}");
        }
        val(env.block().append_operation(arith::xori(
            v,
            btrue,
            self.loc)))
    }

    pub fn compile_unary_neg(&self, env: &mut Environment<'c, 'c>, expr: Expr) -> ir::Value<'c, 'c> {
        let ineg1 = self.compile_literal_int(env, -1);
        let v = self.compile_expr(env, expr);
        if !v.r#type().eq(&self.int_type()) {
            panic!("expected i64, got {v}");
        }
        val(env.block().append_operation(arith::muli(
            v,
            ineg1,
            self.loc)))
    }


    pub fn compile_block(&self, env: &mut Environment<'c, 'c>, stmts: Vec<Stmt>, expr: Expr) -> ir::Block<'c> {
        let block = Block::new(&[]);
        env.extend(&block);
        for stmt in stmts {
            self.compile_stmt(env, stmt);
        }
        let _val = self.compile_expr(env, expr);
        env.pop();
        block
    }
    
    
    pub fn compile_expr<'a>(&self, env: &mut Environment<'c, 'c>, expr: Expr) -> ir::Value<'c, 'c> {
        match expr {
            Expr::Binary { op, lhs, rhs } => match op {
                crate::ast::BinaryOp::Arith(_) => todo!(),
                crate::ast::BinaryOp::Compare(_) => todo!(),
                crate::ast::BinaryOp::Logical(_) => todo!(),
            },
            Expr::Unary { op, expr } => match op {
                crate::ast::UnaryOp::Not => self.compile_unary_not(env, *expr),
                crate::ast::UnaryOp::Neg => self.compile_unary_neg(env, *expr),
            },
            Expr::Literal(v) => match v {
                crate::ast::Value::Int(i) => self.compile_literal_int(env, i),
                crate::ast::Value::Bool(b) => self.compile_literal_bool(env, b),
                crate::ast::Value::String(_) => todo!(),
                crate::ast::Value::Void => todo!(),
            },
            Expr::Ident(Ident(name)) => {
                todo!()
            },
            Expr::Assign { name, expr } => todo!(),
            Expr::Body{ stmts, val } => todo!(),
            Expr::Cond{ pred, conseq, alt } => todo!(),
            Expr::Apply { r#fn, args } => todo!(),
        }
    }
}


#[cfg(test)]
mod tests {

    #[test]
    fn test_name() {

    }
}