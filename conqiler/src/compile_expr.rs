use std::any::Any;

use melior::ir::{TypeLike, ValueLike};
use melior::{dialect::*, ir, Context};
use crate::compile::Compiler;

use crate::ast::{Expr, Ident, Stmt};

fn val<'a, 'i>(x: ir::OperationRef<'a, 'i>) -> ir::Value<'a, 'i> {
    x.result(0)
        .unwrap_or_else(|_| panic!("this operation has no value: {x}"))
        .into()
}

impl<'a> Compiler<'a> {
    
    fn int_type(&self) -> ir::Type<'a> {
        ir::r#type::IntegerType::new(&self.ctx, 64).into()
    }

    fn bool_type(&self) -> ir::Type<'a> {
        ir::r#type::IntegerType::new(&self.ctx, 1).into()
    }
    
    pub fn compile_literal_int(&self, cur_block: &'a ir::Block<'a>, i: i64) -> ir::Value<'a, 'a> {
        val(cur_block.append_operation(arith::constant(
            &self.ctx,
            ir::attribute::IntegerAttribute::new(
                self.int_type(),
                i).into(),
            self.loc)))
    }
    
    pub fn compile_literal_bool(&self, cur_block: &'a ir::Block<'a>, b: bool) -> ir::Value<'a, 'a> {
        val(cur_block.append_operation(arith::constant(
            &self.ctx,
            ir::attribute::IntegerAttribute::new(
                self.bool_type(),
                if b { 1 } else { 0 }).into(),
            self.loc)))
    }

    pub fn compile_unary_not(&self, cur_block: &'a ir::Block<'a>, expr: Expr) -> ir::Value<'a, 'a> {
        let v = self.compile_expr(cur_block, expr);
        if !v.r#type().eq(&self.bool_type()) {
            panic!("expected bool, got {v}");
        }
        val(cur_block.append_operation(arith::xori(
            v,
            self.compile_literal_bool(cur_block, true),
            self.loc)))
    }

    pub fn compile_unary_neg(&self, cur_block: &'a ir::Block<'a>, expr: Expr) -> ir::Value<'a, 'a> {
        let v = self.compile_expr(cur_block, expr);
        if !v.r#type().eq(&self.int_type()) {
            panic!("expected i64, got {v}");
        }
        val(cur_block.append_operation(arith::muli(
            v,
            self.compile_literal_int(cur_block, -1),
            self.loc)))
    }
    
    
    pub fn compile_expr(&self, cur_block: &'a ir::Block<'a>, expr: Expr) -> ir::Value<'a, 'a> {
        match expr {
            Expr::Binary { op, lhs, rhs } => match op {
                crate::ast::BinaryOp::Arith(_) => todo!(),
                crate::ast::BinaryOp::Compare(_) => todo!(),
                crate::ast::BinaryOp::Logical(_) => todo!(),
            },
            Expr::Unary { op, expr } => match op {
                crate::ast::UnaryOp::Not => self.compile_unary_not(cur_block, *expr),
                crate::ast::UnaryOp::Neg => self.compile_unary_neg(cur_block, *expr),
            },
            Expr::Literal(v) => match v {
                crate::ast::Value::Int(i) => self.compile_literal_int(cur_block, i),
                crate::ast::Value::Bool(b) => self.compile_literal_bool(cur_block, b),
                crate::ast::Value::String(_) => todo!(),
                crate::ast::Value::Void => todo!(),
            },
            Expr::Ident(Ident(name)) => {
                todo!()
            },
            Expr::Assign { name, expr } => todo!(),
            Expr::Body { stmts, val } => todo!(),
            Expr::Cond { pred, conseq, alt } => todo!(),
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