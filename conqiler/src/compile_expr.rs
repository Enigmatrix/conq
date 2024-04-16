use std::borrow::Borrow;

use melior::ir::ValueLike;
use melior::{dialect::*, ir};
use crate::compile::{Compiler, Environment};

use crate::ast::{ArithBinaryOp, CompareBinaryOp, Expr, Ident, LogicalBinaryOp, Stmt};

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

    fn assert_int_type(&self, v: ir::Value<'c, '_>) {
        if !v.r#type().eq(&self.int_type()) {
            panic!("expected i64, got {v}");
        }
    }

    fn assert_bool_type(&self, v: ir::Value<'c, '_>) {
        if !v.r#type().eq(&self.bool_type()) {
            panic!("expected bool, got {v}");
        }
    }
    
    pub fn compile_literal_int<'a>(&self, block: &'a ir::Block<'c>, i: i64) -> ir::Value<'c, 'a> {
        val(block.append_operation(arith::constant(
            &self.ctx,
            ir::attribute::IntegerAttribute::new(
                self.int_type(),
                i).into(),
            self.loc)))
    }
    
    pub fn compile_literal_bool<'a>(&self, block: &'a ir::Block<'c>, b: bool) -> ir::Value<'c, 'a> {
        val(block.append_operation(arith::constant(
            &self.ctx,
            ir::attribute::IntegerAttribute::new(
                self.bool_type(),
                if b { 1 } else { 0 }).into(),
            self.loc)))
    }

    pub fn compile_unary_not<'a>(&self, env: &mut Environment<'c, 'a>, expr: Expr) -> ir::Value<'c, 'a> {
        let btrue = self.compile_literal_bool(env.block(), true);
        let v = self.compile_expr(env, expr);
        self.assert_bool_type(v);
        val(env.block().append_operation(arith::xori(
            v,
            btrue,
            self.loc)))
    }

    pub fn compile_unary_neg<'a>(&self, env: &mut Environment<'c, 'a>, expr: Expr) -> ir::Value<'c, 'a> {
        let ineg1 = self.compile_literal_int(env.block(), -1);
        let v = self.compile_expr(env, expr);
        self.assert_int_type(v);
        val(env.block().append_operation(arith::muli(
            v,
            ineg1,
            self.loc)))
    }


    pub fn compile_block<'a>(&self, env: &mut Environment<'c, 'a>, new_block: &'a ir::Block<'c>, stmts: Vec<Stmt>, expr: Expr) -> ir::Value<'c, 'a> {
        let mut env = env.extend(new_block);
        for stmt in stmts {
            self.compile_stmt(&mut env, stmt);
        }
        let val = self.compile_expr(&mut env, expr);
        val
    }

    pub fn compile_expr_block_optim<'a>(&self, env: &mut Environment<'c, 'a>, expr: Expr) -> ir::Value<'c, 'a> {
        match expr {
            Expr::Body{ stmts, val } => {
                // we aren't creating a new env here
                for stmt in stmts {
                    self.compile_stmt(env, stmt);
                }
                let val = self.compile_expr(env, *val);
                val
            },
            expr => self.compile_expr(env, expr)
        }
    }
    
    pub fn compile_arith_binary<'a>(&self, env: &mut Environment<'c, 'a>, op: ArithBinaryOp, lhs: Expr, rhs: Expr) -> ir::Value<'c, 'a> {
        let lhs = self.compile_expr(env, lhs);
        let rhs = self.compile_expr(env, rhs);
        self.assert_int_type(lhs);
        self.assert_int_type(rhs);
        val(env.block().append_operation(match op {
            ArithBinaryOp::Add => arith::addi(lhs, rhs, self.loc),
            ArithBinaryOp::Sub => arith::subi(lhs, rhs, self.loc),
            ArithBinaryOp::Mul => arith::muli(lhs, rhs, self.loc),
            ArithBinaryOp::Div => arith::divsi(lhs, rhs, self.loc),
            ArithBinaryOp::Mod => arith::remsi(lhs, rhs, self.loc),
        }))
    }
    
    pub fn compile_compare_binary<'a>(&self, env: &mut Environment<'c, 'a>, op: CompareBinaryOp, lhs: Expr, rhs: Expr) -> ir::Value<'c, 'a> {
        let lhs = self.compile_expr(env, lhs);
        let rhs = self.compile_expr(env, rhs);
        self.assert_int_type(lhs);
        self.assert_int_type(rhs);
        val(env.block().append_operation(match op {
            CompareBinaryOp::Gt => arith::cmpi(&self.ctx, arith::CmpiPredicate::Sgt, lhs, rhs, self.loc),
            CompareBinaryOp::Lt => arith::cmpi(&self.ctx, arith::CmpiPredicate::Slt, lhs, rhs, self.loc),
            CompareBinaryOp::Gte => arith::cmpi(&self.ctx, arith::CmpiPredicate::Sge, lhs, rhs, self.loc),
            CompareBinaryOp::Lte => arith::cmpi(&self.ctx, arith::CmpiPredicate::Sle, lhs, rhs, self.loc),
            CompareBinaryOp::Eq => arith::cmpi(&self.ctx, arith::CmpiPredicate::Eq, lhs, rhs, self.loc),
            CompareBinaryOp::Neq => arith::cmpi(&self.ctx, arith::CmpiPredicate::Ne, lhs, rhs, self.loc),
        }))
    }
    
    pub fn compile_logical_binary<'a>(&self, env: &mut Environment<'c, 'a>, op: LogicalBinaryOp, lhs: Expr, rhs: Expr) -> ir::Value<'c, 'a> {
        let lhs = self.compile_expr(env, lhs);
        let rhs = self.compile_expr(env, rhs);
        self.assert_bool_type(lhs);
        self.assert_bool_type(rhs);
        val(env.block().append_operation(match op {
            LogicalBinaryOp::Or => todo!(), // need cond for tihs
            LogicalBinaryOp::And => todo!(),
            // TODO Xor
        }))
    }
    
    
    pub fn compile_expr<'a>(&self, env: &mut Environment<'c, 'a>, expr: Expr) -> ir::Value<'c, 'a> {
        match expr {
            Expr::Binary { op, lhs, rhs } => match op {
                crate::ast::BinaryOp::Arith(op) => self.compile_arith_binary(env, op, *lhs, *rhs),
                crate::ast::BinaryOp::Compare(op) => self.compile_compare_binary(env, op, *lhs, *rhs),
                crate::ast::BinaryOp::Logical(op) => self.compile_logical_binary(env, op, *lhs, *rhs),
            },
            Expr::Unary { op, expr } => match op {
                crate::ast::UnaryOp::Neg => self.compile_unary_neg(env, *expr),
                crate::ast::UnaryOp::Not => self.compile_unary_not(env, *expr),
            },
            Expr::Literal(v) => match v {
                crate::ast::Value::Int(i) => self.compile_literal_int(env.block(), i),
                crate::ast::Value::Bool(b) => self.compile_literal_bool(env.block(), b),
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
    
    use crate::{ast::Expr, compile::{tests::setup_ctx, Compiler, Environment}};

    #[test]
    fn test_literal_int() {
        let ctx = setup_ctx();
        let compiler = Compiler::new(&ctx);
        let global_body = compiler.module.body();
        let mut env = Environment::new(&global_body);
        compiler.compile_expr(&mut env, Expr::Literal(crate::ast::Value::Int(1)));
        assert!(compiler.module.as_operation().verify());
        compiler.module.as_operation().dump();
    }

    #[test]
    fn test_literal_bool() {
        let ctx = setup_ctx();
        let compiler = Compiler::new(&ctx);
        let global_body = compiler.module.body();
        let mut env = Environment::new(&global_body);
        compiler.compile_expr(&mut env, Expr::Literal(crate::ast::Value::Bool(true)));
        assert!(compiler.module.as_operation().verify());
        compiler.module.as_operation().dump();
    }
}