use std::borrow::Borrow;

use melior::ir::{block, ValueLike};
use melior::{dialect::*, ir};
use crate::compile::{Compiler, Environment};

use crate::ast::{ArithBinaryOp, CompareBinaryOp, Expr, Ident, LogicalBinaryOp, Stmt, Value};

pub fn val<'a, 'i>(x: ir::OperationRef<'a, 'i>) -> ir::Value<'a, 'i> {
    x.result(0)
        .unwrap_or_else(|_| panic!("this operation has no value: {x}"))
        .into()
}

impl<'c> Compiler<'c> {
    
    pub fn int_type(&self) -> ir::Type<'c> {
        ir::r#type::IntegerType::new(&self.ctx, 64).into()
    }

    fn bool_type(&self) -> ir::Type<'c> {
        ir::r#type::IntegerType::new(&self.ctx, 1).into()
    }

    pub fn int_ref_type(&self) -> ir::Type<'c> {
        ir::r#type::MemRefType::new(self.int_type(), &[], None, None).into()
    }

    pub fn bool_ref_type(&self) -> ir::Type<'c> {
        ir::r#type::MemRefType::new(self.bool_type(), &[], None, None).into()
    }

    fn assert_int_ref_type(&self, v: ir::Value<'c, '_>) {
        if !v.r#type().eq(&self.int_ref_type()) {
            panic!("expected i64, got {v}");
        }
    }

    fn assert_bool_ref_type(&self, v: ir::Value<'c, '_>) {
        if !v.r#type().eq(&self.bool_ref_type()) {
            panic!("expected bool, got {v}");
        }
    }
    
    pub fn alloc<'a>(&self, env: &mut Environment<'c, 'a>, inner_typ: ir::Type<'c>) -> ir::Value<'c, 'a> {
        let alloca = memref::alloc(&self.ctx, ir::r#type::MemRefType::new(inner_typ, &[], None, None), &[], &[], None, self.loc);
        val(env.block().append_operation(alloca))
    }
    
    pub fn store<'a>(&self, env: &mut Environment<'c, 'a>, memref: ir::Value<'c, 'a>, v: ir::Value<'c, '_>) {
        env.block().append_operation(memref::store(v, memref, &[], self.loc));
    }
    
    pub fn alloc_and_store<'a>(&self, env: &mut Environment<'c, 'a>, v: ir::Value<'c, 'a>, inner_typ: ir::Type<'c>) -> ir::Value<'c, 'a> {
        let alloc_ref = self.alloc(env, inner_typ);
        self.store(env, alloc_ref, v);
        alloc_ref
    }
    
    pub fn load<'a>(&self, env: &mut Environment<'c, 'a>, memref: ir::Value<'c, 'a>) -> ir::Value<'c, 'a> {
        let load = memref::load(memref, &[], self.loc);
        let load_ref = val(env.block().append_operation(load));
        load_ref
    }
    
    pub fn compile_literal_int<'a>(&self, env: &mut Environment<'c, 'a>, i: i64) -> ir::Value<'c, 'a> {
        let cnst = val(env.block().append_operation(arith::constant(
            &self.ctx,
            ir::attribute::IntegerAttribute::new(
                self.int_type(),
                i).into(),
            self.loc)));
        self.alloc_and_store(env, cnst, self.int_type())
    }
    
    pub fn compile_literal_bool<'a>(&self, env: &mut Environment<'c, 'a>, b: bool) -> ir::Value<'c, 'a> {
        let cnst = val(env.block().append_operation(arith::constant(
            &self.ctx,
            ir::attribute::IntegerAttribute::new(
                self.bool_type(),
                if b { 1 } else { 0 }).into(),
            self.loc)));
        self.alloc_and_store(env, cnst, self.bool_type())
    }

    pub fn compile_unary_not<'a>(&self, env: &mut Environment<'c, 'a>, expr: Expr) -> ir::Value<'c, 'a> {
        let btrue = self.compile_literal_bool(env, true);
        let v = self.compile_expr_val(env, expr);
        self.assert_bool_ref_type(v);
        let v = val(env.block().append_operation(arith::xori(
            self.load(env, v),
            self.load(env, btrue),
            self.loc)));
        self.alloc_and_store(env, v, self.bool_type())
    }

    pub fn compile_unary_neg<'a>(&self, env: &mut Environment<'c, 'a>, expr: Expr) -> ir::Value<'c, 'a> {
        let ineg1 = self.compile_literal_int(env, -1);
        let v = self.compile_expr_val(env, expr);
        self.assert_int_ref_type(v);
        let v = val(env.block().append_operation(arith::muli(
            self.load(env, v),
            self.load(env, ineg1),
            self.loc)));
        self.alloc_and_store(env, v, self.int_type())
    }


    pub fn compile_block<'a>(&self, env: &mut Environment<'c, 'a>, new_block: &'a ir::Block<'c>, stmts: Vec<Stmt>, expr: Expr) -> Option<ir::Value<'c, 'a>> {
        let mut env = env.extend(new_block);
        for stmt in stmts {
            self.compile_stmt(&mut env, stmt);
        }
        if let Expr::Literal(Value::Void) = expr {
            return None
        }
        let val = self.compile_expr(&mut env, expr);
        val
    }

    pub fn compile_expr_block_optim<'a>(&self, env: &mut Environment<'c, 'a>, expr: Expr) -> Option<ir::Value<'c, 'a>> {
        match expr {
            Expr::Body{ stmts, val } => {
                // we aren't creating a new env here
                for stmt in stmts {
                    self.compile_stmt(env, stmt);
                }
                if let Expr::Literal(Value::Void) = *val {
                    return None
                }
                self.compile_expr(env, *val)
            },
            expr => self.compile_expr(env, expr)
        }
    }
    
    pub fn compile_arith_binary<'a>(&self, env: &mut Environment<'c, 'a>, op: ArithBinaryOp, lhs: Expr, rhs: Expr) -> ir::Value<'c, 'a> {
        let lhs = self.compile_expr_val(env, lhs);
        let rhs = self.compile_expr_val(env, rhs);
        self.assert_int_ref_type(lhs);
        self.assert_int_ref_type(rhs);
        let lhs = self.load(env, lhs);
        let rhs = self.load(env, rhs);
        let v = val(env.block().append_operation(match op {
            ArithBinaryOp::Add => arith::addi(lhs, rhs, self.loc),
            ArithBinaryOp::Sub => arith::subi(lhs, rhs, self.loc),
            ArithBinaryOp::Mul => arith::muli(lhs, rhs, self.loc),
            ArithBinaryOp::Div => arith::divsi(lhs, rhs, self.loc),
            ArithBinaryOp::Mod => arith::remsi(lhs, rhs, self.loc),
        }));
        self.alloc_and_store(env, v, self.int_type())
    }
    
    pub fn compile_compare_binary<'a>(&self, env: &mut Environment<'c, 'a>, op: CompareBinaryOp, lhs: Expr, rhs: Expr) -> ir::Value<'c, 'a> {
        let lhs = self.compile_expr_val(env, lhs);
        let rhs = self.compile_expr_val(env, rhs);
        self.assert_int_ref_type(lhs);
        self.assert_int_ref_type(rhs);
        let lhs = self.load(env, lhs);
        let rhs = self.load(env, rhs);
        let v = val(env.block().append_operation(match op {
            CompareBinaryOp::Gt => arith::cmpi(&self.ctx, arith::CmpiPredicate::Sgt, lhs, rhs, self.loc),
            CompareBinaryOp::Lt => arith::cmpi(&self.ctx, arith::CmpiPredicate::Slt, lhs, rhs, self.loc),
            CompareBinaryOp::Gte => arith::cmpi(&self.ctx, arith::CmpiPredicate::Sge, lhs, rhs, self.loc),
            CompareBinaryOp::Lte => arith::cmpi(&self.ctx, arith::CmpiPredicate::Sle, lhs, rhs, self.loc),
            CompareBinaryOp::Eq => arith::cmpi(&self.ctx, arith::CmpiPredicate::Eq, lhs, rhs, self.loc),
            CompareBinaryOp::Neq => arith::cmpi(&self.ctx, arith::CmpiPredicate::Ne, lhs, rhs, self.loc),
        }));
        self.alloc_and_store(env, v, self.int_type())
    }
    
    pub fn compile_logical_binary<'a>(&self, env: &mut Environment<'c, 'a>, op: LogicalBinaryOp, lhs: Expr, rhs: Expr) -> ir::Value<'c, 'a> {
        let lhs = self.compile_expr_val(env, lhs);
        let rhs = self.compile_expr_val(env, rhs);
        self.assert_bool_ref_type(lhs);
        self.assert_bool_ref_type(rhs);
        let lhs = self.load(env, lhs);
        let rhs = self.load(env, rhs);
        let v = val(env.block().append_operation(match op {
            LogicalBinaryOp::Or => todo!(), // need cond for tihs
            LogicalBinaryOp::And => todo!(),
            // TODO Xor
        }));
        self.alloc_and_store(env, v, self.bool_type())
    }

    pub fn compile_apply<'a>(&self, env: &mut Environment<'c, 'a>, r#fn: Expr, args: Vec<Expr>) -> Option<ir::Value<'c, 'a>> {
        let fn_val = self.compile_expr_val(env, r#fn);
        // eprintln!("fn_val: {fn_val}");
        let ctx = self.ctx;
        let b = env.block();
        let global = self.module.body();
        let global_region = global.parent_region();
        eprintln!("block: {b}, g: {global}, gr: {global_region:?}\n");
        
        // let fn_type = ir::r#type::FunctionType::new(ctx, &[self.int_ref_type(), self.int_ref_type()], &[self.int_ref_type()]);
        // let add = ir::attribute::FlatSymbolRefAttribute::new(ctx, "add");
        // eprintln!(", opadd: {add}");
        // eprintln!(", fntype: {fn_type}");
        // let op = func::constant(ctx, add, fn_type, self.loc);
        // //eprintln!(", op: {op}");
        // let fn_val = val(env.block().append_operation(op.clone()));
        //eprintln!(", op: {op}");
        //eprintln!("fn_val: {fn_val}, op: {op}");
        let args = args.into_iter().map(|arg| self.compile_expr_val(env, arg)).collect::<Vec<_>>();
        // TODO add assert
        // TODO check number of args
        let v = val(env.block().append_operation(func::call_indirect(fn_val, &args, &[self.int_ref_type()], self.loc)));
        // let v = val(env.block().append_operation(func::call(self.ctx, add, &args, &[self.int_ref_type()], self.loc)));
        Some(v)
    }
    
    pub fn compile_expr_val<'a>(&self, env: &mut Environment<'c, 'a>, expr: Expr) -> ir::Value<'c, 'a> {
        return self.compile_expr(env, expr).expect("expr value expected");
    }
    
    pub fn compile_expr<'a>(&self, env: &mut Environment<'c, 'a>, expr: Expr) -> Option<ir::Value<'c, 'a>> {
        match expr {
            Expr::Binary { op, lhs, rhs } => match op {
                crate::ast::BinaryOp::Arith(op) => Some(self.compile_arith_binary(env, op, *lhs, *rhs)),
                crate::ast::BinaryOp::Compare(op) => Some(self.compile_compare_binary(env, op, *lhs, *rhs)),
                crate::ast::BinaryOp::Logical(op) => Some(self.compile_logical_binary(env, op, *lhs, *rhs)),
            },
            Expr::Unary { op, expr } => match op {
                crate::ast::UnaryOp::Neg => Some(self.compile_unary_neg(env, *expr)),
                crate::ast::UnaryOp::Not => Some(self.compile_unary_not(env, *expr)),
            },
            Expr::Literal(v) => match v {
                crate::ast::Value::Int(i) => Some(self.compile_literal_int(env, i)),
                crate::ast::Value::Bool(b) => Some(self.compile_literal_bool(env, b)),
                crate::ast::Value::String(_) => todo!(),
                crate::ast::Value::Void => todo!(),
            },
            Expr::Ident(Ident(name)) => {
                let v = env.get(name.clone());
                if let Some(v) = v {
                    Some(v.clone())
                } else {
                    panic!("unknown variable {name}");
                }
            },
            Expr::Assign { name: Ident(name), expr } => {
                let expr_value = self.compile_expr_val(env, *expr);
                let v = env.get(name.clone()).cloned();
                if let Some(v) = v {
                    self.store(env, v.clone(), expr_value);
                    Some(v.clone())
                } else {
                    panic!("unknown variable {name}");
                }
            },
            // TODO create a completely new block for this -_-
            Expr::Body{ stmts, val } => self.compile_block(env, env.block(), stmts, *val),
            Expr::Cond{ pred, conseq, alt } => todo!(),
            Expr::Apply { r#fn, args } => self.compile_apply(env, *r#fn, args),
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