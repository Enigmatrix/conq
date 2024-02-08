use crate::env::Environment;
use crate::err::{ControlFlow, EvalError, Type};
use crate::expr::{ArithInfixOp, CompareInfixOp, Expr, InfixOp, LogicalInfixOp, UnaryOp};
use crate::instr::Instruction;
use crate::val::Value;

#[derive(Clone, Debug)]
pub enum Control {
    Instruction(Instruction),
    Expr(Expr),
}

pub struct Evaluator {
    control: Vec<Control>,
    stack: Vec<Value>,
    env: Environment,
}

impl Evaluator {
    pub fn new(env: Environment) -> Evaluator {
        Evaluator {
            control: Vec::new(),
            stack: Vec::new(),
            env,
        }
    }

    fn pop_stack(&mut self) -> Value {
        self.stack.pop().expect("empty stack")
    }

    fn pop_control(&mut self) -> Control {
        self.control.pop().expect("empty control")
    }

    fn eval_one(&mut self, control: Control) -> Result<(), EvalError> {
        match control {
            Control::Instruction(instr) => match instr {
                Instruction::Pop => {
                    self.pop_stack();
                }
                Instruction::PopEnv => {
                    self.env.pop();
                }
                Instruction::UnaryOp(op) => {
                    let val = self.pop_stack();
                    self.stack.push(match op {
                        UnaryOp::Not => Value::Bool(!val.to_bool()?),
                        UnaryOp::Neg => Value::Int(-val.to_int()?),
                    })
                }
                Instruction::InfixOp(op) => {
                    let rhs = self.pop_stack();
                    let lhs = self.pop_stack();
                    self.stack.push(eval_infix(op, lhs, rhs)?);
                }
                Instruction::Let(ident) => {
                    let val = self.pop_stack();
                    self.env.declare(ident, val.clone());
                    self.stack.push(val);
                }
                Instruction::Assign(ident) => {
                    let val = self.pop_stack();
                    if !self.env.assign(ident.clone(), val.clone()) {
                        return Err(EvalError::IdentNotFound { ident });
                    }
                    self.stack.push(val);
                }
                Instruction::Return => match self.pop_control() {
                    Control::Instruction(mark @ Instruction::Mark) => {
                        self.control.push(Control::Instruction(mark))
                    }
                    _ => self.control.push(Control::Instruction(Instruction::Return)),
                },
                Instruction::Break => match self.pop_control() {
                    Control::Instruction(Instruction::While { .. }) => (),
                    _ => self.control.push(Control::Instruction(Instruction::Break)),
                },
                Instruction::Continue => match self.pop_control() {
                    Control::Instruction(Instruction::While { pred, body }) => {
                        self.control.push(Control::Instruction(Instruction::While {
                            pred: pred.clone(),
                            body,
                        }));
                        self.control.push(Control::Expr(pred));
                        self.control.push(Control::Expr(Expr::Literal(Value::Void)));
                    }
                    _ => self.control.push(Control::Instruction(Instruction::Break)),
                },
                Instruction::Call { nargs } => {
                    let r#fn = self.pop_stack();
                    if let Value::Fn { params, expr } = r#fn {
                        if params.len() != nargs {
                            return Err(EvalError::IncorrectArgumentLength {
                                expected: params.len(),
                                actual: nargs,
                                r#fn: Value::Fn { params, expr },
                            });
                        }
                        self.env.extend();
                        params.into_iter().rev().for_each(|ident| {
                            let arg = self.pop_stack();
                            self.env.declare(ident, arg);
                        });
                        self.control.push(Control::Instruction(Instruction::Mark));
                        self.control.push(Control::Expr(*expr));
                    } else if let Value::PredefinedFn { r#impl } = r#fn {
                        let mut args = (0..nargs).map(|_| self.pop_stack()).collect::<Vec<_>>();
                        args.reverse();
                        self.stack.push(r#impl.call(args)?)
                    } else {
                        return Err(EvalError::TypeError {
                            expected: Type::Fn,
                            actual: r#fn,
                        });
                    }
                }
                Instruction::Mark => self.env.pop(),
                Instruction::Cond { conseq, alt } => {
                    if self.pop_stack().to_bool()? {
                        self.control.push(Control::Expr(conseq));
                    } else {
                        self.control.push(Control::Expr(alt));
                    }
                }
                Instruction::While { pred, body } => {
                    let should_continue = self.pop_stack();
                    let body_val = self.pop_stack();
                    if should_continue.to_bool()? {
                        self.control.push(Control::Instruction(Instruction::While {
                            pred: pred.clone(),
                            body: body.clone(),
                        }));
                        self.control.push(Control::Expr(pred));
                        self.control.push(Control::Expr(body));
                    } else {
                        self.stack.push(body_val);
                    }
                }
            },
            Control::Expr(expr) => match expr {
                Expr::Ident(ident) => {
                    let value =
                        self.env
                            .lookup(&ident)
                            .ok_or_else(|| EvalError::IdentNotFound {
                                ident: ident.clone(),
                            })?;
                    self.stack.push(value);
                }
                Expr::Literal(value) => self.stack.push(value),
                Expr::Unary { op, expr } => {
                    self.control
                        .push(Control::Instruction(Instruction::UnaryOp(op)));
                    self.control.push(Control::Expr(*expr));
                }
                Expr::Infix { op, lhs, rhs } => {
                    self.control
                        .push(Control::Instruction(Instruction::InfixOp(op)));
                    self.control.push(Control::Expr(*rhs)); // TODO short-circuit this
                    self.control.push(Control::Expr(*lhs));
                }
                Expr::Let { name, expr } => {
                    self.control
                        .push(Control::Instruction(Instruction::Let(name)));
                    self.control.push(Control::Expr(*expr));
                }
                Expr::Assign { name, expr } => {
                    self.control
                        .push(Control::Instruction(Instruction::Assign(name)));
                    self.control.push(Control::Expr(*expr));
                }
                Expr::Body(exprs) => {
                    self.env.extend();
                    exprs.into_iter().rev().enumerate().for_each(|(idx, expr)| {
                        if idx != 0 {
                            self.control.push(Control::Instruction(Instruction::Pop))
                        } else {
                            self.control.push(Control::Instruction(Instruction::PopEnv))
                        }
                        self.control.push(Control::Expr(expr))
                    });
                }
                Expr::Break => self.control.push(Control::Instruction(Instruction::Break)),
                Expr::Continue => self
                    .control
                    .push(Control::Instruction(Instruction::Continue)),
                Expr::Return(expr) => {
                    self.control.push(Control::Instruction(Instruction::Return));
                    self.control.push(Control::Expr(*expr));
                }
                Expr::Cond { pred, conseq, alt } => {
                    self.control.push(Control::Instruction(Instruction::Cond {
                        conseq: *conseq,
                        alt: *alt,
                    }));
                    self.control.push(Control::Expr(*pred));
                }
                Expr::While { pred, body } => {
                    self.control.push(Control::Instruction(Instruction::While {
                        pred: *pred.clone(),
                        body: *body,
                    }));
                    self.control.push(Control::Expr(*pred));
                    self.control.push(Control::Expr(Expr::Literal(Value::Void)));
                }
                Expr::Fn { name, params, expr } => {
                    let val = Value::Fn { params, expr };
                    self.env.declare(name, val.clone());
                    self.stack.push(val);
                }
                Expr::Apply { r#fn, args } => {
                    self.control.push(Control::Instruction(Instruction::Call {
                        nargs: args.len(),
                    }));
                    self.control.push(Control::Expr(*r#fn));
                    // evaluate arguments from left to right
                    args.into_iter()
                        .rev()
                        .for_each(|expr| self.control.push(Control::Expr(expr)));
                }
                Expr::Launch(expr) => return Err(EvalError::ControlFlow(ControlFlow::Launch { expr: *expr, env: self.env.clone() })),
                Expr::Yield => return Err(EvalError::ControlFlow(ControlFlow::Yield))
            },
        }
        Ok(())
    }

    pub fn eval(&mut self, expr: Expr) -> Result<Value, EvalError> {
        self.control.push(Control::Expr(expr));

        // println!("control: {:?}\n stack: {:?}\n", self.control, self.stack);
        while let Some(control) = self.control.pop() {
            self.eval_one(control)?;
            // println!("control: {:?}\n stack: {:?}\n", self.control, self.stack);
        }
        if self.stack.len() > 1 {
            panic!("stack remnants found");
        } else {
            Ok(self.pop_stack())
        }
    }
}

fn eval_infix(op: InfixOp, lhs: Value, rhs: Value) -> Result<Value, EvalError> {
    match op {
        InfixOp::Arith(arith) => {
            let lhs = lhs.to_int()?;
            let rhs = rhs.to_int()?;
            Ok(Value::Int(match arith {
                ArithInfixOp::Add => lhs + rhs,
                ArithInfixOp::Sub => lhs - rhs,
                ArithInfixOp::Mul => lhs * rhs,
                ArithInfixOp::Div => lhs / rhs,
            }))
        }
        InfixOp::Compare(cmp) => {
            let lhs = lhs.to_int()?;
            let rhs = rhs.to_int()?;
            Ok(Value::Bool(match cmp {
                CompareInfixOp::Gt => lhs > rhs,
                CompareInfixOp::Lt => lhs < rhs,
                CompareInfixOp::Gte => lhs >= rhs,
                CompareInfixOp::Lte => lhs <= rhs,
                CompareInfixOp::Eq => lhs == rhs,
                CompareInfixOp::Neq => lhs != rhs,
            }))
        }
        InfixOp::Logical(logical) => {
            let lhs = lhs.to_bool()?;
            let rhs = rhs.to_bool()?;
            Ok(Value::Bool(match logical {
                LogicalInfixOp::Or => lhs || rhs,
                LogicalInfixOp::And => lhs && rhs,
            }))
        }
    }
}
