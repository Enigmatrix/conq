use std::collections::VecDeque;

use rand::{rngs::ThreadRng, thread_rng, Rng};

use crate::{
    env::Environment,
    err::{ControlFlow, EvalError},
    eval::Evaluator,
    expr::Expr,
    val::Value,
};

pub struct Runtime {
    evals: VecDeque<Evaluator>,
    rng: ThreadRng,
}

impl Runtime {
    pub fn choose_next(&mut self) -> Option<Evaluator> {
        if self.evals.len() == 0 {
            return None;
        }
        self.evals.swap(0, self.rng.gen_range(0..self.evals.len()));
        self.evals.pop_front()
    }

    pub fn eval(&mut self) -> Result<Value, EvalError> {
        let mut last_finish = Value::Void;
        while let Some(mut e) = self.choose_next() {
            // println!("Running {}", e.id());
            let run = e.eval();
            match run {
                Ok(v) => last_finish = v,
                Err(EvalError::ControlFlow(ControlFlow::Yield)) => {
                    self.evals.push_back(e);
                }
                Err(EvalError::ControlFlow(ControlFlow::Launch { expr, env })) => {
                    let new_eval = Evaluator::new(expr, env);
                    self.evals.push_back(new_eval);
                    self.evals.push_back(e);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(last_finish)
    }

    pub fn new(expr: Expr, env: Environment) -> Self {
        let mut vec = VecDeque::new();
        vec.push_back(Evaluator::new(expr, env));
        Runtime {
            evals: vec,
            rng: thread_rng(),
        }
    }
}
