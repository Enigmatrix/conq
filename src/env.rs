use crate::val::{Ident, Value};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

#[derive(Clone)]
pub struct Frame {
    inner: Arc<RwLock<HashMap<Ident, Value>>>,
}

impl Frame {
    pub fn new() -> Frame {
        Frame {
            inner: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn lookup(&self, ident: &Ident) -> Option<Value> {
        let inner = self.inner.read().expect("poisoned lookup");
        inner.get(ident).cloned()
    }

    pub fn assign(&mut self, ident: Ident, val: Value) {
        let mut inner = self.inner.write().expect("poisoned assign");
        inner.insert(ident.clone(), val);
    }
}

#[derive(Clone)]
pub struct Environment {
    frames: Vec<Frame>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            frames: vec![Frame::new()],
        }
    }

    pub fn lookup(&self, ident: &Ident) -> Option<Value> {
        self.frames
            .iter()
            .rev()
            .fold(None, |v, frame| v.or_else(|| frame.lookup(ident)))
    }

    pub fn declare(&mut self, ident: Ident, val: Value) {
        self.frames
            .last_mut()
            .expect("global frame not found")
            .assign(ident, val);
    }

    pub fn assign(&mut self, ident: Ident, val: Value) -> bool {
        let decl_frame = self
            .frames
            .iter_mut()
            .rev()
            .skip_while(|frame| frame.lookup(&ident).is_none())
            .next();
        if let Some(decl_frame) = decl_frame {
            decl_frame.assign(ident, val);
            true
        } else {
            false
        }
    }

    pub fn extend(&mut self) {
        self.frames.push(Frame::new());
    }

    pub fn pop(&mut self) {
        if let None = self.frames.pop() {
            panic!("empty frames");
        }
    }
}
