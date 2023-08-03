use {
    crate::{
        interpreter::{
            function::native::{Assert, Clock, Print, Sleep},
            StdOutPrinter,
        },
        parser::{Ident, IdentByAddress},
        Value,
    },
    std::collections::HashMap,
};

pub struct Environment {
    globals: HashMap<Ident, Value>,
    locals: Vec<HashMap<IdentByAddress, Value>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
            .with_global("assert", Assert)
            .with_global("print", Print::new(StdOutPrinter))
            .with_global("clock", Clock)
            .with_global("sleep", Sleep)
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            globals: HashMap::default(),
            locals: Vec::default(),
        }
    }

    pub fn define(&mut self, ident: &Ident, value: impl Into<Value>) {
        let value = value.into();
        if self.locals.is_empty() {
            self.globals.insert(ident.clone(), value);
        } else {
            self.locals.last_mut().unwrap().insert(ident.into(), value);
        }
    }

    pub fn with_global(mut self, ident: impl Into<Ident>, value: impl Into<Value>) -> Self {
        self.globals.insert(ident.into(), value.into());
        self
    }

    pub fn get(&self, ident: &Ident) -> Option<&Value> {
        for scope in self.locals.iter().rev() {
            if let Some(value) = scope.get(&ident.into()) {
                return Some(value);
            }
        }

        self.globals.get(ident)
    }

    pub fn get_mut(&mut self, ident: &Ident) -> Option<&mut Value> {
        for scope in self.locals.iter_mut().rev() {
            if let Some(value) = scope.get_mut(&ident.into()) {
                return Some(value);
            }
        }

        self.globals.get_mut(ident)
    }

    pub fn push_scope(&mut self) {
        self.locals.push(HashMap::default());
    }

    pub fn pop_scope(&mut self) {
        self.locals.pop();
    }
}
