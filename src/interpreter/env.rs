use {
    crate::{
        interpreter::{
            function::native::{Assert, Clock, Print, Sleep},
            Printer, StdOutPrinter,
        },
        parser::Ident,
        Value,
    },
    std::collections::HashMap,
};

pub struct Environment {
    globals: HashMap<Ident, Value>,
    locals: Vec<HashMap<usize, Value>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
            .with_clock()
            .with_assert()
            .with_sleep()
            .with_print(StdOutPrinter)
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
            self.locals.last_mut().unwrap().insert(ident.id(), value);
        }
    }

    pub fn get(&self, ident: &Ident) -> Option<&Value> {
        for scope in self.locals.iter().rev() {
            if let Some(value) = scope.get(&ident.id()) {
                return Some(value);
            }
        }

        self.globals.get(ident)
    }

    pub fn get_mut(&mut self, ident: &Ident) -> Option<&mut Value> {
        for scope in self.locals.iter_mut().rev() {
            if let Some(value) = scope.get_mut(&ident.id()) {
                return Some(value);
            }
        }

        self.globals.get_mut(ident)
    }

    pub fn with_assert(mut self) -> Self {
        self.globals.insert(Ident::new("assert"), Assert.into());
        self
    }

    pub fn with_clock(mut self) -> Self {
        self.globals.insert(Ident::new("clock"), Clock.into());
        self
    }

    pub fn with_print<P>(mut self, printer: P) -> Self
    where
        P: Printer + 'static,
    {
        self.globals
            .insert(Ident::new("print"), Print::new(printer).into());
        self
    }

    pub fn with_sleep(mut self) -> Self {
        self.globals.insert(Ident::new("sleep"), Sleep.into());
        self
    }

    pub fn push_scope(&mut self) {
        self.locals.push(HashMap::default());
    }

    pub fn pop_scope(&mut self) {
        self.locals.pop();
    }
}
