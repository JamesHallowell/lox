use {crate::Value, std::collections::HashMap};

pub struct Environment {
    scopes: Vec<Scope>,
}

#[derive(Default)]
pub struct Scope {
    values: HashMap<String, Value>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }
}

impl Environment {
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() == 1 {
            return;
        }

        self.scopes.pop();
    }

    pub fn global_scope(&mut self) -> &mut Scope {
        &mut self.scopes[0]
    }

    pub fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("always at least one scope")
    }

    pub fn scopes(&self) -> impl Iterator<Item = &Scope> {
        self.scopes.iter().rev()
    }

    pub fn scopes_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.scopes.iter_mut().rev()
    }
}

impl Scope {
    pub fn define(&mut self, ident: &str, value: impl Into<Value>) {
        self.values.insert(ident.to_string(), value.into());
    }

    pub fn get(&self, ident: &str) -> Option<&Value> {
        self.values.get(ident)
    }

    pub fn get_mut(&mut self, ident: &str) -> Option<&mut Value> {
        self.values.get_mut(ident)
    }
}
