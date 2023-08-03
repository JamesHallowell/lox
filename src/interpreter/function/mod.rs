use {
    crate::{
        interpreter::{
            value::{Arity, Callable},
            Error,
        },
        parser::{FnStmt, StmtVisitor},
        Interpreter, Value,
    },
    std::{cell::RefCell, rc::Rc},
};

pub mod native;

pub struct Function {
    stmt: FnStmt,
}

impl Function {
    pub fn new(stmt: FnStmt) -> Self {
        Self { stmt }
    }
}

impl Callable for Function {
    fn arity(&self) -> Arity {
        Arity::N(self.stmt.params.len())
    }

    fn call(&mut self, args: &[Value], interpreter: &mut Interpreter) -> Result<Value, Error> {
        interpreter.environment.push_scope();
        for (param, arg) in self.stmt.params.iter().zip(args) {
            interpreter.environment.define(param, arg.clone());
        }

        for stmt in &self.stmt.body {
            if let Some(value) = interpreter.visit_stmt(stmt)? {
                interpreter.environment.pop_scope();
                return Ok(value);
            }
        }

        interpreter.environment.pop_scope();
        Ok(Value::Nil)
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Self::Function(Rc::new(RefCell::new(value)))
    }
}
