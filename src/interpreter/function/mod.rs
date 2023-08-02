use crate::interpreter::Value;

pub mod native;

pub trait Function {
    fn arity(&self) -> Option<usize>;
    fn call(&mut self, args: &[Value]) -> Result<Value, CallError>;
}

#[derive(Debug, thiserror::Error)]
pub enum CallError {
    #[error("can only call functions and classes")]
    NotCallable,

    #[error("arity mismatch, expected {expected} args, got {actual} args")]
    ArityMismatch { expected: usize, actual: usize },

    #[error("wrong argument type")]
    WrongArgType,
}
