use {
    crate::interpreter::{
        value::{CallError, Callable, Value},
        Printer,
    },
    std::{
        thread::sleep,
        time::{Duration, SystemTime, UNIX_EPOCH},
    },
};

pub struct Clock;

impl Callable for Clock {
    fn arity(&self) -> Option<usize> {
        Some(0)
    }

    fn call(&mut self, _args: &[Value]) -> Result<Value, CallError> {
        Ok(Value::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        ))
    }
}

impl From<Clock> for Value {
    fn from(clock: Clock) -> Value {
        Value::function(clock)
    }
}

pub struct Sleep;

impl Callable for Sleep {
    fn arity(&self) -> Option<usize> {
        Some(1)
    }

    fn call(&mut self, args: &[Value]) -> Result<Value, CallError> {
        let duration_to_sleep = if let Some(Value::Number(milliseconds)) = args.first() {
            Duration::from_millis(milliseconds.round() as u64)
        } else {
            return Err(CallError::WrongArgType);
        };

        sleep(duration_to_sleep);

        Ok(Value::Nil)
    }
}

impl From<Sleep> for Value {
    fn from(sleep: Sleep) -> Self {
        Value::function(sleep)
    }
}

pub struct Print<P>(P);

impl<P> Print<P> {
    pub fn new(printer: P) -> Self {
        Self(printer)
    }
}

impl<P> Callable for Print<P>
where
    P: Printer,
{
    fn arity(&self) -> Option<usize> {
        None
    }

    fn call(&mut self, args: &[Value]) -> Result<Value, CallError> {
        for arg in args {
            self.0.print(&arg);
        }
        Ok(Value::Nil)
    }
}

impl<P> From<Print<P>> for Value
where
    P: Printer + 'static,
{
    fn from(print: Print<P>) -> Self {
        Value::function(print)
    }
}

pub struct Assert;

impl Callable for Assert {
    fn arity(&self) -> Option<usize> {
        None
    }

    fn call(&mut self, args: &[Value]) -> Result<Value, CallError> {
        for arg in args {
            assert!(arg.is_truthy());
        }
        Ok(Value::Nil)
    }
}

impl From<Assert> for Value {
    fn from(assert: Assert) -> Self {
        Value::function(assert)
    }
}
