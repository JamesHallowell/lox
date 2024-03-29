use {
    crate::{
        interpreter::{
            value::{Arity, Callable, Value},
            Error, Printer,
        },
        Interpreter,
    },
    std::{
        thread::sleep,
        time::{Duration, SystemTime, UNIX_EPOCH},
    },
};

pub struct Clock;

impl Callable for Clock {
    fn arity(&self) -> Arity {
        Arity::N(0)
    }

    fn call(&self, _args: &[Value], _: &mut Interpreter) -> Result<Value, Error> {
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
    fn arity(&self) -> Arity {
        Arity::N(1)
    }

    fn call(&self, args: &[Value], _: &mut Interpreter) -> Result<Value, Error> {
        let duration_to_sleep = if let Some(Value::Number(milliseconds)) = args.first() {
            Duration::from_millis(milliseconds.round() as u64)
        } else {
            return Err(Error::WrongArgType);
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
    fn arity(&self) -> Arity {
        Arity::Variadic
    }

    fn call(&self, args: &[Value], _: &mut Interpreter) -> Result<Value, Error> {
        for arg in args {
            self.0.print(arg);
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
    fn arity(&self) -> Arity {
        Arity::Variadic
    }

    fn call(&self, args: &[Value], _: &mut Interpreter) -> Result<Value, Error> {
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
