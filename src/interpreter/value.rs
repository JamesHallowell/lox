use {
    crate::{interpreter::Error, Interpreter},
    std::{
        cmp::{Ordering, PartialEq, PartialOrd},
        fmt,
        ops::{Add, Div, Mul, Neg, Not, Rem, Sub},
        rc::Rc,
    },
};

#[derive(Clone)]
pub enum Value {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Rc<dyn Callable>),
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match &self {
            Self::Nil => "nil",
            Self::Number(_) => "number",
            Self::String(_) => "string",
            Self::Boolean(_) => "boolean",
            Self::Function(_) => "function",
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Nil | Self::Boolean(false))
    }

    pub fn function(func: impl Callable + 'static) -> Self {
        Self::Function(Rc::new(func))
    }
}

impl Add<Self> for Value {
    type Output = Result<Self, Error>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(lhs), Self::Number(rhs)) => Ok(Self::Number(lhs + rhs)),
            (Self::String(mut lhs), Self::String(rhs)) => Ok(Self::String({
                lhs.push_str(&rhs);
                lhs
            })),
            (lhs, rhs) => Err(Error::TypeMismatch(format!(
                "cannot add {} and {}",
                lhs.type_name(),
                rhs.type_name()
            ))),
        }
    }
}

impl Sub<Self> for Value {
    type Output = Result<Self, Error>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(lhs), Self::Number(rhs)) => Ok(Self::Number(lhs - rhs)),
            (lhs, rhs) => Err(Error::TypeMismatch(format!(
                "cannot subtract {} from {}",
                rhs.type_name(),
                lhs.type_name()
            ))),
        }
    }
}

impl Mul<Self> for Value {
    type Output = Result<Self, Error>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(lhs), Self::Number(rhs)) => Ok(Self::Number(lhs * rhs)),
            (lhs, rhs) => Err(Error::TypeMismatch(format!(
                "cannot multiply {} and {}",
                lhs.type_name(),
                rhs.type_name()
            ))),
        }
    }
}

impl Div<Self> for Value {
    type Output = Result<Self, Error>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(lhs), Self::Number(rhs)) => Ok(Self::Number(lhs / rhs)),
            (lhs, rhs) => Err(Error::TypeMismatch(format!(
                "cannot perform division on {} and {}",
                lhs.type_name(),
                rhs.type_name()
            ))),
        }
    }
}

impl Rem<Self> for Value {
    type Output = Result<Self, Error>;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(lhs), Self::Number(rhs)) => Ok(Self::Number(lhs % rhs)),
            (lhs, rhs) => Err(Error::TypeMismatch(format!(
                "cannot perform modulo on {} and {}",
                lhs.type_name(),
                rhs.type_name()
            ))),
        }
    }
}

impl Neg for Value {
    type Output = Result<Self, Error>;

    fn neg(self) -> Self::Output {
        if let Self::Number(number) = self {
            Ok(Self::Number(-number))
        } else {
            Err(Error::TypeMismatch(format!(
                "cannot negate {}",
                self.type_name()
            )))
        }
    }
}

impl Not for Value {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::Boolean(!self.is_truthy())
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Number(lhs), Self::Number(rhs)) => lhs == rhs,
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            (Self::Boolean(lhs), Self::Boolean(rhs)) => lhs == rhs,
            (Self::Function(_), Self::Function(_)) => false,
            _ => false,
        }
    }
}

impl PartialOrd<Value> for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match (self, other) {
            (Self::Number(lhs), Self::Number(rhs)) => lhs.partial_cmp(rhs),
            (Self::String(lhs), Self::String(rhs)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Number(number) => write!(f, "{}", number),
            Self::String(string) => write!(f, "{}", string),
            Self::Boolean(boolean) => write!(f, "{}", boolean),
            Self::Function(_) => write!(f, "<function>"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Number(value) => f.debug_tuple("Number").field(value).finish(),
            Self::String(value) => f.debug_tuple("String").field(value).finish(),
            Self::Boolean(value) => f.debug_tuple("Boolean").field(value).finish(),
            Self::Function(_) => f.debug_tuple("Function").finish(),
        }
    }
}

pub enum Arity {
    N(usize),
    Variadic,
}

pub trait Callable {
    fn arity(&self) -> Arity;
    fn call(&self, args: &[Value], interpreter: &mut Interpreter) -> Result<Value, Error>;
}

impl Value {
    pub fn call(
        &mut self,
        args: Vec<Value>,
        interpreter: &mut Interpreter,
    ) -> Result<Value, Error> {
        match self {
            Self::Function(function) => {
                if let Arity::N(arity) = function.arity() {
                    if arity != args.len() {
                        return Err(Error::ArityMismatch {
                            expected: arity,
                            actual: args.len(),
                        });
                    }
                }

                function.call(&args, interpreter)
            }
            _ => Err(Error::NotCallable),
        }
    }
}
