use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Not, Sub},
};

#[derive(Debug, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("type mismatch: {0}")]
    TypeMismatch(String),
}

impl Value {
    pub fn type_name(&self) -> &str {
        match &self {
            Self::Nil => "nil",
            Self::Number(_) => "number",
            Self::String(_) => "string",
            Self::Boolean(_) => "boolean",
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Nil | Self::Boolean(false))
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Number(number) => write!(f, "{}", number),
            Self::String(string) => write!(f, "{}", string),
            Self::Boolean(boolean) => write!(f, "{}", boolean),
        }
    }
}
