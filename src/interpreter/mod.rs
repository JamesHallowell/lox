use crate::{
    lexer::{lex, Token},
    parser::{self, parse, BinaryExpr, Expr, GroupExpr, LiteralExpr, UnaryExpr, Visitor},
};

mod value;
pub use value::Value;

#[derive(Default)]
pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&mut self, expression: &str) -> Result<Value, Error> {
        let tokens = lex(expression);
        let ast = parse(&tokens)?;
        self.visit_expr(&ast)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Parser(#[from] parser::Error),

    #[error(transparent)]
    Value(#[from] value::Error),

    #[error("unexpected unary operator {0:?}")]
    UnexpectedUnaryOperator(Token),

    #[error("unexpected binary operator {0:?}")]
    UnexpectedBinaryOperator(Token),
}

impl Visitor for Interpreter {
    type Output = Value;
    type Error = Error;

    fn visit_expr(&mut self, expr: &Expr) -> Result<Self::Output, Self::Error> {
        match expr {
            Expr::Binary(expr) => self.visit_binary_expr(expr),
            Expr::Unary(expr) => self.visit_unary_expr(expr),
            Expr::Group(expr) => self.visit_group_expr(expr),
            Expr::Literal(expr) => self.visit_literal_expr(expr),
        }
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<Self::Output, Self::Error> {
        let lhs = self.visit_expr(expr.left())?;
        let rhs = self.visit_expr(expr.right())?;

        match expr.operator() {
            Token::Plus => Ok((lhs + rhs)?),
            Token::Minus => Ok((lhs - rhs)?),
            Token::Star => Ok((lhs * rhs)?),
            Token::Slash => Ok((lhs / rhs)?),
            Token::EqualEqual => Ok(Value::Boolean(lhs == rhs)),
            Token::BangEqual => Ok(Value::Boolean(lhs != rhs)),
            token => Err(Error::UnexpectedBinaryOperator(token.clone())),
        }
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<Self::Output, Self::Error> {
        let rhs = self.visit_expr(expr.right())?;

        match expr.operator() {
            Token::Minus => Ok((-rhs)?),
            Token::Bang => Ok(!rhs),
            token => Err(Error::UnexpectedUnaryOperator(token.clone())),
        }
    }

    fn visit_group_expr(&mut self, expr: &GroupExpr) -> Result<Self::Output, Self::Error> {
        self.visit_expr(expr.grouped())
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Result<Self::Output, Self::Error> {
        match expr {
            LiteralExpr::Boolean(value) => Ok(Value::Boolean(*value)),
            LiteralExpr::Nil => Ok(Value::Nil),
            LiteralExpr::Number(value) => Ok(Value::Number(*value)),
            LiteralExpr::String(value) => Ok(Value::String(value.clone())),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn evaluate_simple_expression() {
        let result = Interpreter::default().interpret("2 + 2").unwrap();

        assert_eq!(result, Value::Number(4.0));
    }
}
