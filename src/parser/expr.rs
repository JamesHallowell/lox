use crate::{
    lexer::{Keyword, Literal, Token},
    parser::{Error, Ident, TokenStream},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign(AssignExpr),
    Binary(BinaryExpr),
    Callable(CallableExpr),
    Group(GroupExpr),
    Literal(LiteralExpr),
    Logical(LogicalExpr),
    Unary(UnaryExpr),
    Var(VarExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub ident: Ident,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub right: Box<Expr>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub right: Box<Expr>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaryOperator {
    Not,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallableExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupExpr {
    pub grouped: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl From<bool> for Expr {
    fn from(value: bool) -> Self {
        Expr::Literal(LiteralExpr::Boolean(value))
    }
}

impl From<i32> for Expr {
    fn from(value: i32) -> Self {
        Expr::Literal(LiteralExpr::Number(value as f64))
    }
}

impl From<f64> for Expr {
    fn from(value: f64) -> Self {
        Expr::Literal(LiteralExpr::Number(value))
    }
}

impl<'a> From<&'a str> for Expr {
    fn from(value: &'a str) -> Self {
        Expr::Literal(LiteralExpr::String(value.to_string()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: LogicalOperator,
    pub right: Box<Expr>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarExpr {
    pub ident: Ident,
}

pub fn expression(tokens: TokenStream<'_>) -> Result<(Expr, TokenStream<'_>), Error> {
    assignment(tokens)
}

pub fn assignment(tokens: TokenStream<'_>) -> Result<(Expr, TokenStream<'_>), Error> {
    let (expr, tokens) = or(tokens)?;

    match tokens.peek() {
        Some(Token::Equal) => {
            let (_equal, tokens) = tokens.next()?;
            let (value, tokens) = assignment(tokens)?;

            match expr {
                Expr::Var(VarExpr { ident }) => Ok((
                    Expr::Assign(AssignExpr {
                        ident,
                        value: Box::new(value),
                    }),
                    tokens,
                )),
                _ => Err(Error::InvalidAssignment),
            }
        }
        _ => Ok((expr, tokens)),
    }
}

pub fn or(tokens: TokenStream<'_>) -> Result<(Expr, TokenStream<'_>), Error> {
    let (mut left, mut tokens) = and(tokens)?;

    while let Some(Token::Keyword(Keyword::Or)) = tokens.peek() {
        let (_, leftover) = tokens.next()?;
        let (right, leftover) = and(leftover)?;

        left = Expr::Logical(LogicalExpr {
            left: Box::new(left),
            operator: LogicalOperator::Or,
            right: Box::new(right),
        });
        tokens = leftover;
    }

    Ok((left, tokens))
}

pub fn and(tokens: TokenStream) -> Result<(Expr, TokenStream), Error> {
    let (mut left, mut tokens) = equality(tokens)?;

    while let Some(Token::Keyword(Keyword::And)) = tokens.peek() {
        let (_, leftover) = tokens.next()?;
        let (right, leftover) = equality(leftover)?;

        left = Expr::Logical(LogicalExpr {
            left: Box::new(left),
            operator: LogicalOperator::And,
            right: Box::new(right),
        });
        tokens = leftover;
    }

    Ok((left, tokens))
}

pub fn equality(tokens: TokenStream<'_>) -> Result<(Expr, TokenStream<'_>), Error> {
    let (mut lhs, mut tokens) = comparison(tokens)?;

    while let Some(Token::EqualEqual | Token::BangEqual) = tokens.peek() {
        let (operator, rhs_tokens) = tokens.next()?;
        let operator = match operator {
            Token::EqualEqual => BinaryOperator::Equal,
            Token::BangEqual => BinaryOperator::NotEqual,
            _ => panic!(),
        };

        let (rhs, rhs_tokens) = comparison(rhs_tokens)?;

        lhs = Expr::Binary(BinaryExpr {
            left: Box::new(lhs),
            operator,
            right: Box::new(rhs),
        });
        tokens = rhs_tokens;
    }

    Ok((lhs, tokens))
}

pub fn comparison(tokens: TokenStream<'_>) -> Result<(Expr, TokenStream<'_>), Error> {
    let (mut lhs, mut tokens) = term(tokens)?;

    while let Some(Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual) =
        tokens.peek()
    {
        let (operator, rhs_tokens) = tokens.next()?;
        let operator = match operator {
            Token::Greater => BinaryOperator::GreaterThan,
            Token::GreaterEqual => BinaryOperator::GreaterThanOrEqual,
            Token::Less => BinaryOperator::LessThan,
            Token::LessEqual => BinaryOperator::LessThanOrEqual,
            _ => return Err(Error::UnexpectedToken(format!("{:?}", operator))),
        };

        let (rhs, rhs_tokens) = term(rhs_tokens)?;

        lhs = Expr::Binary(BinaryExpr {
            left: Box::new(lhs),
            operator,
            right: Box::new(rhs),
        });
        tokens = rhs_tokens;
    }

    Ok((lhs, tokens))
}

pub fn term(tokens: TokenStream<'_>) -> Result<(Expr, TokenStream<'_>), Error> {
    let (mut lhs, mut tokens) = factor(tokens)?;

    while let Some(Token::Plus | Token::Minus) = tokens.peek() {
        let (operator, rhs_tokens) = tokens.next()?;
        let operator = match operator {
            Token::Plus => BinaryOperator::Plus,
            Token::Minus => BinaryOperator::Minus,
            _ => return Err(Error::UnexpectedToken(format!("{:?}", operator))),
        };

        let (rhs, leftover_tokens) = factor(rhs_tokens)?;
        lhs = Expr::Binary(BinaryExpr {
            left: Box::new(lhs),
            operator,
            right: Box::new(rhs),
        });
        tokens = leftover_tokens;
    }

    Ok((lhs, tokens))
}

pub fn factor(tokens: TokenStream<'_>) -> Result<(Expr, TokenStream<'_>), Error> {
    let (mut left, mut tokens) = unary(tokens)?;

    while let Some(Token::Slash | Token::Star | Token::Percent) = tokens.peek() {
        let (operator, leftover) = tokens.next()?;
        let operator = match operator {
            Token::Slash => BinaryOperator::Divide,
            Token::Star => BinaryOperator::Multiply,
            Token::Percent => BinaryOperator::Modulo,
            _ => return Err(Error::UnexpectedToken(format!("{:?}", operator))),
        };

        let (right, leftover) = unary(leftover)?;
        left = Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        });
        tokens = leftover;
    }

    Ok((left, tokens))
}

pub fn unary(tokens: TokenStream<'_>) -> Result<(Expr, TokenStream<'_>), Error> {
    match tokens.peek() {
        Some(Token::Bang | Token::Minus) => {
            let (operator, tokens) = tokens.next()?;

            let operator = match operator {
                Token::Bang => UnaryOperator::Not,
                Token::Minus => UnaryOperator::Minus,
                _ => return Err(Error::UnexpectedToken(format!("{:?}", operator))),
            };

            let (expr, tokens) = unary(tokens)?;
            Ok((
                Expr::Unary(UnaryExpr {
                    operator,
                    right: Box::new(expr),
                }),
                tokens,
            ))
        }
        _ => call(tokens),
    }
}

fn call(tokens: TokenStream<'_>) -> Result<(Expr, TokenStream<'_>), Error> {
    let (mut expr, mut tokens) = primary(tokens)?;

    while let Some(Token::LeftParen) = tokens.peek() {
        let (args, leftover) = call_args(tokens)?;
        expr = Expr::Callable(CallableExpr {
            callee: Box::new(expr),
            args,
        });
        tokens = leftover;
    }

    Ok((expr, tokens))
}

pub fn call_args(tokens: TokenStream<'_>) -> Result<(Vec<Expr>, TokenStream<'_>), Error> {
    let mut tokens = tokens.expect(Token::LeftParen)?;

    if let Some((Token::RightParen, tokens)) = tokens.next_if(Token::RightParen)? {
        return Ok((vec![], tokens));
    }

    let mut args = vec![];
    loop {
        let (expr, leftover) = expression(tokens)?;
        args.push(expr);

        match leftover.next_if(Token::Comma)? {
            Some((_, leftover)) => {
                tokens = leftover;
            }
            None => {
                tokens = leftover;
                break;
            }
        }
    }

    let tokens = tokens.expect(Token::RightParen)?;

    Ok((args, tokens))
}

pub fn primary(tokens: TokenStream<'_>) -> Result<(Expr, TokenStream<'_>), Error> {
    match tokens.next()? {
        (Token::Keyword(Keyword::True), tokens) => Ok((Expr::from(true), tokens)),
        (Token::Keyword(Keyword::False), tokens) => Ok((Expr::from(false), tokens)),
        (Token::Keyword(Keyword::Nil), tokens) => Ok((Expr::Literal(LiteralExpr::Nil), tokens)),
        (Token::LeftParen, tokens) => {
            let (expr, tokens) = expression(tokens)?;
            let tokens = tokens.expect(Token::RightParen)?;

            Ok((
                Expr::Group(GroupExpr {
                    grouped: Box::new(expr),
                }),
                tokens,
            ))
        }
        (Token::Literal(Literal::Number(number)), tokens) => Ok((Expr::from(number), tokens)),
        (Token::Literal(Literal::String(string)), tokens) => Ok((Expr::from(string), tokens)),
        (Token::Literal(Literal::Identifier(ident)), tokens) => Ok((
            Expr::Var(VarExpr {
                ident: Ident::new(ident),
            }),
            tokens,
        )),
        (token, _) => Err(Error::UnexpectedToken(format!("{:?}", token))),
    }
}
