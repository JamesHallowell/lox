use crate::lexer::{Keyword, Literal, Token};

mod visitor;
pub use visitor::Visitor;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Group(GroupExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
}

impl Expr {
    fn binary(left: Expr, operator: Token, right: Expr) -> Expr {
        Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn unary(operator: Token, right: Expr) -> Expr {
        Expr::Unary(UnaryExpr {
            operator,
            right: Box::new(right),
        })
    }

    fn group(expr: Expr) -> Expr {
        Expr::Group(GroupExpr {
            expr: Box::new(expr),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    left: Box<Expr>,
    operator: Token,
    right: Box<Expr>,
}

impl BinaryExpr {
    pub fn left(&self) -> &Expr {
        &self.left
    }

    pub fn operator(&self) -> &Token {
        &self.operator
    }

    pub fn right(&self) -> &Expr {
        &self.right
    }
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    operator: Token,
    right: Box<Expr>,
}

impl UnaryExpr {
    pub fn operator(&self) -> &Token {
        &self.operator
    }

    pub fn right(&self) -> &Expr {
        &self.right
    }
}

#[derive(Debug, PartialEq)]
pub struct GroupExpr {
    expr: Box<Expr>,
}

impl GroupExpr {
    pub fn grouped(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug, PartialEq)]
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

impl From<&str> for Expr {
    fn from(value: &str) -> Self {
        Expr::Literal(LiteralExpr::String(value.to_string()))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected end of token stream")]
    UnexpectedEndOfTokenStream,

    #[error("unexpected token {0:?}")]
    UnexpectedToken(Token),
}

pub fn parse(tokens: &[Token]) -> Result<Expr, Error> {
    let (expr, _) = expression(tokens)?;
    Ok(expr)
}

fn expression(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    equality(tokens)
}

fn equality(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    let (mut lhs, mut tokens) = comparison(tokens)?;

    while let Some(Token::EqualEqual | Token::BangEqual) = peek(tokens) {
        let (operator, rhs_tokens) = next(tokens)?;
        let (rhs, rhs_tokens) = comparison(rhs_tokens)?;

        lhs = Expr::binary(lhs, operator.clone(), rhs);
        tokens = rhs_tokens;
    }

    Ok((lhs, tokens))
}

fn comparison(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    let (mut lhs, mut tokens) = term(tokens)?;

    while let Some(Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual) =
        peek(tokens)
    {
        let (operator, rhs_tokens) = next(tokens)?;
        let (rhs, rhs_tokens) = term(rhs_tokens)?;

        lhs = Expr::binary(lhs, operator.clone(), rhs);
        tokens = rhs_tokens;
    }

    Ok((lhs, tokens))
}

fn term(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    let (mut lhs, mut tokens) = factor(tokens)?;

    while let Some(Token::Plus | Token::Minus) = peek(tokens) {
        let (operator, rhs_tokens) = next(tokens)?;
        let (rhs, leftover_tokens) = factor(rhs_tokens)?;
        lhs = Expr::binary(lhs, operator.clone(), rhs);
        tokens = leftover_tokens;
    }

    Ok((lhs, tokens))
}

fn factor(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    let (mut left, mut tokens) = unary(tokens)?;

    while let Some(Token::Slash | Token::Star) = peek(tokens) {
        let (operator, leftover) = next(tokens)?;
        let (right, leftover) = unary(leftover)?;
        left = Expr::binary(left, operator.clone(), right);
        tokens = leftover;
    }

    Ok((left, tokens))
}

fn unary(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    match next(tokens)? {
        (operator @ Token::Bang | operator @ Token::Minus, tokens) => {
            let (expr, tokens) = unary(tokens)?;
            Ok((Expr::unary(operator.clone(), expr), tokens))
        }
        _ => primary(tokens),
    }
}

fn primary(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    match next(tokens)? {
        (Token::Keyword(Keyword::True), tokens) => Ok((Expr::from(true), tokens)),
        (Token::Keyword(Keyword::False), tokens) => Ok((Expr::from(false), tokens)),
        (Token::Keyword(Keyword::Nil), tokens) => Ok((Expr::Literal(LiteralExpr::Nil), tokens)),
        (Token::LeftParen, tokens) => {
            let (expr, tokens) = expression(tokens)?;
            let (_, tokens) = next_exact(Token::RightParen)(tokens)?;

            Ok((Expr::group(expr), tokens))
        }
        (Token::Literal(Literal::Number(number)), tokens) => Ok((Expr::from(*number), tokens)),
        (Token::Literal(Literal::String(string)), tokens) => {
            Ok((Expr::from(string.as_str()), tokens))
        }
        (token, _) => Err(Error::UnexpectedToken(token.clone())),
    }
}

fn next(tokens: &[Token]) -> Result<(&Token, &[Token]), Error> {
    match tokens.first() {
        Some(token) => Ok((token, &tokens[1..])),
        None => Err(Error::UnexpectedEndOfTokenStream),
    }
}

fn peek(tokens: &[Token]) -> Option<&Token> {
    tokens.first()
}

fn next_exact(expected_token: Token) -> impl Fn(&[Token]) -> Result<(&Token, &[Token]), Error> {
    move |tokens: &[Token]| match tokens.first() {
        Some(token) if token == &expected_token => Ok((token, &tokens[1..])),
        Some(token) => Err(Error::UnexpectedToken(token.clone())),
        None => Err(Error::UnexpectedEndOfTokenStream),
    }
}

#[cfg(test)]
mod test {
    use {super::*, crate::lexer::lex};

    #[test]
    fn parse_string() {
        let tokens = lex("\"Hello, world!\"");
        let ast = parse(&tokens).unwrap();

        assert_eq!(ast, Expr::from("Hello, world!"));
    }

    #[test]
    fn parse_unary_expression() {
        let tokens = lex("!true");
        let ast = parse(&tokens).unwrap();

        assert_eq!(ast, Expr::unary(Token::Bang, Expr::from(true)));
    }

    #[test]
    fn parse_terms_and_factors() {
        let tokens = lex("1 + 2 - 3 * 4 / 5");
        let ast = parse(&tokens).unwrap();

        assert_eq!(
            ast,
            Expr::binary(
                Expr::binary(Expr::from(1), Token::Plus, Expr::from(2)),
                Token::Minus,
                Expr::binary(
                    Expr::binary(Expr::from(3), Token::Star, Expr::from(4)),
                    Token::Slash,
                    Expr::from(5)
                )
            )
        );
    }
}
