use crate::lexer::{Keyword, Literal, Token};

mod visitor;
pub use visitor::Visitor;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Block(BlockStmt),
    Expr(ExprStmt),
    If(IfStmt),
    Var(VarStmt),
    While(WhileStmt),
}

impl Stmt {
    fn block(stmts: impl IntoIterator<Item = Stmt>) -> Self {
        Self::Block(BlockStmt {
            stmts: stmts.into_iter().collect(),
        })
    }

    fn expr(expr: Expr) -> Self {
        Self::Expr(ExprStmt { expr })
    }

    fn if_then_else(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Self::If(IfStmt {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        })
    }

    fn var(ident: String, init: Option<Expr>) -> Self {
        Self::Var(VarStmt { ident, init })
    }

    fn while_loop(condition: Expr, body: Stmt) -> Self {
        Self::While(WhileStmt {
            condition,
            body: Box::new(body),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStmt {
    stmts: Vec<Stmt>,
}

impl BlockStmt {
    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts
    }
}

#[derive(Debug, PartialEq)]
pub struct ExprStmt {
    expr: Expr,
}

impl ExprStmt {
    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug, PartialEq)]
pub struct IfStmt {
    condition: Expr,
    then_branch: Box<Stmt>,
    else_branch: Option<Box<Stmt>>,
}

impl IfStmt {
    pub fn condition(&self) -> &Expr {
        &self.condition
    }

    pub fn then_branch(&self) -> &Stmt {
        &self.then_branch
    }

    pub fn else_branch(&self) -> Option<&Stmt> {
        self.else_branch.as_ref().map(|branch| branch.as_ref())
    }
}

#[derive(Debug, PartialEq)]
pub struct VarStmt {
    ident: String,
    init: Option<Expr>,
}

impl VarStmt {
    pub fn ident(&self) -> &str {
        self.ident.as_str()
    }

    pub fn init(&self) -> Option<&Expr> {
        self.init.as_ref()
    }
}

#[derive(Debug, PartialEq)]
pub struct WhileStmt {
    condition: Expr,
    body: Box<Stmt>,
}

impl WhileStmt {
    pub fn condition(&self) -> &Expr {
        &self.condition
    }

    pub fn body(&self) -> &Stmt {
        self.body.as_ref()
    }
}

#[derive(Debug, PartialEq)]
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

impl Expr {
    fn assign(ident: String, value: Expr) -> Self {
        Expr::Assign(AssignExpr {
            ident,
            value: Box::new(value),
        })
    }

    fn binary(left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn callable(callee: Expr, args: impl IntoIterator<Item = Expr>) -> Self {
        Expr::Callable(CallableExpr {
            callee: Box::new(callee),
            args: args.into_iter().collect(),
        })
    }

    fn var(ident: String) -> Self {
        Expr::Var(VarExpr { ident })
    }

    fn unary(operator: Token, right: Expr) -> Self {
        Expr::Unary(UnaryExpr {
            operator,
            right: Box::new(right),
        })
    }

    fn group(expr: Expr) -> Self {
        Expr::Group(GroupExpr {
            expr: Box::new(expr),
        })
    }

    fn and(left: Expr, right: Expr) -> Self {
        Expr::Logical(LogicalExpr {
            left: Box::new(left),
            operator: LogicalOperator::And,
            right: Box::new(right),
        })
    }

    fn or(left: Expr, right: Expr) -> Self {
        Expr::Logical(LogicalExpr {
            left: Box::new(left),
            operator: LogicalOperator::Or,
            right: Box::new(right),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct AssignExpr {
    ident: String,
    value: Box<Expr>,
}

impl AssignExpr {
    pub fn ident(&self) -> &str {
        self.ident.as_str()
    }

    pub fn value(&self) -> &Expr {
        &self.value
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
pub struct CallableExpr {
    callee: Box<Expr>,
    args: Vec<Expr>,
}

impl CallableExpr {
    pub fn callee(&self) -> &Expr {
        self.callee.as_ref()
    }

    pub fn args(&self) -> impl Iterator<Item = &Expr> {
        self.args.iter()
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

#[derive(Debug, PartialEq)]
pub struct LogicalExpr {
    left: Box<Expr>,
    operator: LogicalOperator,
    right: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
}

impl LogicalExpr {
    pub fn left(&self) -> &Expr {
        self.left.as_ref()
    }

    pub fn operator(&self) -> &LogicalOperator {
        &self.operator
    }

    pub fn right(&self) -> &Expr {
        self.right.as_ref()
    }
}

#[derive(Debug, PartialEq)]
pub struct VarExpr {
    ident: String,
}

impl VarExpr {
    pub fn ident(&self) -> &str {
        self.ident.as_str()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected end of token stream")]
    UnexpectedEndOfTokenStream,

    #[error("unexpected token {0:?}")]
    UnexpectedToken(Token),

    #[error("invalid assignment")]
    InvalidAssignment,
}

pub fn parse(mut tokens: &[Token]) -> Result<Vec<Stmt>, Error> {
    let mut statements = vec![];

    while !tokens.is_empty() {
        let (statement, remaining_tokens) = statement(tokens)?;

        statements.push(statement);
        tokens = remaining_tokens;
    }

    Ok(statements)
}

fn statement(tokens: &[Token]) -> Result<(Stmt, &[Token]), Error> {
    match peek(tokens) {
        Some(Token::LeftBrace) => block_statement(tokens),
        Some(Token::Keyword(Keyword::If)) => if_statement(tokens),
        Some(Token::Keyword(Keyword::Var)) => var_statement(tokens),
        Some(Token::Keyword(Keyword::While)) => while_statement(tokens),
        Some(Token::Keyword(Keyword::For)) => for_statement(tokens),
        _ => expression_statement(tokens),
    }
}

fn block_statement(tokens: &[Token]) -> Result<(Stmt, &[Token]), Error> {
    let mut stmts = vec![];

    let (_, mut tokens) = next_exact(Token::LeftBrace)(tokens)?;

    while !matches!(peek(tokens), Some(Token::RightBrace)) {
        let (stmt, remaining_tokens) = statement(tokens)?;

        stmts.push(stmt);
        tokens = remaining_tokens;
    }

    let (_, tokens) = next_exact(Token::RightBrace)(tokens)?;

    Ok((Stmt::block(stmts), tokens))
}

fn if_statement(tokens: &[Token]) -> Result<(Stmt, &[Token]), Error> {
    let (_, tokens) = next_exact(Token::Keyword(Keyword::If))(tokens)?;
    let (_, tokens) = next_exact(Token::LeftParen)(tokens)?;
    let (condition, tokens) = expression(tokens)?;
    let (_, tokens) = next_exact(Token::RightParen)(tokens)?;

    let (then_branch, tokens) = statement(tokens)?;

    let (else_branch, tokens) = if matches!(peek(tokens), Some(Token::Keyword(Keyword::Else))) {
        let (_, tokens) = next_exact(Token::Keyword(Keyword::Else))(tokens)?;
        let (stmt, tokens) = statement(tokens)?;
        (Some(stmt), tokens)
    } else {
        (None, tokens)
    };

    Ok((
        Stmt::if_then_else(condition, then_branch, else_branch),
        tokens,
    ))
}

fn var_statement(tokens: &[Token]) -> Result<(Stmt, &[Token]), Error> {
    let (_, tokens) = next_exact(Token::Keyword(Keyword::Var))(tokens)?;
    let (ident, tokens) = next(tokens)?;

    let ident = if let Token::Literal(Literal::Identifier(ident)) = ident {
        ident.clone()
    } else {
        return Err(Error::UnexpectedToken(ident.clone()));
    };

    let (init, tokens) = match next_if(Token::Equal)(tokens)? {
        Some((_, tokens)) => {
            let (expr, tokens) = expression(tokens)?;
            (Some(expr), tokens)
        }
        None => (None, tokens),
    };

    let (_, tokens) = next_exact(Token::Semicolon)(tokens)?;

    Ok((Stmt::var(ident, init), tokens))
}

fn while_statement(tokens: &[Token]) -> Result<(Stmt, &[Token]), Error> {
    let (_, tokens) = next_exact(Token::Keyword(Keyword::While))(tokens)?;

    let (_, tokens) = next_exact(Token::LeftParen)(tokens)?;
    let (condition, tokens) = expression(tokens)?;
    let (_, tokens) = next_exact(Token::RightParen)(tokens)?;

    let (body, tokens) = statement(tokens)?;

    Ok((Stmt::while_loop(condition, body), tokens))
}

fn for_statement(tokens: &[Token]) -> Result<(Stmt, &[Token]), Error> {
    let (_, tokens) = next_exact(Token::Keyword(Keyword::For))(tokens)?;
    let (_, tokens) = next_exact(Token::LeftParen)(tokens)?;
    let (init, tokens) = for_statement_init(tokens)?;
    let (condition, tokens) = for_statement_condition(tokens)?;
    let (expr, tokens) = for_statement_expression(tokens)?;
    let (_, tokens) = next_exact(Token::RightParen)(tokens)?;
    let (body, tokens) = statement(tokens)?;

    let condition = condition.unwrap_or(Expr::from(true));
    let body = Stmt::block([Some(body), expr.map(Stmt::expr)].into_iter().flatten());
    let while_loop = Stmt::while_loop(condition, body);
    let for_loop = Stmt::block([init, Some(while_loop)].into_iter().flatten());

    Ok((for_loop, tokens))
}

fn for_statement_init(tokens: &[Token]) -> Result<(Option<Stmt>, &[Token]), Error> {
    Ok(match peek(tokens) {
        Some(Token::Semicolon) => {
            let (_, tokens) = next(tokens)?;
            (None, tokens)
        }
        Some(Token::Keyword(Keyword::Var)) => {
            let (stmt, tokens) = var_statement(tokens)?;
            (Some(stmt), tokens)
        }
        _ => {
            let (stmt, tokens) = expression_statement(tokens)?;
            (Some(stmt), tokens)
        }
    })
}

fn for_statement_condition(tokens: &[Token]) -> Result<(Option<Expr>, &[Token]), Error> {
    Ok(match peek(tokens) {
        Some(Token::Semicolon) => {
            let (_, tokens) = next(tokens)?;
            (None, tokens)
        }
        _ => {
            let (expr, tokens) = expression(tokens)?;
            let (_, tokens) = next_exact(Token::Semicolon)(tokens)?;
            (Some(expr), tokens)
        }
    })
}

fn for_statement_expression(tokens: &[Token]) -> Result<(Option<Expr>, &[Token]), Error> {
    Ok(match peek(tokens) {
        Some(Token::RightParen) => (None, tokens),
        _ => {
            let (expr, tokens) = expression(tokens)?;
            (Some(expr), tokens)
        }
    })
}

fn expression_statement(tokens: &[Token]) -> Result<(Stmt, &[Token]), Error> {
    let (expr, tokens) = expression(tokens)?;
    let (_, tokens) = next_exact(Token::Semicolon)(tokens)?;
    Ok((Stmt::expr(expr), tokens))
}

fn expression(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    assignment(tokens)
}

fn assignment(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    let (expr, tokens) = or(tokens)?;

    match peek(tokens) {
        Some(Token::Equal) => {
            let (_equal, tokens) = next(tokens)?;
            let (value, tokens) = assignment(tokens)?;

            match expr {
                Expr::Var(VarExpr { ident }) => Ok((Expr::assign(ident, value), tokens)),
                _ => Err(Error::InvalidAssignment),
            }
        }
        _ => Ok((expr, tokens)),
    }
}

fn or(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    let (mut left, mut tokens) = and(tokens)?;

    while let Some(Token::Keyword(Keyword::Or)) = peek(tokens) {
        let (_, leftover) = next(tokens)?;
        let (right, leftover) = and(leftover)?;

        left = Expr::or(left, right);
        tokens = leftover;
    }

    Ok((left, tokens))
}

fn and(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    let (mut left, mut tokens) = equality(tokens)?;

    while let Some(Token::Keyword(Keyword::And)) = peek(tokens) {
        let (_, leftover) = next(tokens)?;
        let (right, leftover) = equality(leftover)?;

        left = Expr::and(left, right);
        tokens = leftover;
    }

    Ok((left, tokens))
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
    match peek(tokens) {
        Some(Token::Bang | Token::Minus) => {
            let (operator, tokens) = next(tokens)?;
            let (expr, tokens) = unary(tokens)?;
            Ok((Expr::unary(operator.clone(), expr), tokens))
        }
        _ => call(tokens),
    }
}

fn call(tokens: &[Token]) -> Result<(Expr, &[Token]), Error> {
    let (mut expr, mut tokens) = primary(tokens)?;

    while let Some(Token::LeftParen) = peek(tokens) {
        let (args, leftover) = call_args(tokens)?;
        expr = Expr::callable(expr, args);
        tokens = leftover;
    }

    Ok((expr, tokens))
}

fn call_args(tokens: &[Token]) -> Result<(Vec<Expr>, &[Token]), Error> {
    let (_, mut tokens) = next_exact(Token::LeftParen)(tokens)?;

    if let Some((Token::RightParen, tokens)) = next_if(Token::RightParen)(tokens)? {
        return Ok((vec![], tokens));
    }

    let mut args = vec![];
    loop {
        let (expr, leftover) = expression(tokens)?;
        args.push(expr);

        match next_if(Token::Comma)(leftover)? {
            Some((_, leftover)) => {
                tokens = leftover;
            }
            None => {
                tokens = leftover;
                break;
            }
        }
    }

    let (_, tokens) = next_exact(Token::RightParen)(tokens)?;

    Ok((args, tokens))
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
        (Token::Literal(Literal::Identifier(ident)), tokens) => {
            Ok((Expr::var(ident.clone()), tokens))
        }
        (token, _) => Err(Error::UnexpectedToken(token.clone())),
    }
}

fn peek(tokens: &[Token]) -> Option<&Token> {
    tokens.first()
}

fn next(tokens: &[Token]) -> Result<(&Token, &[Token]), Error> {
    match tokens.first() {
        Some(token) => Ok((token, &tokens[1..])),
        None => Err(Error::UnexpectedEndOfTokenStream),
    }
}

fn next_if(
    expected_token: Token,
) -> impl Fn(&[Token]) -> Result<Option<(&Token, &[Token])>, Error> {
    move |tokens: &[Token]| match tokens.first() {
        Some(token) if token == &expected_token => Ok(Some((token, &tokens[1..]))),
        Some(_) => Ok(None),
        None => Err(Error::UnexpectedEndOfTokenStream),
    }
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
        let tokens = lex("\"Hello, world!\";");
        let statements = parse(&tokens).unwrap();

        assert_eq!(statements, vec![Stmt::expr(Expr::from("Hello, world!"))]);
    }

    #[test]
    fn parse_unary_expression() {
        let tokens = lex("!true;");
        let statements: Vec<Stmt> = parse(&tokens).unwrap();

        assert_eq!(
            statements,
            vec![Stmt::expr(Expr::unary(Token::Bang, Expr::from(true)))]
        );
    }

    #[test]
    fn parse_terms_and_factors() {
        let tokens = lex("1 + 2 - 3 * 4 / 5;");
        let statements = parse(&tokens).unwrap();

        assert_eq!(
            statements,
            vec![Stmt::expr(Expr::binary(
                Expr::binary(Expr::from(1), Token::Plus, Expr::from(2)),
                Token::Minus,
                Expr::binary(
                    Expr::binary(Expr::from(3), Token::Star, Expr::from(4)),
                    Token::Slash,
                    Expr::from(5)
                )
            ))]
        );
    }

    #[test]
    fn parse_callable() {
        let program = r#"
        foo();
        foo(5, true);
        foo(5, true, "hello", nil);
        "#;
        let tokens = lex(program);
        let statements = parse(&tokens).unwrap();

        assert_eq!(
            statements,
            vec![
                Stmt::expr(Expr::callable(Expr::var("foo".to_string()), vec![])),
                Stmt::expr(Expr::callable(
                    Expr::var("foo".to_string()),
                    [Expr::from(5), Expr::from(true)]
                )),
                Stmt::expr(Expr::callable(
                    Expr::var("foo".to_string()),
                    [
                        Expr::from(5),
                        Expr::from(true),
                        Expr::from("hello"),
                        Expr::Literal(LiteralExpr::Nil),
                    ]
                ))
            ]
        );
    }
}
