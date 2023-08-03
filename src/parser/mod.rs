use {
    crate::lexer::{Keyword, Literal, Token},
    std::{
        fmt::{Debug, Display, Formatter},
        hash::Hash,
        rc::Rc,
    },
};

mod visitor;
pub use visitor::{ExprVisitor, StmtVisitor};

mod expr;
pub use expr::*;

mod resolver;
pub use resolver::{resolve, Error as ResolveError};

#[derive(Debug, Clone, Copy)]
pub struct TokenStream<'input> {
    tokens: &'input [Token<'input>],
}

impl<'input> Iterator for TokenStream<'input> {
    type Item = Token<'input>;

    fn next(&mut self) -> Option<Token<'input>> {
        match self.tokens.first().copied() {
            Some(token) => {
                self.advance();
                Some(token)
            }
            None => None,
        }
    }
}

impl<'input> TokenStream<'input> {
    fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    fn peek(&self) -> Option<Token<'input>> {
        self.tokens.first().copied()
    }

    fn next(self) -> Result<(Token<'input>, Self), Error> {
        match self.peek() {
            Some(token) => Ok((token, self.advance())),
            None => Err(Error::UnexpectedEndOfTokenStream),
        }
    }

    fn advance(self) -> Self {
        Self {
            tokens: &self.tokens[1..],
        }
    }

    fn next_if(
        self,
        expected_token: impl Into<Token<'input>>,
    ) -> Result<Option<(Token<'input>, TokenStream<'input>)>, Error> {
        let expected_token = expected_token.into();
        match self.peek() {
            Some(token) if token == expected_token => self.next().map(Some),
            Some(_) => Ok(None),
            None => Err(Error::UnexpectedEndOfTokenStream),
        }
    }

    fn expect_ident(self) -> Result<(Ident, TokenStream<'input>), Error> {
        match self.next()? {
            (Token::Literal(Literal::Identifier(ident)), token_stream) => {
                Ok((Ident::new(ident), token_stream))
            }
            (token, _) => Err(Error::UnexpectedToken(format!("{:?}", token))),
        }
    }

    fn expect(
        self,
        expected_token: impl Into<Token<'input>>,
    ) -> Result<TokenStream<'input>, Error> {
        let expected_token = expected_token.into();
        match self.peek() {
            Some(token) if token == expected_token => self.next().map(|(_, tokens)| tokens),
            Some(token) => Err(Error::UnexpectedToken(format!("{:?}", token))),
            None => Err(Error::UnexpectedEndOfTokenStream),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Ident {
    name: Rc<str>,
}

impl From<&'_ str> for Ident {
    fn from(name: &str) -> Self {
        Self { name: name.into() }
    }
}

impl Ident {
    pub fn new(name: impl AsRef<str>) -> Self {
        let name = name.as_ref().into();

        Self { name }
    }

    pub fn id(&self) -> usize {
        self.name.as_ptr() as usize
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ident")
            .field("name", &self.name)
            .field("id", &self.id())
            .finish()
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Block(BlockStmt),
    Expr(ExprStmt),
    Fn(FnStmt),
    If(IfStmt),
    Return(ReturnStmt),
    Var(VarStmt),
    While(WhileStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnStmt {
    pub ident: Ident,
    pub params: Vec<Ident>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarStmt {
    pub ident: Ident,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected end of token stream")]
    UnexpectedEndOfTokenStream,

    #[error("unexpected token {0}")]
    UnexpectedToken(String),

    #[error("invalid assignment")]
    InvalidAssignment,
}

pub fn parse<'a>(tokens: &'a [Token<'a>]) -> Result<Vec<Stmt>, Error> {
    let mut statements = vec![];

    let mut tokens = TokenStream { tokens };
    while !tokens.is_empty() {
        let (statement, remaining_tokens) = statement(tokens)?;

        statements.push(statement);
        tokens = remaining_tokens;
    }

    Ok(statements)
}

fn statement(tokens: TokenStream<'_>) -> Result<(Stmt, TokenStream<'_>), Error> {
    match tokens.peek() {
        Some(Token::LeftBrace) => block_statement(tokens),
        Some(Token::Keyword(Keyword::If)) => if_statement(tokens),
        Some(Token::Keyword(Keyword::Var)) => var_statement(tokens),
        Some(Token::Keyword(Keyword::Fn)) => fn_statement(tokens),
        Some(Token::Keyword(Keyword::While)) => while_statement(tokens),
        Some(Token::Keyword(Keyword::For)) => for_statement(tokens),
        Some(Token::Keyword(Keyword::Return)) => return_statement(tokens),
        _ => expression_statement(tokens),
    }
}

fn braced_scope(tokens: TokenStream<'_>) -> Result<(Vec<Stmt>, TokenStream<'_>), Error> {
    let mut tokens = tokens.expect(Token::LeftBrace)?;

    let mut stmts = vec![];
    while tokens.peek() != Some(Token::RightBrace) {
        let (stmt, remaining_tokens) = statement(tokens)?;

        stmts.push(stmt);
        tokens = remaining_tokens;
    }

    tokens = tokens.expect(Token::RightBrace)?;

    Ok((stmts, tokens))
}

fn block_statement(tokens: TokenStream<'_>) -> Result<(Stmt, TokenStream<'_>), Error> {
    braced_scope(tokens).map(|(stmts, tokens)| (Stmt::Block(BlockStmt { stmts }), tokens))
}

fn fn_statement(tokens: TokenStream<'_>) -> Result<(Stmt, TokenStream<'_>), Error> {
    let tokens = tokens.expect(Keyword::Fn)?;
    let (ident, tokens) = tokens.expect_ident()?;

    let mut tokens = tokens.expect(Token::LeftParen)?;
    let mut params = vec![];
    while tokens.peek() != Some(Token::RightParen) {
        let (param, remaining_tokens) = tokens.expect_ident()?;
        params.push(param);

        match remaining_tokens.next_if(Token::Comma)? {
            Some((_, remaining_tokens)) => tokens = remaining_tokens,
            None => tokens = remaining_tokens,
        }
    }
    tokens = tokens.expect(Token::RightParen)?;

    let (body, tokens) = braced_scope(tokens)?;

    Ok((
        Stmt::Fn(FnStmt {
            ident,
            params,
            body,
        }),
        tokens,
    ))
}

fn if_statement(tokens: TokenStream<'_>) -> Result<(Stmt, TokenStream<'_>), Error> {
    let tokens = tokens.expect(Keyword::If)?;
    let tokens = tokens.expect(Token::LeftParen)?;
    let (condition, tokens) = expression(tokens)?;
    let tokens = tokens.expect(Token::RightParen)?;

    let (then_branch, tokens) = statement(tokens)?;

    let (else_branch, tokens) = match tokens.next_if(Keyword::Else)? {
        Some((_, tokens)) => {
            let (stmt, tokens) = statement(tokens)?;
            (Some(stmt), tokens)
        }
        None => (None, tokens),
    };

    Ok((
        Stmt::If(IfStmt {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        }),
        tokens,
    ))
}

fn return_statement(tokens: TokenStream<'_>) -> Result<(Stmt, TokenStream<'_>), Error> {
    let tokens = tokens.expect(Keyword::Return)?;

    let (expr, tokens) = match tokens.next_if(Token::Semicolon)? {
        Some((_, tokens)) => (None, tokens),
        None => {
            let (expr, tokens) = expression(tokens)?;
            let tokens = tokens.expect(Token::Semicolon)?;
            (Some(expr), tokens)
        }
    };

    Ok((Stmt::Return(ReturnStmt { expr }), tokens))
}

fn var_statement(tokens: TokenStream<'_>) -> Result<(Stmt, TokenStream<'_>), Error> {
    let tokens = tokens.expect(Keyword::Var)?;
    let (ident, tokens) = tokens.expect_ident()?;

    let (init, tokens) = match tokens.next_if(Token::Equal)? {
        Some((_, tokens)) => {
            let (expr, tokens) = expression(tokens)?;
            (Some(expr), tokens)
        }
        None => (None, tokens),
    };

    let tokens = tokens.expect(Token::Semicolon)?;

    Ok((Stmt::Var(VarStmt { ident, init }), tokens))
}

fn while_statement(tokens: TokenStream<'_>) -> Result<(Stmt, TokenStream<'_>), Error> {
    let tokens = tokens.expect(Keyword::While)?;

    let tokens = tokens.expect(Token::LeftParen)?;
    let (condition, tokens) = expression(tokens)?;
    let tokens = tokens.expect(Token::RightParen)?;

    let (body, tokens) = statement(tokens)?;

    Ok((
        Stmt::While(WhileStmt {
            condition,
            body: Box::new(body),
        }),
        tokens,
    ))
}

fn for_statement(tokens: TokenStream<'_>) -> Result<(Stmt, TokenStream<'_>), Error> {
    let tokens = tokens.expect(Keyword::For)?;
    let tokens = tokens.expect(Token::LeftParen)?;
    let (init, tokens) = for_statement_init(tokens)?;
    let (condition, tokens) = for_statement_condition(tokens)?;
    let (expr, tokens) = for_statement_expression(tokens)?;
    let tokens = tokens.expect(Token::RightParen)?;
    let (body, tokens) = statement(tokens)?;

    let condition = condition.unwrap_or(Expr::from(true));
    let body = Stmt::Block(BlockStmt {
        stmts: [Some(body), expr.map(|expr| Stmt::Expr(ExprStmt { expr }))]
            .into_iter()
            .flatten()
            .collect(),
    });
    let while_loop = Stmt::While(WhileStmt {
        condition,
        body: Box::new(body),
    });
    let for_loop = Stmt::Block(BlockStmt {
        stmts: [init, Some(while_loop)].into_iter().flatten().collect(),
    });

    Ok((for_loop, tokens))
}

fn for_statement_init(tokens: TokenStream<'_>) -> Result<(Option<Stmt>, TokenStream<'_>), Error> {
    Ok(match tokens.peek() {
        Some(Token::Semicolon) => {
            let (_, tokens) = tokens.next()?;
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

fn for_statement_condition(
    tokens: TokenStream<'_>,
) -> Result<(Option<Expr>, TokenStream<'_>), Error> {
    Ok(match tokens.next_if(Token::Semicolon)? {
        Some(_) => (None, tokens),
        None => {
            let (expr, tokens) = expression(tokens)?;
            let tokens = tokens.expect(Token::Semicolon)?;
            (Some(expr), tokens)
        }
    })
}

fn for_statement_expression(
    tokens: TokenStream<'_>,
) -> Result<(Option<Expr>, TokenStream<'_>), Error> {
    Ok(match tokens.peek() {
        Some(Token::RightParen) => (None, tokens),
        _ => {
            let (expr, tokens) = expression(tokens)?;
            (Some(expr), tokens)
        }
    })
}

fn expression_statement(tokens: TokenStream<'_>) -> Result<(Stmt, TokenStream<'_>), Error> {
    let (expr, tokens) = expression(tokens)?;
    let tokens = tokens.expect(Token::Semicolon)?;
    Ok((Stmt::Expr(ExprStmt { expr }), tokens))
}

#[cfg(test)]
mod test {
    use {super::*, crate::lexer::lex};

    #[test]
    fn parse_string() {
        let tokens = lex("\"Hello, world!\";");
        let statements = parse(&tokens).unwrap();

        assert_eq!(
            statements,
            vec![Stmt::Expr(ExprStmt {
                expr: Expr::from("Hello, world!")
            })]
        );
    }

    #[test]
    fn parse_unary_expression() {
        let tokens = lex("!true;");
        let statements: Vec<Stmt> = parse(&tokens).unwrap();

        assert_eq!(
            statements,
            vec![Stmt::Expr(ExprStmt {
                expr: Expr::Unary(UnaryExpr {
                    operator: UnaryOperator::Not,
                    right: Box::new(Expr::from(true)),
                })
            })]
        );
    }

    #[test]
    fn parse_terms_and_factors() {
        let tokens = lex("1 + 2 - 3 * 4 / 5;");
        let statements = parse(&tokens).unwrap();

        assert_eq!(
            statements,
            vec![Stmt::Expr(ExprStmt {
                expr: Expr::Binary(BinaryExpr {
                    left: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::from(1)),
                        operator: BinaryOperator::Plus,
                        right: Box::new(Expr::from(2))
                    })),
                    operator: BinaryOperator::Minus,
                    right: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::Binary(BinaryExpr {
                            left: Box::new(Expr::from(3)),
                            operator: BinaryOperator::Multiply,
                            right: Box::new(Expr::from(4))
                        })),
                        operator: BinaryOperator::Divide,
                        right: Box::new(Expr::from(5))
                    }))
                })
            })]
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

        let first = Stmt::Expr(ExprStmt {
            expr: Expr::Callable(CallableExpr {
                callee: Box::new(Expr::Var(VarExpr {
                    ident: Ident::new("foo"),
                })),
                args: vec![],
            }),
        });
        let second = Stmt::Expr(ExprStmt {
            expr: Expr::Callable(CallableExpr {
                callee: Box::new(Expr::Var(VarExpr {
                    ident: Ident::new("foo"),
                })),
                args: vec![Expr::from(5), Expr::from(true)],
            }),
        });
        let third = Stmt::Expr(ExprStmt {
            expr: Expr::Callable(CallableExpr {
                callee: Box::new(Expr::Var(VarExpr {
                    ident: Ident::new("foo"),
                })),
                args: vec![
                    Expr::from(5),
                    Expr::from(true),
                    Expr::from("hello"),
                    Expr::Literal(LiteralExpr::Nil),
                ],
            }),
        });

        assert_eq!(statements, vec![first, second, third]);
    }

    #[test]
    fn parse_function() {
        let program = r#"
        fn foo(a, b) {
            print(a + b);
        }
        "#;

        let tokens = lex(program);
        let statements = parse(&tokens).unwrap();

        assert_eq!(
            statements,
            vec![Stmt::Fn(FnStmt {
                ident: Ident::new("foo"),
                params: vec![Ident::new("a"), Ident::new("b")],
                body: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Callable(CallableExpr {
                        callee: Box::new(Expr::Var(VarExpr {
                            ident: Ident::new("print")
                        })),
                        args: vec![Expr::Binary(BinaryExpr {
                            left: Box::new(Expr::Var(VarExpr {
                                ident: Ident::new("a")
                            })),
                            operator: BinaryOperator::Plus,
                            right: Box::new(Expr::Var(VarExpr {
                                ident: Ident::new("b")
                            }))
                        })]
                    })
                })]
            })]
        );
    }
}
