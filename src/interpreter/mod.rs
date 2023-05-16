use {
    crate::{
        lexer::{lex, Token},
        parser::{
            self, parse, AssignExpr, BinaryExpr, BlockStmt, Expr, ExprStmt, GroupExpr, IfStmt,
            LiteralExpr, LogicalExpr, LogicalOperator, PrintStmt, Stmt, UnaryExpr, VarExpr,
            VarStmt, Visitor,
        },
    },
    std::collections::HashMap,
};

mod value;
pub use value::Value;

pub struct Interpreter<P> {
    printer: P,
    environments: Vec<HashMap<String, Value>>,
}

impl Default for Interpreter<StdOutPrinter> {
    fn default() -> Self {
        Self::with_printer(StdOutPrinter)
    }
}

pub trait Printer {
    fn print(&mut self, value: Value);
}

pub struct StdOutPrinter;

impl Printer for StdOutPrinter {
    fn print(&mut self, value: Value) {
        println!("{value}");
    }
}

impl<P> Interpreter<P>
where
    P: Printer,
{
    pub fn with_printer(printer: P) -> Self {
        Self {
            printer,
            environments: vec![HashMap::new()],
        }
    }

    pub fn interpret(&mut self, program: &str) -> Result<(), Error> {
        let tokens = lex(program);
        let stmts = parse(&tokens)?;

        for stmt in stmts {
            self.visit_stmt(&stmt)?;
        }

        Ok(())
    }

    fn into_printer(self) -> P {
        self.printer
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Parser(#[from] parser::Error),

    #[error(transparent)]
    Value(#[from] value::Error),

    #[error("undefined variable {0}")]
    UndefinedVar(String),

    #[error("unexpected unary operator {0:?}")]
    UnexpectedUnaryOperator(Token),

    #[error("unexpected binary operator {0:?}")]
    UnexpectedBinaryOperator(Token),
}

impl<P> Visitor for Interpreter<P>
where
    P: Printer,
{
    type Output = Value;
    type Error = Error;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Error> {
        match stmt {
            Stmt::Block(stmt) => self.visit_block_stmt(stmt),
            Stmt::Expr(stmt) => self.visit_expr_stmt(stmt),
            Stmt::If(stmt) => self.visit_if_stmt(stmt),
            Stmt::Print(stmt) => self.visit_print_stmt(stmt),
            Stmt::Var(stmt) => self.visit_var_stmt(stmt),
        }
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Result<(), Self::Error> {
        self.environments.push(HashMap::default());
        for stmt in stmt.stmts() {
            self.visit_stmt(stmt)?;
        }
        self.environments.pop();
        Ok(())
    }

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<(), Self::Error> {
        let _value = self.visit_expr(stmt.expr())?;
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Result<(), Self::Error> {
        if self.visit_expr(stmt.condition())?.is_truthy() {
            self.visit_stmt(stmt.then_branch())?;
        } else if let Some(else_branch) = stmt.else_branch() {
            self.visit_stmt(else_branch)?;
        }

        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Result<(), Self::Error> {
        let value = self.visit_expr(stmt.expr())?;
        self.printer.print(value);
        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Result<(), Self::Error> {
        let init_value = if let Some(expr) = stmt.init() {
            self.visit_expr(expr)?
        } else {
            Value::Nil
        };

        self.environments
            .last_mut()
            .expect("at least one environment")
            .insert(stmt.ident().to_string(), init_value);

        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<Self::Output, Self::Error> {
        match expr {
            Expr::Assign(expr) => self.visit_assign_expr(expr),
            Expr::Binary(expr) => self.visit_binary_expr(expr),
            Expr::Unary(expr) => self.visit_unary_expr(expr),
            Expr::Group(expr) => self.visit_group_expr(expr),
            Expr::Literal(expr) => self.visit_literal_expr(expr),
            Expr::Logical(expr) => self.visit_logical_expr(expr),
            Expr::Var(expr) => self.visit_var_expr(expr),
        }
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<Self::Output, Self::Error> {
        let value = self.visit_expr(expr.value())?;

        for environment in self.environments.iter_mut().rev() {
            if environment.contains_key(expr.ident()) {
                environment.insert(expr.ident().to_string(), value.clone());
                return Ok(value);
            }
        }

        Err(Error::UndefinedVar(expr.ident().to_string()))
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

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Result<Self::Output, Self::Error> {
        let left = self.visit_expr(expr.left())?;

        match expr.operator() {
            LogicalOperator::And if !left.is_truthy() => Ok(left),
            LogicalOperator::Or if left.is_truthy() => Ok(left),
            _ => self.visit_expr(expr.right()),
        }
    }

    fn visit_var_expr(&mut self, expr: &VarExpr) -> Result<Self::Output, Self::Error> {
        for environment in self.environments.iter().rev() {
            if let Some(value) = environment.get(expr.ident()) {
                return Ok(value.clone());
            }
        }

        Err(Error::UndefinedVar(expr.ident().to_string()))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Default)]
    struct SpyPrinter(Vec<Value>);

    impl Printer for SpyPrinter {
        fn print(&mut self, value: Value) {
            self.0.push(value);
        }
    }

    #[test]
    fn if_statements() {
        let mut interpreter = Interpreter::with_printer(SpyPrinter::default());

        let program = r#"
        var x = 3;
        if (x == 3) {
            print 5;
        } else {
            print 4;
        }
        "#;

        interpreter.interpret(program).unwrap();

        let SpyPrinter(output) = interpreter.into_printer();

        assert_eq!(output, vec![Value::Number(5.0)]);
    }

    #[test]
    fn logical_expressions() {
        let mut interpreter = Interpreter::with_printer(SpyPrinter::default());

        let program = r#"
        print true and (1 == 1) and (2 + 2 == 4);
        print true and (1 == 1) and (2 + 2 == 5);

        print true or (1 == 2) or false;
        print false or true;

        print "hi" or 2;
        print nil or "yes";
        "#;

        interpreter.interpret(program).unwrap();

        let SpyPrinter(output) = interpreter.into_printer();

        assert_eq!(
            output,
            vec![
                Value::Boolean(true),
                Value::Boolean(false),
                Value::Boolean(true),
                Value::Boolean(true),
                Value::String("hi".to_string()),
                Value::String("yes".to_string())
            ]
        );
    }

    #[test]
    fn scoping_variables() {
        let mut interpreter = Interpreter::with_printer(SpyPrinter::default());

        let program = r#"
        var a = "global a";
        var b = "global b";
        var c = "global c";
        {
          var a = "outer a";
          var b = "outer b";
          {
            var a = "inner a";
            print a;
            print b;
            print c;
          }
          print a;
          print b;
          print c;
        }
        print a;
        print b;
        print c;
        "#;

        interpreter.interpret(program).unwrap();

        let SpyPrinter(output) = interpreter.into_printer();

        assert_eq!(
            output,
            vec![
                Value::String("inner a".to_string()),
                Value::String("outer b".to_string()),
                Value::String("global c".to_string()),
                Value::String("outer a".to_string()),
                Value::String("outer b".to_string()),
                Value::String("global c".to_string()),
                Value::String("global a".to_string()),
                Value::String("global b".to_string()),
                Value::String("global c".to_string()),
            ]
        );
    }
}
