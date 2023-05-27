use {
    crate::{
        lexer::lex,
        parser::{
            parse, AssignExpr, BinaryExpr, BinaryOperator, BlockStmt, CallableExpr,
            Error as ParserError, Expr, ExprStmt, GroupExpr, IfStmt, LiteralExpr, LogicalExpr,
            LogicalOperator, Stmt, UnaryExpr, UnaryOperator, VarExpr, VarStmt, Visitor, WhileStmt,
        },
    },
    std::{
        thread::sleep,
        time::{Duration, SystemTime, UNIX_EPOCH},
    },
};

mod value;
pub use value::{CallError, Error as ValueError, Function, Value};

mod env;
use env::Environment;

pub struct Interpreter {
    environment: Environment,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::with_printer(StdOutPrinter)
    }
}

pub trait Printer {
    fn print(&mut self, value: &Value);
}

pub struct StdOutPrinter;

impl Printer for StdOutPrinter {
    fn print(&mut self, value: &Value) {
        println!("{value}");
    }
}

impl Interpreter {
    pub fn with_printer(printer: impl Printer + 'static) -> Self {
        let mut interpreter = Self {
            environment: Environment::default(),
        };

        let global_scope = interpreter.environment.global_scope();
        global_scope.define("clock", Clock);
        global_scope.define("sleep", Sleep);
        global_scope.define("print", Print(printer));
        global_scope.define("assert", Assert);

        interpreter
    }

    pub fn interpret(&mut self, program: &str) -> Result<(), Error> {
        let tokens = lex(program);
        let stmts = parse(&tokens)?;

        for stmt in stmts {
            self.visit_stmt(&stmt)?;
        }

        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Parser(#[from] ParserError),

    #[error(transparent)]
    Value(#[from] ValueError),

    #[error("undefined variable {0}")]
    UndefinedVar(String),

    #[error("unexpected unary operator {0}")]
    UnexpectedUnaryOperator(String),

    #[error("unexpected binary operator {0}")]
    UnexpectedBinaryOperator(String),
}

impl Visitor for Interpreter {
    type Output = Value;
    type Error = Error;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Error> {
        match stmt {
            Stmt::Block(stmt) => self.visit_block_stmt(stmt),
            Stmt::Expr(stmt) => self.visit_expr_stmt(stmt),
            Stmt::If(stmt) => self.visit_if_stmt(stmt),
            Stmt::Var(stmt) => self.visit_var_stmt(stmt),
            Stmt::While(stmt) => self.visit_while_stmt(stmt),
        }
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Result<(), Self::Error> {
        self.environment.push_scope();
        for stmt in &stmt.stmts {
            self.visit_stmt(stmt)?;
        }
        self.environment.pop_scope();
        Ok(())
    }

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<(), Self::Error> {
        let _value = self.visit_expr(&stmt.expr)?;
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Result<(), Self::Error> {
        if self.visit_expr(&stmt.condition)?.is_truthy() {
            self.visit_stmt(&stmt.then_branch)?;
        } else if let Some(else_branch) = &stmt.else_branch {
            self.visit_stmt(else_branch)?;
        }

        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Result<(), Self::Error> {
        let init_value = if let Some(expr) = &stmt.init {
            self.visit_expr(expr)?
        } else {
            Value::Nil
        };

        self.environment
            .current_scope()
            .define(stmt.ident, init_value);

        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Result<(), Self::Error> {
        while self.visit_expr(&stmt.condition)?.is_truthy() {
            self.visit_stmt(&stmt.body)?;
        }

        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<Self::Output, Self::Error> {
        match expr {
            Expr::Assign(expr) => self.visit_assign_expr(expr),
            Expr::Binary(expr) => self.visit_binary_expr(expr),
            Expr::Callable(expr) => self.visit_callable_expr(expr),
            Expr::Unary(expr) => self.visit_unary_expr(expr),
            Expr::Group(expr) => self.visit_group_expr(expr),
            Expr::Literal(expr) => self.visit_literal_expr(expr),
            Expr::Logical(expr) => self.visit_logical_expr(expr),
            Expr::Var(expr) => self.visit_var_expr(expr),
        }
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<Self::Output, Self::Error> {
        let value = self.visit_expr(&expr.value)?;

        for scope in self.environment.scopes_mut() {
            if let Some(var) = scope.get_mut(expr.ident) {
                *var = value;
                return Ok(var.clone());
            }
        }

        Err(Error::UndefinedVar(expr.ident.to_string()))
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<Self::Output, Self::Error> {
        let lhs = self.visit_expr(&expr.left)?;
        let rhs = self.visit_expr(&expr.right)?;

        match expr.operator {
            BinaryOperator::Plus => Ok((lhs + rhs)?),
            BinaryOperator::Minus => Ok((lhs - rhs)?),
            BinaryOperator::Multiply => Ok((lhs * rhs)?),
            BinaryOperator::Divide => Ok((lhs / rhs)?),
            BinaryOperator::Equal => Ok(Value::Boolean(lhs == rhs)),
            BinaryOperator::NotEqual => Ok(Value::Boolean(lhs != rhs)),
            BinaryOperator::LessThan => Ok(Value::Boolean(lhs < rhs)),
            BinaryOperator::LessThanOrEqual => Ok(Value::Boolean(lhs <= rhs)),
            BinaryOperator::GreaterThan => Ok(Value::Boolean(lhs > rhs)),
            BinaryOperator::GreaterThanOrEqual => Ok(Value::Boolean(lhs >= rhs)),
        }
    }

    fn visit_callable_expr(&mut self, expr: &CallableExpr) -> Result<Self::Output, Self::Error> {
        let mut callee = self.visit_expr(&expr.callee)?;

        let args = expr
            .args
            .iter()
            .map(|arg| self.visit_expr(arg))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(callee.call(args).map_err(value::Error::from)?)
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<Self::Output, Self::Error> {
        let rhs = self.visit_expr(&expr.right)?;

        match expr.operator {
            UnaryOperator::Minus => Ok((-rhs)?),
            UnaryOperator::Not => Ok(!rhs),
        }
    }

    fn visit_group_expr(&mut self, expr: &GroupExpr) -> Result<Self::Output, Self::Error> {
        self.visit_expr(&expr.grouped)
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Result<Self::Output, Self::Error> {
        match expr {
            LiteralExpr::Boolean(value) => Ok(Value::Boolean(*value)),
            LiteralExpr::Nil => Ok(Value::Nil),
            LiteralExpr::Number(value) => Ok(Value::Number(*value)),
            LiteralExpr::String(value) => Ok(Value::String(value.to_string())),
        }
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Result<Self::Output, Self::Error> {
        let left = self.visit_expr(&expr.left)?;

        match expr.operator {
            LogicalOperator::And if !left.is_truthy() => Ok(left),
            LogicalOperator::Or if left.is_truthy() => Ok(left),
            _ => self.visit_expr(&expr.right),
        }
    }

    fn visit_var_expr(&mut self, expr: &VarExpr) -> Result<Self::Output, Self::Error> {
        for scope in self.environment.scopes() {
            if let Some(var) = scope.get(expr.ident) {
                return Ok(var.clone());
            }
        }

        Err(Error::UndefinedVar(expr.ident.to_string()))
    }
}

struct Clock;

impl Function for Clock {
    fn arity(&self) -> Option<usize> {
        Some(0)
    }

    fn call(&mut self, _args: Vec<Value>) -> Result<Value, CallError> {
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

struct Sleep;

impl Function for Sleep {
    fn arity(&self) -> Option<usize> {
        Some(1)
    }

    fn call(&mut self, args: Vec<Value>) -> Result<Value, CallError> {
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

struct Print<P>(P);

impl<P> Function for Print<P>
where
    P: Printer,
{
    fn arity(&self) -> Option<usize> {
        None
    }

    fn call(&mut self, args: Vec<Value>) -> Result<Value, CallError> {
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

struct Assert;

impl Function for Assert {
    fn arity(&self) -> Option<usize> {
        None
    }

    fn call(&mut self, args: Vec<Value>) -> Result<Value, CallError> {
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

#[cfg(test)]
mod test {
    use {
        super::*,
        std::{cell::RefCell, rc::Rc},
    };

    #[derive(Default, Clone)]
    struct SpyPrinter(Rc<RefCell<Vec<Value>>>);

    impl Printer for SpyPrinter {
        fn print(&mut self, value: &Value) {
            self.0.borrow_mut().push(value.clone());
        }
    }

    impl SpyPrinter {
        fn into_inner(self) -> Vec<Value> {
            self.0.borrow().clone()
        }
    }

    #[test]
    fn if_statements() {
        let spy_printer = SpyPrinter::default();
        let mut interpreter = Interpreter::with_printer(spy_printer.clone());

        let program = r#"
        var x = 3;
        if (x == 3) {
            print(5);
        } else {
            print(4);
        }
        "#;

        interpreter.interpret(program).unwrap();

        let output = spy_printer.into_inner();

        assert_eq!(output, vec![Value::Number(5.0)]);
    }

    #[test]
    fn string_equality() {
        let mut interpreter = Interpreter::default();

        let program = r#"
        assert ("hello" == "hello");
        assert ("hello" != "world");
        "#;

        interpreter.interpret(program).unwrap();
    }

    #[test]
    fn logical_expressions() {
        let spy_printer = SpyPrinter::default();
        let mut interpreter = Interpreter::with_printer(spy_printer.clone());

        let program = r#"
        print(true and (1 == 1) and (2 + 2 == 4));
        print(true and (1 == 1) and (2 + 2 == 5));

        print(true or (1 == 2) or false);
        print(false or true);

        print("hi" or 2);
        print(nil or "yes");
        "#;

        interpreter.interpret(program).unwrap();

        let output = spy_printer.into_inner();

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
        let mut interpreter = Interpreter::default();

        let program = r#"
        var a = "global a";
        var b = "global b";
        var c = "global c";
        {
          var a = "outer a";
          var b = "outer b";
          {
            var a = "inner a";
            assert(a == "inner a", b == "outer b", c == "global c");
          }
          assert(a == "outer a", b == "outer b", c == "global c");
        }
        assert(a == "global a", b == "global b", c == "global c");
        "#;

        interpreter.interpret(program).unwrap();
    }

    #[test]
    fn while_loop() {
        let mut interpreter = Interpreter::default();

        let program = r#"
        var i = 0;
        while (i != 5) {
            assert (i < 5);
            i = i + 1;
        }
        assert(i == 5);
        "#;

        interpreter.interpret(program).unwrap();
    }

    #[test]
    fn for_loop() {
        let spy_printer = SpyPrinter::default();
        let mut interpreter = Interpreter::with_printer(spy_printer.clone());

        let program = "for (var i = 0; i != 5; i = i + 1) print(i);";

        interpreter.interpret(program).unwrap();

        let output = spy_printer.into_inner();

        assert_eq!(
            output,
            vec![
                Value::Number(0.0),
                Value::Number(1.0),
                Value::Number(2.0),
                Value::Number(3.0),
                Value::Number(4.0),
            ]
        );
    }

    #[test]
    fn for_loop_components_are_optional() {
        let mut interpreter = Interpreter::default();

        let program = r#"
        var i = 0;
        for (; i != 5;) {
            i = i + 1;
        }
        assert(i == 5);
        "#;

        interpreter.interpret(program).unwrap();
    }

    #[test]
    fn call_function() {
        let mut interpreter = Interpreter::default();

        let program = r#"
        var start = clock();
        sleep(10);
        var stop = clock();
        assert(stop > start);
        "#;

        interpreter.interpret(program).unwrap();
    }

    #[test]
    fn call_function_with_wrong_number_of_args() {
        let mut interpreter = Interpreter::default();

        let result = interpreter.interpret("sleep(1, 2, 3);");

        assert!(matches!(
            result,
            Err(Error::Value(ValueError::CallError(
                CallError::ArityMismatch {
                    expected: 1,
                    actual: 3
                }
            )))
        ));
    }

    #[test]
    fn call_uncallable_type() {
        let mut interpreter = Interpreter::default();

        let program = r#""hello"(1, 2, 3);"#;

        let result = interpreter.interpret(program);
        assert!(matches!(
            result,
            Err(Error::Value(ValueError::CallError(CallError::NotCallable)))
        ));
    }

    #[test]
    fn print_function_variadic_number_of_args() {
        let spy_printer = SpyPrinter::default();
        let mut interpreter = Interpreter::with_printer(spy_printer.clone());

        let program = r#"
        print(1, true, "hello", nil);
        "#;

        interpreter.interpret(program).unwrap();

        let output = spy_printer.into_inner();

        assert_eq!(
            output,
            vec![
                Value::Number(1.0),
                Value::Boolean(true),
                Value::String("hello".to_string()),
                Value::Nil
            ]
        );
    }

    #[test]
    fn assertion_pass_does_not_panic() {
        let mut interpreter = Interpreter::default();

        let program = r#"
        assert(2 + 2 == 4);
        "#;

        interpreter.interpret(program).unwrap();
    }

    #[test]
    #[should_panic]
    fn assertion_failure_will_panic() {
        let mut interpreter = Interpreter::default();

        let program = r#"
        assert(2 + 2 == 5);
        "#;

        interpreter.interpret(program).unwrap();
    }
}
