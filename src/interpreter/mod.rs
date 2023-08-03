use crate::{
    interpreter::function::Function,
    lexer::lex,
    parser::{
        parse, AssignExpr, BinaryExpr, BinaryOperator, BlockStmt, CallableExpr,
        Error as ParserError, ExprStmt, ExprVisitor, GroupExpr, IfStmt, LiteralExpr, LogicalExpr,
        LogicalOperator, StmtVisitor, UnaryExpr, UnaryOperator, VarExpr, VarStmt, WhileStmt,
    },
};

mod value;
pub use value::Value;

mod env;
use {
    crate::parser::{resolve, FnStmt, Ident, ResolveError, ReturnStmt},
    env::Environment,
};

mod function;

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
        Self {
            environment: Environment::default().with_print(printer),
        }
    }

    pub fn interpret(&mut self, program: &str) -> Result<(), Error> {
        let tokens = lex(program);
        let stmts = parse(&tokens)?;
        let stmts = resolve(&stmts)?;

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
    Resolve(#[from] ResolveError),

    #[error("undefined variable {0}")]
    UndefinedVar(Ident),

    #[error("unexpected unary operator {0}")]
    UnexpectedUnaryOperator(String),

    #[error("unexpected binary operator {0}")]
    UnexpectedBinaryOperator(String),

    #[error("type mismatch: {0}")]
    TypeMismatch(String),

    #[error("can only call functions and classes")]
    NotCallable,

    #[error("arity mismatch, expected {expected} args, got {actual} args")]
    ArityMismatch { expected: usize, actual: usize },

    #[error("wrong argument type")]
    WrongArgType,
}

impl StmtVisitor for Interpreter {
    type Output = Option<Value>;
    type Error = Error;

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Result<Self::Output, Self::Error> {
        self.environment.push_scope();

        let mut result = None;
        for stmt in &stmt.stmts {
            let value = self.visit_stmt(stmt)?;
            if value.is_some() {
                result = value;
                break;
            }
        }

        self.environment.pop_scope();
        Ok(result)
    }

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<Self::Output, Self::Error> {
        let _value = self.visit_expr(&stmt.expr)?;
        Ok(None)
    }

    fn visit_fn_stmt(&mut self, stmt: &FnStmt) -> Result<Self::Output, Self::Error> {
        let function = Function::new(stmt.clone());
        self.environment.define(&stmt.ident, function);
        Ok(None)
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Result<Self::Output, Self::Error> {
        if self.visit_expr(&stmt.condition)?.is_truthy() {
            self.visit_stmt(&stmt.then_branch)
        } else if let Some(else_branch) = &stmt.else_branch {
            self.visit_stmt(else_branch)
        } else {
            Ok(None)
        }
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Result<Self::Output, Self::Error> {
        match &stmt.expr {
            Some(expr) => self.visit_expr(expr).map(Some),
            None => Ok(None),
        }
    }

    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Result<Self::Output, Self::Error> {
        let init_value = if let Some(expr) = &stmt.init {
            self.visit_expr(expr)?
        } else {
            Value::Nil
        };

        self.environment.define(&stmt.ident, init_value);

        Ok(None)
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Result<Self::Output, Self::Error> {
        while self.visit_expr(&stmt.condition)?.is_truthy() {
            let value = self.visit_stmt(&stmt.body)?;
            if value.is_some() {
                return Ok(value);
            }
        }

        Ok(None)
    }
}

impl ExprVisitor for Interpreter {
    type Output = Value;
    type Error = Error;

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<Self::Output, Self::Error> {
        let value = self.visit_expr(&expr.value)?;

        if let Some(var) = self.environment.get_mut(&expr.ident) {
            *var = value;
            return Ok(var.clone());
        }

        Err(Error::UndefinedVar(expr.ident.clone()))
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

        callee.call(args, self).map_err(Error::from)
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
        if let Some(var) = self.environment.get(&expr.ident) {
            return Ok(var.clone());
        }

        Err(Error::UndefinedVar(expr.ident.clone()))
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
            Err(Error::ArityMismatch {
                expected: 1,
                actual: 3
            })
        ));
    }

    #[test]
    fn call_uncallable_type() {
        let mut interpreter = Interpreter::default();

        let program = r#""hello"(1, 2, 3);"#;

        let result = interpreter.interpret(program);
        assert!(matches!(result, Err(Error::NotCallable)));
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

    #[test]
    fn call_declared_function_with_return() {
        let program = r#"
        fn add(a, b) {
            return a + b;
        }

        assert(add(2, 2) == 4);
        "#;

        Interpreter::default().interpret(program).unwrap();
    }

    #[test]
    fn call_declared_function_with_multiple_return() {
        let program = r#"
        fn divide(a, b) {
            if (b == 0) {
                return "bad input";
            }
        
            return a / b;
        }

        assert(divide(2, 0) == "bad input");
        assert(divide(2, 2) == 1);
        "#;

        Interpreter::default().interpret(program).unwrap();
    }

    #[test]
    fn scope_resolution_bug() {
        let program = r#"
        var a = "global";
        {
          fn getA() {
            return a;
          }
        
          assert(getA() == "global");
          var a = "block";
          assert(getA() == "global");
          assert(a == "block");
        }
        "#;

        Interpreter::default().interpret(program).unwrap();
    }

    #[test]
    fn split_declaration_and_definition() {
        let program = r#"
        var x;
        {
          {
            var x = "inner";
            print(x);
          }
          x = 5;
        }
        assert(x == 5);
        "#;

        Interpreter::default().interpret(program).unwrap();
    }

    #[test]
    fn call_function_with_too_many_arguments() {
        let program = r#"
        fn foo(a, b) {
            return a + b;
        }
        
        assert(foo(1, 2, 3) == 3);
        "#;

        let result = Interpreter::default().interpret(program);
        assert!(matches!(
            result,
            Err(Error::ArityMismatch {
                expected: 2,
                actual: 3
            })
        ));
    }
}
