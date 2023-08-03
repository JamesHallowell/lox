use {
    crate::parser::{
        AssignExpr, BinaryExpr, BlockStmt, CallableExpr, Expr, ExprStmt, ExprVisitor, FnStmt,
        GroupExpr, Ident, IfStmt, LiteralExpr, LogicalExpr, ReturnStmt, Stmt, StmtVisitor,
        UnaryExpr, VarExpr, VarStmt, WhileStmt,
    },
    std::{
        collections::{hash_map::Entry, HashMap},
        mem,
    },
};

pub fn resolve(stmts: &[Stmt]) -> Result<Vec<Stmt>, Error> {
    let mut resolver = Resolver::default();
    resolver.resolve(stmts)
}

#[derive(Default)]
struct Resolver {
    scopes: Vec<Scope>,
    state: ResolverState,
}

#[derive(Debug, Clone, Default)]
enum ResolverState {
    #[default]
    None,
    InsideFunction,
    InitialisingVar(Ident),
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("variable {0} cannot appear in its own initializer")]
    InitialisedWithItself(Ident),

    #[error("variable {0} is already declared in this scope")]
    AlreadyDeclared(Ident),

    #[error("cannot return from top-level code")]
    InvalidReturn,

    #[error("variable {0} is not used")]
    Unused(Ident),

    #[error("variable {0} accessed before it was defined")]
    Undefined(Ident),
}

enum VarState {
    Declared,
    Defined,
    Accessed,
}

#[derive(Default)]
struct Scope {
    locals: HashMap<Ident, VarState>,
}

impl Resolver {
    fn resolve(&mut self, stmts: &[Stmt]) -> Result<Vec<Stmt>, Error> {
        stmts.iter().map(|stmt| self.visit_stmt(stmt)).collect()
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop_scope(&mut self) -> Result<(), Error> {
        if let Some(scope) = self.scopes.pop() {
            for (ident, state) in scope.locals {
                if !matches!(state, VarState::Accessed) {
                    return Err(Error::Unused(ident));
                }
            }
        }
        Ok(())
    }

    fn set_state(&mut self, ident: &Ident, state: VarState) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.locals.insert(ident.clone(), state);
        }
    }

    fn is_ident_already_declared_at_current_scope(&self, ident: &Ident) -> bool {
        self.scopes
            .last()
            .map(|scope| scope.locals.contains_key(ident))
            .unwrap_or(false)
    }
}

impl StmtVisitor for Resolver {
    type Output = Stmt;
    type Error = Error;

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Result<Self::Output, Self::Error> {
        self.push_scope();
        let stmts = stmt
            .stmts
            .iter()
            .map(|stmt| self.visit_stmt(stmt))
            .collect::<Result<_, _>>()?;
        self.pop_scope()?;

        Ok(Stmt::Block(BlockStmt { stmts }))
    }

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<Self::Output, Self::Error> {
        Ok(Stmt::Expr(ExprStmt {
            expr: self.visit_expr(&stmt.expr)?,
        }))
    }

    fn visit_fn_stmt(&mut self, stmt: &FnStmt) -> Result<Self::Output, Self::Error> {
        if self.is_ident_already_declared_at_current_scope(&stmt.ident) {
            return Err(Error::AlreadyDeclared(stmt.ident.clone()));
        }

        self.set_state(&stmt.ident, VarState::Defined);

        let mut previous_state = mem::replace(&mut self.state, ResolverState::InsideFunction);

        self.push_scope();

        for param in &stmt.params {
            self.set_state(param, VarState::Defined);
        }

        let body = stmt
            .body
            .iter()
            .map(|stmt| self.visit_stmt(stmt))
            .collect::<Result<_, _>>()?;

        self.pop_scope()?;

        mem::swap(&mut self.state, &mut previous_state);

        Ok(Stmt::Fn(FnStmt {
            ident: stmt.ident.clone(),
            params: stmt.params.clone(),
            body,
        }))
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Result<Self::Output, Self::Error> {
        let condition = self.visit_expr(&stmt.condition)?;
        let then_branch = Box::new(self.visit_stmt(&stmt.then_branch)?);
        let else_branch = stmt
            .else_branch
            .as_ref()
            .map(|stmt| self.visit_stmt(stmt))
            .transpose()?
            .map(Box::new);

        Ok(Stmt::If(IfStmt {
            condition,
            then_branch,
            else_branch,
        }))
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Result<Self::Output, Self::Error> {
        if !matches!(self.state, ResolverState::InsideFunction) {
            return Err(Error::InvalidReturn);
        }

        let expr = stmt
            .expr
            .as_ref()
            .map(|expr| self.visit_expr(expr))
            .transpose()?;

        Ok(Stmt::Return(ReturnStmt { expr }))
    }

    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Result<Self::Output, Self::Error> {
        if self.is_ident_already_declared_at_current_scope(&stmt.ident) {
            return Err(Error::AlreadyDeclared(stmt.ident.clone()));
        }

        self.set_state(&stmt.ident, VarState::Declared);

        let mut previous_state = mem::replace(
            &mut self.state,
            ResolverState::InitialisingVar(stmt.ident.clone()),
        );

        let init = stmt
            .init
            .as_ref()
            .map(|expr| self.visit_expr(expr))
            .transpose()?;

        mem::swap(&mut self.state, &mut previous_state);

        if stmt.init.is_some() {
            self.set_state(&stmt.ident, VarState::Defined);
        }

        Ok(Stmt::Var(VarStmt {
            ident: stmt.ident.clone(),
            init,
        }))
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Result<Self::Output, Self::Error> {
        let condition = self.visit_expr(&stmt.condition)?;
        let body = self.visit_stmt(&stmt.body).map(Box::new)?;

        Ok(Stmt::While(WhileStmt { condition, body }))
    }
}

impl ExprVisitor for Resolver {
    type Output = Expr;
    type Error = Error;

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<Self::Output, Self::Error> {
        let value = self.visit_expr(&expr.value).map(Box::new)?;

        for scope in self.scopes.iter_mut().rev() {
            if let Entry::Occupied(mut entry) = scope.locals.entry(expr.ident.clone()) {
                entry.insert(VarState::Accessed);

                return Ok(Expr::Assign(AssignExpr {
                    ident: entry.key().clone(),
                    value,
                }));
            }
        }

        Ok(Expr::Assign(AssignExpr {
            ident: expr.ident.clone(),
            value,
        }))
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<Self::Output, Self::Error> {
        let left = self.visit_expr(&expr.left).map(Box::new)?;
        let right = self.visit_expr(&expr.right).map(Box::new)?;

        Ok(Expr::Binary(BinaryExpr {
            left,
            operator: expr.operator,
            right,
        }))
    }

    fn visit_callable_expr(&mut self, expr: &CallableExpr) -> Result<Self::Output, Self::Error> {
        let callee = self.visit_expr(&expr.callee).map(Box::new)?;
        let args = expr
            .args
            .iter()
            .map(|arg| self.visit_expr(arg))
            .collect::<Result<_, _>>()?;

        Ok(Expr::Callable(CallableExpr { callee, args }))
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<Self::Output, Self::Error> {
        let right = self.visit_expr(&expr.right).map(Box::new)?;

        Ok(Expr::Unary(UnaryExpr {
            operator: expr.operator,
            right,
        }))
    }

    fn visit_group_expr(&mut self, expr: &GroupExpr) -> Result<Self::Output, Self::Error> {
        let grouped = self.visit_expr(&expr.grouped).map(Box::new)?;

        Ok(Expr::Group(GroupExpr { grouped }))
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Result<Self::Output, Self::Error> {
        Ok(Expr::Literal(expr.clone()))
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Result<Self::Output, Self::Error> {
        let left = self.visit_expr(&expr.left).map(Box::new)?;
        let right = self.visit_expr(&expr.right).map(Box::new)?;

        Ok(Expr::Logical(LogicalExpr {
            left,
            operator: expr.operator,
            right,
        }))
    }

    fn visit_var_expr(&mut self, expr: &VarExpr) -> Result<Self::Output, Self::Error> {
        if matches!(&self.state, ResolverState::InitialisingVar(ident) if ident == &expr.ident) {
            return Err(Error::InitialisedWithItself(expr.ident.clone()));
        }

        for scope in self.scopes.iter_mut().rev() {
            if let Entry::Occupied(mut entry) = scope.locals.entry(expr.ident.clone()) {
                let previous_state = entry.insert(VarState::Accessed);

                if matches!(previous_state, VarState::Declared) {
                    return Err(Error::Undefined(entry.key().clone()));
                }

                return Ok(Expr::Var(VarExpr {
                    ident: entry.key().clone(),
                }));
            }
        }

        Ok(Expr::Var(VarExpr {
            ident: expr.ident.clone(),
        }))
    }
}

#[cfg(test)]
mod test {
    use {
        super::*,
        crate::{lexer::lex, parser::parse},
    };

    #[test]
    fn variables_cannot_appear_in_their_own_initializer() {
        let program = r#"
        fn foo() {
          var a = a;
        }
        "#;

        let result = resolve(&parse(&lex(program)).unwrap());
        assert!(matches!(result, Err(Error::InitialisedWithItself(_))));
    }

    #[test]
    fn cannot_redeclare_variables_in_the_same_scope() {
        let program = r#"
        fn foo(x) {
          var x = 10;
          return x;
        }
        "#;

        let result = resolve(&parse(&lex(program)).unwrap());
        assert!(matches!(result, Err(Error::AlreadyDeclared(_))));
    }

    #[test]
    fn cannot_return_from_top_level_code() {
        let program = r#"
        return "whoops!";
        "#;

        let result = resolve(&parse(&lex(program)).unwrap());
        assert!(matches!(result, Err(Error::InvalidReturn)));
    }

    #[test]
    fn handles_returns_with_nested_functions() {
        let program = r#"
        fn a() {
            fn b() {
                fn c() {
                    return "c";
                }
                return c();
            }
            return b();
        }
        "#;

        let result = resolve(&parse(&lex(program)).unwrap());
        assert!(result.is_ok());
    }

    #[test]
    fn unused_variables_are_reported_as_errors() {
        let program = r#"
        fn foo() {
            var a = 10;
        }
        "#;

        let result = resolve(&parse(&lex(program)).unwrap());
        assert!(matches!(result, Err(Error::Unused(_))));
    }

    #[test]
    fn undefined_variables_are_reported_as_errors() {
        let program = r#"
        fn foo() {
            var a;
            print(a);
        }
        "#;

        let result = resolve(&parse(&lex(program)).unwrap());
        assert!(matches!(result, Err(Error::Undefined(_))));
    }

    #[test]
    fn vars_can_be_defined_in_different_scopes() {
        let program = r#"
        fn foo() {
            var a;
            a = 3;
            
            var b;
            {
              b = 5;
            }
            
            assert(a == 3);
            assert(b == 5);
        }
        "#;

        let result = resolve(&parse(&lex(program)).unwrap());
        assert!(result.is_ok());
    }

    #[test]
    fn cannot_declare_functions_with_same_identifier_within_scope() {
        let program = r#"
        {
          var foo;
          fn foo() {} // foo already exists!
        }
        "#;

        let result = resolve(&parse(&lex(program)).unwrap());
        assert!(matches!(result, Err(Error::AlreadyDeclared(_))));
    }
}
