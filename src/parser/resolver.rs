use {
    crate::parser::{
        AssignExpr, BinaryExpr, BlockStmt, CallableExpr, Expr, ExprStmt, ExprVisitor, FnStmt,
        GroupExpr, Ident, IfStmt, LiteralExpr, LogicalExpr, ReturnStmt, Stmt, StmtVisitor,
        UnaryExpr, VarExpr, VarStmt, WhileStmt,
    },
    std::collections::HashMap,
};

pub fn resolve(stmts: &[Stmt]) -> Result<Vec<Stmt>, Error> {
    let mut resolver = Resolver::default();
    resolver.resolve(stmts)
}

#[derive(Default)]
struct Resolver {
    scopes: Vec<Scope>,
    inside_function: bool,
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
    pub fn resolve(&mut self, stmts: &[Stmt]) -> Result<Vec<Stmt>, Error> {
        stmts.iter().map(|stmt| self.visit_stmt(stmt)).collect()
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn set_state(&mut self, ident: &Ident, state: VarState) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.locals.insert(ident.clone(), state);
        }
    }

    #[must_use]
    fn resolve_name(&mut self, ident: &Ident) -> Option<(usize, Ident)> {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.locals.contains_key(ident) {
                return scope
                    .locals
                    .get_key_value(ident)
                    .map(|(ident, _)| (i, ident.clone()));
            }
        }
        None
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
        self.pop_scope();

        Ok(Stmt::Block(BlockStmt { stmts }))
    }

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<Self::Output, Self::Error> {
        Ok(Stmt::Expr(ExprStmt {
            expr: self.visit_expr(&stmt.expr)?,
        }))
    }

    fn visit_fn_stmt(&mut self, stmt: &FnStmt) -> Result<Self::Output, Self::Error> {
        self.set_state(&stmt.ident, VarState::Defined);

        let enclosing_function = self.inside_function;
        self.inside_function = true;

        self.push_scope();

        for param in &stmt.params {
            self.set_state(param, VarState::Defined);
        }

        let body = stmt
            .body
            .iter()
            .map(|stmt| self.visit_stmt(stmt))
            .collect::<Result<_, _>>()?;

        self.pop_scope();

        self.inside_function = enclosing_function;

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
        if !self.inside_function {
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
        if let Some((0, _)) = self.resolve_name(&stmt.ident) {
            return Err(Error::AlreadyDeclared(stmt.ident.clone()));
        }

        self.set_state(&stmt.ident, VarState::Declared);
        let init = stmt
            .init
            .as_ref()
            .map(|expr| self.visit_expr(expr))
            .transpose()?;
        self.set_state(&stmt.ident, VarState::Defined);

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

        let ident = match self.resolve_name(&expr.ident) {
            Some((_, resolved)) => resolved,
            None => expr.ident.clone(),
        };

        Ok(Expr::Assign(AssignExpr { ident, value }))
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
        if let Some(scope) = self.scopes.last() {
            if let Some(VarState::Declared) = scope.locals.get(&expr.ident) {
                return Err(Error::InitialisedWithItself(expr.ident.clone()));
            }
        }

        let ident = if let Some((_, resolved)) = self.resolve_name(&expr.ident) {
            resolved
        } else {
            expr.ident.clone()
        };

        Ok(Expr::Var(VarExpr { ident }))
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
                return "b";
            }
            return "a";
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
}
