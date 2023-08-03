use crate::parser::{
    AssignExpr, BinaryExpr, BlockStmt, CallableExpr, Expr, ExprStmt, FnStmt, GroupExpr, IfStmt,
    LiteralExpr, LogicalExpr, ReturnStmt, Stmt, UnaryExpr, VarExpr, VarStmt, WhileStmt,
};

pub trait StmtVisitor {
    type Output;
    type Error;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Self::Output, Self::Error> {
        match stmt {
            Stmt::Block(stmt) => self.visit_block_stmt(stmt),
            Stmt::Expr(stmt) => self.visit_expr_stmt(stmt),
            Stmt::Fn(stmt) => self.visit_fn_stmt(stmt),
            Stmt::If(stmt) => self.visit_if_stmt(stmt),
            Stmt::Return(stmt) => self.visit_return_stmt(stmt),
            Stmt::Var(stmt) => self.visit_var_stmt(stmt),
            Stmt::While(stmt) => self.visit_while_stmt(stmt),
        }
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Result<Self::Output, Self::Error>;
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<Self::Output, Self::Error>;
    fn visit_fn_stmt(&mut self, stmt: &FnStmt) -> Result<Self::Output, Self::Error>;
    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Result<Self::Output, Self::Error>;
    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Result<Self::Output, Self::Error>;
    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Result<Self::Output, Self::Error>;
    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Result<Self::Output, Self::Error>;
}

pub trait ExprVisitor {
    type Output;
    type Error;

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

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<Self::Output, Self::Error>;
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<Self::Output, Self::Error>;
    fn visit_callable_expr(&mut self, expr: &CallableExpr) -> Result<Self::Output, Self::Error>;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<Self::Output, Self::Error>;
    fn visit_group_expr(&mut self, expr: &GroupExpr) -> Result<Self::Output, Self::Error>;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Result<Self::Output, Self::Error>;
    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Result<Self::Output, Self::Error>;
    fn visit_var_expr(&mut self, expr: &VarExpr) -> Result<Self::Output, Self::Error>;
}
