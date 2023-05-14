use crate::parser::{
    AssignExpr, BinaryExpr, BlockStmt, Expr, ExprStmt, GroupExpr, LiteralExpr, PrintStmt, Stmt,
    UnaryExpr, VarExpr, VarStmt,
};

pub trait Visitor {
    type Output;
    type Error;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Error>;
    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Result<(), Self::Error>;
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<(), Self::Error>;
    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Result<(), Self::Error>;
    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Result<(), Self::Error>;

    fn visit_expr(&mut self, expr: &Expr) -> Result<Self::Output, Self::Error>;
    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<Self::Output, Self::Error>;
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<Self::Output, Self::Error>;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<Self::Output, Self::Error>;
    fn visit_group_expr(&mut self, expr: &GroupExpr) -> Result<Self::Output, Self::Error>;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Result<Self::Output, Self::Error>;
    fn visit_var_expr(&mut self, expr: &VarExpr) -> Result<Self::Output, Self::Error>;
}
