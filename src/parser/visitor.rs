use crate::parser::{BinaryExpr, Expr, GroupExpr, LiteralExpr, UnaryExpr};

pub trait Visitor {
    type Output;
    type Error;

    fn visit_expr(&mut self, expr: &Expr) -> Result<Self::Output, Self::Error>;
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<Self::Output, Self::Error>;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<Self::Output, Self::Error>;
    fn visit_group_expr(&mut self, expr: &GroupExpr) -> Result<Self::Output, Self::Error>;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Result<Self::Output, Self::Error>;
}
