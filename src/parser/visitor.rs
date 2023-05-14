use crate::parser::{BinaryExpr, Expr, GroupExpr, LiteralExpr, UnaryExpr};

pub trait Visitor {
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary(expr) => self.visit_binary_expr(expr),
            Expr::Group(expr) => self.visit_group_expr(expr),
            Expr::Literal(expr) => self.visit_literal_expr(expr),
            Expr::Unary(expr) => self.visit_unary_expr(expr),
        }
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) {
        self.visit_expr(expr.left());
        self.visit_expr(expr.right());
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) {
        self.visit_expr(expr.right());
    }

    fn visit_group_expr(&mut self, expr: &GroupExpr) {
        self.visit_expr(expr.grouped());
    }

    fn visit_literal_expr(&mut self, _: &LiteralExpr) {}
}

pub fn visit<V>(visitor: &mut V, expr: &Expr)
where
    V: Visitor,
{
    visitor.visit_expr(expr);
}
