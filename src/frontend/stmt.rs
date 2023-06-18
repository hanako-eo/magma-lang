use super::expr::Expr;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    VarDeclaration(Expr, Expr),
    FuncDeclaration(String, Vec<String>, Vec<Stmt>),
    Return(Option<Expr>),
}
