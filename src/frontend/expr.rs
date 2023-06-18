use super::stmt::Stmt;

#[derive(Debug, PartialEq)]
pub enum Expr {
    IntLiteral(String),
    FloatLiteral(String),
    CharLiteral(char),
    StrLiteral(String),
    Identifier(String),

    Assign(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),  // a == b
    Ne(Box<Expr>, Box<Expr>),  // a != b
    Lt(Box<Expr>, Box<Expr>),  // a < b
    Le(Box<Expr>, Box<Expr>),  // a <= b
    Gt(Box<Expr>, Box<Expr>),  // a > b
    Ge(Box<Expr>, Box<Expr>),  // a >= b
    Add(Box<Expr>, Box<Expr>), // a + b
    Sub(Box<Expr>, Box<Expr>), // a - b
    Mul(Box<Expr>, Box<Expr>), // a * b
    Div(Box<Expr>, Box<Expr>), // a / b
    Call(Box<Expr>, Vec<Expr>),

    IfBranch {
        condition: Box<Expr>,
        body: Vec<Stmt>,
        else_branch: Option<Box<Expr>>,
    },
    ElseBranch(Vec<Stmt>),

    WhileLoop(Box<Expr>, Vec<Stmt>),
}
