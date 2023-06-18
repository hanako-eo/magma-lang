mod expr;
mod stmt;

use expr::Expr;
use stmt::Stmt;

peg::parser!(pub grammar parser() for str {
    pub rule statements() -> Vec<Stmt>
        = s:(_ s:statement() _ ";"* _ { s })* { s }

    rule statement() -> Stmt
        = function()
        / return()
        / v:var_declaration() ";" { v }
        / c:if_else() { Stmt::Expr(c) }
        / w:while_loop() { Stmt::Expr(w) }
        / e:expression() ";" { Stmt::Expr(e) }

    rule function() -> Stmt
        = "fn" _ name:identifier() _ "(" params:((_ i:identifier() _ { i }) ** ",") ")" _ "{" _ stmts:statements() _ "}" {
            Stmt::FuncDeclaration(name, params, stmts)
        }

    rule return() -> Stmt
        = "return" _ e:expression()? ";" { Stmt::Return(e) }

    rule var_declaration() -> Stmt
        = "let" _ i:identifier() _ "=" _ e:expression() {Stmt::VarDeclaration(Expr::Identifier(i), e)}

    rule expression() -> Expr
        = if_else()
        / while_loop()
        / assignment()
        / binary_op()

    rule if_else() -> Expr
        = "if" _ e:expression() _ "{" _ body:statements() _ "}" _ "else" _ else_branch:if_else() { Expr::IfBranch {
            condition: Box::new(e), body, else_branch: Some(Box::new(else_branch))
        } }
        / "if" _ e:expression() _ "{" _ body:statements() _ "}" _ "else" _ "{" _ else_body:statements() _ "}" _ { Expr::IfBranch {
            condition: Box::new(e), body, else_branch: Some(Box::new(Expr::ElseBranch(else_body)))
        } }
        / "if" _ e:expression() _ "{" _ body:statements() _ "}" { Expr::IfBranch {
            condition: Box::new(e), body, else_branch: None
        } }

    rule while_loop() -> Expr
        = "while" _ e:expression() _ "{" _ loop_body:statements() _ "}" { Expr::WhileLoop(Box::new(e), loop_body) }

    rule assignment() -> Expr
        = i:identifier() _ "=" _ e:expression() {Expr::Assign(Box::new(Expr::Identifier(i)), Box::new(e))}

    rule binary_op() -> Expr = precedence!{
        a:@ _ "==" _ b:(@) { Expr::Eq(Box::new(a), Box::new(b)) }
        a:@ _ "!=" _ b:(@) { Expr::Ne(Box::new(a), Box::new(b)) }
        a:@ _ "<"  _ b:(@) { Expr::Lt(Box::new(a), Box::new(b)) }
        a:@ _ "<=" _ b:(@) { Expr::Le(Box::new(a), Box::new(b)) }
        a:@ _ ">"  _ b:(@) { Expr::Gt(Box::new(a), Box::new(b)) }
        a:@ _ ">=" _ b:(@) { Expr::Ge(Box::new(a), Box::new(b)) }
        --
        a:@ _ "+" _ b:(@) { Expr::Add(Box::new(a), Box::new(b)) }
        a:@ _ "-" _ b:(@) { Expr::Sub(Box::new(a), Box::new(b)) }
        --
        a:@ _ "*" _ b:(@) { Expr::Mul(Box::new(a), Box::new(b)) }
        a:@ _ "/" _ b:(@) { Expr::Div(Box::new(a), Box::new(b)) }
        --
        i:identifier() _ "(" args:((_ e:expression() _ {e}) ** ",") ")" { Expr::Call(Box::new(Expr::Identifier(i)), args) }
        i:identifier() { Expr::Identifier(i) }
        l:literal() { l }
        "(" _ a:binary_op() _ ")" { a }
    }

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("identifier")

    rule literal() -> Expr
        = n:$(['0'..='9']* "." ['0'..='9']+) { Expr::FloatLiteral(n.to_owned()) }
        / n:$(['0'..='9']+) { Expr::IntLiteral(n.to_owned()) }
        / "'" n:$([^'\'' | '\\'] / "\\" ['n' | 't' | 'r' | '\'' | '\\']) "'" { Expr::CharLiteral(match n {
            "\\n" => '\n',
            "\\t" => '\t',
            "\\r" => '\r',
            "\\\\" => '\\',
            "\\'" => '\'',
            n => n.as_bytes()[0] as char
        }) }
        / "\"" c:string_char()* "\"" { Expr::StrLiteral(String::from_iter(c)) }

    rule string_char() -> char
        = n:$(([^'"' | '\\'] / "\\" ['n' | 't' | 'r' | '"' | '\\'])) {
            match n {
                "\\n" => '\n',
                "\\t" => '\t',
                "\\r" => '\r',
                "\\\\" => '\\',
                "\\\"" => '"',
                n => n.as_bytes()[0] as char
            }
        }

    rule _() = quiet!{[' ' | '\t' | '\r' | '\n']*}
});

mod tests {
    use super::*;

    #[test]
    fn function_parsing() {
        assert_eq!(
            parser::statements("fn main() {}"),
            Ok(vec![Stmt::FuncDeclaration("main".into(), vec![], vec![])])
        );
    }

    #[test]
    fn function_with_arg_parsing() {
        assert_eq!(
            parser::statements("fn main(test) {}"),
            Ok(vec![Stmt::FuncDeclaration(
                "main".into(),
                vec!["test".into()],
                vec![]
            )])
        );
    }

    #[test]
    fn function_with_body_parsing() {
        assert_eq!(
            parser::statements("fn main() {2;}"),
            Ok(vec![Stmt::FuncDeclaration(
                "main".into(),
                vec![],
                vec![Stmt::Expr(Expr::IntLiteral("2".into()))]
            )])
        );
    }

    #[test]
    fn literal_parsing() {
        assert_eq!(
            parser::statements("2;2.0;'\\n';\"test\\ntest\";"),
            Ok(vec![
                Stmt::Expr(Expr::IntLiteral("2".into())),
                Stmt::Expr(Expr::FloatLiteral("2.0".into())),
                Stmt::Expr(Expr::CharLiteral('\n')),
                Stmt::Expr(Expr::StrLiteral("test\ntest".into()))
            ])
        );
    }

    #[test]
    fn var_parsing() {
        assert_eq!(
            parser::statements("let a = 2;"),
            Ok(vec![Stmt::VarDeclaration(
                Expr::Identifier("a".into()),
                Expr::IntLiteral("2".into())
            )])
        );
    }

    #[test]
    fn return_parsing() {
        assert_eq!(
            parser::statements("return 2;"),
            Ok(vec![Stmt::Return(Some(Expr::IntLiteral("2".into())))])
        );
    }

    #[test]
    fn return_empty_parsing() {
        assert_eq!(parser::statements("return;"), Ok(vec![Stmt::Return(None)]));
    }

    #[test]
    fn while_parsing() {
        assert_eq!(
            parser::statements("while 1 {}"),
            Ok(vec![Stmt::Expr(Expr::WhileLoop(
                Box::new(Expr::IntLiteral("1".into())),
                vec![]
            ))])
        );
    }

    #[test]
    fn if_parsing() {
        assert_eq!(
            parser::statements("if 1 {}"),
            Ok(vec![Stmt::Expr(Expr::IfBranch {
                condition: Box::new(Expr::IntLiteral("1".into())),
                body: vec![],
                else_branch: None
            })])
        );
    }

    #[test]
    fn if_else_parsing() {
        assert_eq!(
            parser::statements("if 1 {} else {}"),
            Ok(vec![Stmt::Expr(Expr::IfBranch {
                condition: Box::new(Expr::IntLiteral("1".into())),
                body: vec![],
                else_branch: Some(Box::new(Expr::ElseBranch(vec![])))
            })])
        );
    }

    #[test]
    fn if_else_if_parsing() {
        assert_eq!(
            parser::statements("if 1 {} else if (1) {}"),
            Ok(vec![Stmt::Expr(Expr::IfBranch {
                condition: Box::new(Expr::IntLiteral("1".into())),
                body: vec![],
                else_branch: Some(Box::new(Expr::IfBranch {
                    condition: Box::new(Expr::IntLiteral("1".into())),
                    body: vec![],
                    else_branch: None
                }))
            })])
        );
    }
}
