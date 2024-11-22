use regex::Regex;

use crate::parser;

#[derive(Debug)]
pub enum Token<'a> {
    Int,
    Void,
    Return,
    Identifier(parser::Identifier<'a>),
    Constant(parser::Constant<'a>),
    OParen,
    CParen,
    OBrace,
    CBrace,
    Semicolon,
    Minus,
    DoubleMinus,
    Tilde,
}

pub fn lex(source: &String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut current = &source[..];
    current = current.trim_start();
    let re =
        Regex::new("\\A(?:(?<int>int\\b)|(?<void>void\\b)|(?<return>return\\b)|(?<identifier>[a-zA-Z_]\\w*\\b)|(?<constant>[0-9]+\\b)|(?<o_paren>\\()|(?<c_paren>\\))|(?<o_brace>\\{)|(?<c_brace>\\})|(?<semicolon>;)|(?<minus>-)|(?<double_minus>--)|(?<tilde>~))").unwrap();
    while current.len() != 0 {
        let ca = re.captures(current);

        if let Some(ca) = ca {
            let (token, end) = if let Some(m) = ca.name("int") {
                (Token::Int, m.end())
            } else if let Some(m) = ca.name("void") {
                (Token::Void, m.end())
            } else if let Some(m) = ca.name("return") {
                (Token::Return, m.end())
            } else if let Some(m) = ca.name("identifier") {
                (Token::Identifier(parser::Identifier(m.as_str())), m.end())
            } else if let Some(m) = ca.name("constant") {
                (Token::Constant(parser::Constant(m.as_str())), m.end())
            } else if let Some(m) = ca.name("o_paren") {
                (Token::OParen, m.end())
            } else if let Some(m) = ca.name("c_paren") {
                (Token::CParen, m.end())
            } else if let Some(m) = ca.name("o_brace") {
                (Token::OBrace, m.end())
            } else if let Some(m) = ca.name("c_brace") {
                (Token::CBrace, m.end())
            } else if let Some(m) = ca.name("semicolon") {
                (Token::Semicolon, m.end())
            } else if let Some(m) = ca.name("minus") {
                (Token::Minus, m.end())
            } else if let Some(m) = ca.name("double_minus") {
                (Token::DoubleMinus, m.end())
            } else if let Some(m) = ca.name("tilde") {
                (Token::Tilde, m.end())
            } else {
                panic!()
            };

            tokens.push(token);

            current = &current[end..];
            current = current.trim_start();
        } else {
            panic!();
        }
    }

    tokens
}
