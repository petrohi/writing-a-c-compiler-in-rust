use clap::Parser;
use regex::Regex;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::process::Command;
use tempfile::tempdir;

#[derive(Debug, Parser)]
struct Cli {
    path: String,
    #[clap(long, action)]
    lex: bool,
    #[clap(long, action)]
    parse: bool,
    #[clap(long, action)]
    codegen: bool,
}

#[derive(Debug)]
struct Identifier<'a>(&'a str);
#[derive(Debug)]
struct Constant<'a>(&'a str);

#[derive(Debug)]
enum Token<'a> {
    Int,
    Void,
    Return,
    Identifier(Identifier<'a>),
    Constant(Constant<'a>),
    OParen,
    CParen,
    OBrace,
    CBrace,
    Semicolon,
}

#[derive(Debug)]
enum Expression<'a> {
    Constant(Constant<'a>),
}
#[derive(Debug)]
enum Statement<'a> {
    Return(Expression<'a>),
}

#[derive(Debug)]
struct Function<'a> {
    name: Identifier<'a>,
    body: Statement<'a>,
}
#[derive(Debug)]
struct Program<'a>(Function<'a>);

fn lex(source: &String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut current = &source[..];
    current = current.trim_start();
    let re =
        Regex::new("\\A(?:(?<int>int\\b)|(?<void>void\\b)|(?<return>return\\b)|(?<identifier>[a-zA-Z_]\\w*\\b)|(?<constant>[0-9]+\\b)|(?<oparen>\\()|(?<cparen>\\))|(?<obrace>\\{)|(?<cbrace>\\})|(?<semicolon>;))").unwrap();
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
                (Token::Identifier(Identifier(m.as_str())), m.end())
            } else if let Some(m) = ca.name("constant") {
                (Token::Constant(Constant(m.as_str())), m.end())
            } else if let Some(m) = ca.name("oparen") {
                (Token::OParen, m.end())
            } else if let Some(m) = ca.name("cparen") {
                (Token::CParen, m.end())
            } else if let Some(m) = ca.name("obrace") {
                (Token::OBrace, m.end())
            } else if let Some(m) = ca.name("cbrace") {
                (Token::CBrace, m.end())
            } else if let Some(m) = ca.name("semicolon") {
                (Token::Semicolon, m.end())
            } else {
                panic!()
            };

            dbg!(&token);
            tokens.push(token);

            current = &current[end..];
            current = current.trim_start();
        } else {
            panic!();
        }
    }

    tokens
}

fn parse_expression<'a>(tokens: &mut Vec<Token<'a>>) -> Expression<'a> {
    if let Some(Token::Constant(constant)) = tokens.pop() {
        Expression::Constant(constant)
    } else {
        panic!("Expected constant");
    }
}

fn parse_statement<'a>(tokens: &mut Vec<Token<'a>>) -> Statement<'a> {
    if let Some(Token::Return) = tokens.pop() {
        let expression = parse_expression(tokens);
        if let Some(Token::Semicolon) = tokens.pop() {
            Statement::Return(expression)
        } else {
            panic!("Expected ;");
        }
    } else {
        panic!("Expected return");
    }
}

fn parse_function<'a>(tokens: &mut Vec<Token<'a>>) -> Function<'a> {
    if let Some(Token::Int) = tokens.pop() {
        if let Some(Token::Identifier(identifier)) = tokens.pop() {
            if let Some(Token::OParen) = tokens.pop() {
                if let Some(Token::Void) = tokens.pop() {
                    if let Some(Token::CParen) = tokens.pop() {
                        if let Some(Token::OBrace) = tokens.pop() {
                            let function = Function {
                                body: parse_statement(tokens),
                                name: identifier,
                            };

                            if let Some(Token::CBrace) = tokens.pop() {
                                function
                            } else {
                                panic!("Expected }}");
                            }
                        } else {
                            panic!("Expected {{");
                        }
                    } else {
                        panic!("Expected )");
                    }
                } else {
                    panic!("Expected void");
                }
            } else {
                panic!("Expected (");
            }
        } else {
            panic!("Expected identifier");
        }
    } else {
        panic!("Expected int");
    }
}

fn parse_program<'a>(tokens: &mut Vec<Token<'a>>) -> Program<'a> {
    let program = Program(parse_function(tokens));

    if tokens.is_empty() {
        program
    } else {
        panic!("Unexpected tokens at the end")
    }
}

fn main() {
    let args = Cli::parse();
    let source_path = PathBuf::from(args.path);

    if let Ok(temp_dir) = tempdir() {
        let preprocessed_path = temp_dir.path().join(source_path.file_name().unwrap());

        if let Ok(status) = Command::new("gcc")
            .args([
                "-E",
                "-P",
                source_path.to_str().unwrap(),
                "-o",
                preprocessed_path.to_str().unwrap(),
            ])
            .status()
        {
            if status.success() {
                match read_to_string(preprocessed_path) {
                    Ok(source) => {
                        let mut tokens = lex(&source);
                        tokens.reverse();
                        let program = parse_program(&mut tokens);
                        dbg!(program);
                    }
                    Err(_) => panic!("Cannot read file"),
                }
            } else {
                panic!("Preprocessing failed")
            }
        } else {
            panic!("GCC not found")
        }

        temp_dir.close().unwrap();
    } else {
        panic!("Error creating temp dir")
    }
}
