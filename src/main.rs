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
enum Token<'a> {
    Int,
    Void,
    Return,
    Identifier(&'a str),
    Constant(&'a str),
    OParen,
    CParen,
    OBrace,
    CBrace,
    Semicolon,
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
                                    (Token::Identifier(m.as_str()), m.end())
                                } else if let Some(m) = ca.name("constant") {
                                    (Token::Constant(m.as_str()), m.end())
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

                                current = &current[end..];
                                current = current.trim_start();
                            } else {
                                panic!();
                            }
                        }
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
