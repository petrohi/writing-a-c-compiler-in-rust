use regex::{Match, Regex};

use crate::parser;

#[derive(Debug)]
pub enum Token<'a> {
    Int,
    Void,
    Return,
    If,
    Else,
    Do,
    While,
    For,
    Break,
    Continue,
    Identifier(parser::Identifier<'a>),
    Constant(parser::Constant<'a>),
    OParen,
    CParen,
    OBrace,
    CBrace,
    Semicolon,
    DoubleMinus,
    Minus,
    Tilde,
    Plus,
    Star,
    Slash,
    Percent,
    LessThanEqual,
    LessThan,
    GreaterThanEqual,
    GreaterThan,
    DoubleEqual,
    Equal,
    ExclamationEqual,
    Exclamation,
    DoubleAmpersand,
    DoublePipe,
    Question,
    Colon,
}

struct RegexTokenizer<'a, 'b> {
    regex: &'a str,
    match_to_token: fn(Match<'b>) -> Token<'b>,
}

pub fn lex(source: &String) -> Vec<Token> {
    let mut regex_tokenizers: Vec<(&str, RegexTokenizer)> = Vec::new();

    regex_tokenizers.push((
        "int",
        RegexTokenizer {
            regex: "(?<int>int\\b)",
            match_to_token: |_| Token::Int,
        },
    ));

    regex_tokenizers.push((
        "void",
        RegexTokenizer {
            regex: "(?<void>void\\b)",
            match_to_token: |_| Token::Void,
        },
    ));

    regex_tokenizers.push((
        "return",
        RegexTokenizer {
            regex: "(?<return>return\\b)",
            match_to_token: |_| Token::Return,
        },
    ));

    regex_tokenizers.push((
        "if",
        RegexTokenizer {
            regex: "(?<if>if\\b)",
            match_to_token: |_| Token::If,
        },
    ));

    regex_tokenizers.push((
        "else",
        RegexTokenizer {
            regex: "(?<else>else\\b)",
            match_to_token: |_| Token::Else,
        },
    ));

    regex_tokenizers.push((
        "do",
        RegexTokenizer {
            regex: "(?<do>do\\b)",
            match_to_token: |_| Token::Do,
        },
    ));

    regex_tokenizers.push((
        "while",
        RegexTokenizer {
            regex: "(?<while>while\\b)",
            match_to_token: |_| Token::While,
        },
    ));

    regex_tokenizers.push((
        "for",
        RegexTokenizer {
            regex: "(?<for>for\\b)",
            match_to_token: |_| Token::For,
        },
    ));

    regex_tokenizers.push((
        "break",
        RegexTokenizer {
            regex: "(?<break>break\\b)",
            match_to_token: |_| Token::Break,
        },
    ));

    regex_tokenizers.push((
        "continue",
        RegexTokenizer {
            regex: "(?<continue>continue\\b)",
            match_to_token: |_| Token::Continue,
        },
    ));

    regex_tokenizers.push((
        "identifier",
        RegexTokenizer {
            regex: "(?<identifier>[a-zA-Z_]\\w*\\b)",
            match_to_token: |m| Token::Identifier(parser::Identifier(m.as_str())),
        },
    ));

    regex_tokenizers.push((
        "constant",
        RegexTokenizer {
            regex: "(?<constant>[0-9]+\\b)",
            match_to_token: |m| Token::Constant(parser::Constant(m.as_str())),
        },
    ));

    regex_tokenizers.push((
        "o_paren",
        RegexTokenizer {
            regex: "(?<o_paren>\\()",
            match_to_token: |_| Token::OParen,
        },
    ));

    regex_tokenizers.push((
        "c_paren",
        RegexTokenizer {
            regex: "(?<c_paren>\\))",
            match_to_token: |_| Token::CParen,
        },
    ));

    regex_tokenizers.push((
        "o_brace",
        RegexTokenizer {
            regex: "(?<o_brace>\\{)",
            match_to_token: |_| Token::OBrace,
        },
    ));

    regex_tokenizers.push((
        "c_brace",
        RegexTokenizer {
            regex: "(?<c_brace>\\})",
            match_to_token: |_| Token::CBrace,
        },
    ));

    regex_tokenizers.push((
        "semicolon",
        RegexTokenizer {
            regex: "(?<semicolon>;)",
            match_to_token: |_| Token::Semicolon,
        },
    ));

    regex_tokenizers.push((
        "double_minus",
        RegexTokenizer {
            regex: "(?<double_minus>--)",
            match_to_token: |_| Token::DoubleMinus,
        },
    ));

    regex_tokenizers.push((
        "minus",
        RegexTokenizer {
            regex: "(?<minus>-)",
            match_to_token: |_| Token::Minus,
        },
    ));

    regex_tokenizers.push((
        "tilde",
        RegexTokenizer {
            regex: "(?<tilde>~)",
            match_to_token: |_| Token::Tilde,
        },
    ));

    regex_tokenizers.push((
        "plus",
        RegexTokenizer {
            regex: "(?<plus>\\+)",
            match_to_token: |_| Token::Plus,
        },
    ));

    regex_tokenizers.push((
        "star",
        RegexTokenizer {
            regex: "(?<star>\\*)",
            match_to_token: |_| Token::Star,
        },
    ));

    regex_tokenizers.push((
        "slash",
        RegexTokenizer {
            regex: "(?<slash>/)",
            match_to_token: |_| Token::Slash,
        },
    ));

    regex_tokenizers.push((
        "percent",
        RegexTokenizer {
            regex: "(?<percent>%)",
            match_to_token: |_| Token::Percent,
        },
    ));

    regex_tokenizers.push((
        "less_than_equal",
        RegexTokenizer {
            regex: "(?<less_than_equal><=)",
            match_to_token: |_| Token::LessThanEqual,
        },
    ));

    regex_tokenizers.push((
        "less_than",
        RegexTokenizer {
            regex: "(?<less_than><)",
            match_to_token: |_| Token::LessThan,
        },
    ));

    regex_tokenizers.push((
        "greater_than_equal",
        RegexTokenizer {
            regex: "(?<greater_than_equal>>=)",
            match_to_token: |_| Token::GreaterThanEqual,
        },
    ));

    regex_tokenizers.push((
        "greater_than",
        RegexTokenizer {
            regex: "(?<greater_than>>)",
            match_to_token: |_| Token::GreaterThan,
        },
    ));

    regex_tokenizers.push((
        "double_equal",
        RegexTokenizer {
            regex: "(?<double_equal>==)",
            match_to_token: |_| Token::DoubleEqual,
        },
    ));

    regex_tokenizers.push((
        "equal",
        RegexTokenizer {
            regex: "(?<equal>=)",
            match_to_token: |_| Token::Equal,
        },
    ));

    regex_tokenizers.push((
        "exclamation_equal",
        RegexTokenizer {
            regex: "(?<exclamation_equal>!=)",
            match_to_token: |_| Token::ExclamationEqual,
        },
    ));

    regex_tokenizers.push((
        "exclamation",
        RegexTokenizer {
            regex: "(?<exclamation>!)",
            match_to_token: |_| Token::Exclamation,
        },
    ));

    regex_tokenizers.push((
        "double_ampersand",
        RegexTokenizer {
            regex: "(?<double_ampersand>&&)",
            match_to_token: |_| Token::DoubleAmpersand,
        },
    ));

    regex_tokenizers.push((
        "double_pipe",
        RegexTokenizer {
            regex: "(?<double_pipe>\\|\\|)",
            match_to_token: |_| Token::DoublePipe,
        },
    ));

    regex_tokenizers.push((
        "question",
        RegexTokenizer {
            regex: "(?<question>\\?)",
            match_to_token: |_| Token::Question,
        },
    ));

    regex_tokenizers.push((
        "colon",
        RegexTokenizer {
            regex: "(?<colon>:)",
            match_to_token: |_| Token::Colon,
        },
    ));

    let mut tokens = Vec::new();
    let mut current = &source[..];
    current = current.trim_start();

    let re = Regex::new(&format!(
        "\\A(?:{})",
        regex_tokenizers
            .iter()
            .map(|(_, rt)| rt.regex)
            .collect::<Vec<_>>()
            .join("|")
    ))
    .unwrap();

    while current.len() != 0 {
        let ca = re.captures(current);

        if let Some(ca) = ca {
            for (name, rt) in regex_tokenizers.iter() {
                if let Some(m) = ca.name(name) {
                    tokens.push((rt.match_to_token)(m));
                    current = &current[m.end()..];
                    current = current.trim_start();

                    break;
                }
            }
        } else {
            println!("{}", current);
            panic!();
        }
    }

    tokens
}
