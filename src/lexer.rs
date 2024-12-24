use regex::{Match, Regex};

#[derive(Debug, Clone)]
pub struct Identifier<'a>(pub &'a str);
#[derive(Debug, Clone)]
pub struct Constant<'a>(pub &'a str);

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
    Static,
    Extern,
    Identifier(Identifier<'a>),
    Constant(Constant<'a>),
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
    Comma,
}

struct RegexTokenizer<'a, 'b> {
    regex: &'a str,
    match_to_token: fn(Match<'b>) -> Token<'b>,
}

pub fn lex(source: &str) -> Vec<Token> {
    let regex_tokenizers: Vec<(&str, RegexTokenizer)> = vec![
        (
            "int",
            RegexTokenizer {
                regex: "(?<int>int\\b)",
                match_to_token: |_| Token::Int,
            },
        ),
        (
            "void",
            RegexTokenizer {
                regex: "(?<void>void\\b)",
                match_to_token: |_| Token::Void,
            },
        ),
        (
            "return",
            RegexTokenizer {
                regex: "(?<return>return\\b)",
                match_to_token: |_| Token::Return,
            },
        ),
        (
            "if",
            RegexTokenizer {
                regex: "(?<if>if\\b)",
                match_to_token: |_| Token::If,
            },
        ),
        (
            "else",
            RegexTokenizer {
                regex: "(?<else>else\\b)",
                match_to_token: |_| Token::Else,
            },
        ),
        (
            "do",
            RegexTokenizer {
                regex: "(?<do>do\\b)",
                match_to_token: |_| Token::Do,
            },
        ),
        (
            "while",
            RegexTokenizer {
                regex: "(?<while>while\\b)",
                match_to_token: |_| Token::While,
            },
        ),
        (
            "for",
            RegexTokenizer {
                regex: "(?<for>for\\b)",
                match_to_token: |_| Token::For,
            },
        ),
        (
            "break",
            RegexTokenizer {
                regex: "(?<break>break\\b)",
                match_to_token: |_| Token::Break,
            },
        ),
        (
            "continue",
            RegexTokenizer {
                regex: "(?<continue>continue\\b)",
                match_to_token: |_| Token::Continue,
            },
        ),
        (
            "static",
            RegexTokenizer {
                regex: "(?<static>static\\b)",
                match_to_token: |_| Token::Static,
            },
        ),
        (
            "extern",
            RegexTokenizer {
                regex: "(?<extern>extern\\b)",
                match_to_token: |_| Token::Extern,
            },
        ),
        (
            "identifier",
            RegexTokenizer {
                regex: "(?<identifier>[a-zA-Z_]\\w*\\b)",
                match_to_token: |m| Token::Identifier(Identifier(m.as_str())),
            },
        ),
        (
            "constant",
            RegexTokenizer {
                regex: "(?<constant>[0-9]+\\b)",
                match_to_token: |m| Token::Constant(Constant(m.as_str())),
            },
        ),
        (
            "o_paren",
            RegexTokenizer {
                regex: "(?<o_paren>\\()",
                match_to_token: |_| Token::OParen,
            },
        ),
        (
            "c_paren",
            RegexTokenizer {
                regex: "(?<c_paren>\\))",
                match_to_token: |_| Token::CParen,
            },
        ),
        (
            "o_brace",
            RegexTokenizer {
                regex: "(?<o_brace>\\{)",
                match_to_token: |_| Token::OBrace,
            },
        ),
        (
            "c_brace",
            RegexTokenizer {
                regex: "(?<c_brace>\\})",
                match_to_token: |_| Token::CBrace,
            },
        ),
        (
            "semicolon",
            RegexTokenizer {
                regex: "(?<semicolon>;)",
                match_to_token: |_| Token::Semicolon,
            },
        ),
        (
            "double_minus",
            RegexTokenizer {
                regex: "(?<double_minus>--)",
                match_to_token: |_| Token::DoubleMinus,
            },
        ),
        (
            "minus",
            RegexTokenizer {
                regex: "(?<minus>-)",
                match_to_token: |_| Token::Minus,
            },
        ),
        (
            "tilde",
            RegexTokenizer {
                regex: "(?<tilde>~)",
                match_to_token: |_| Token::Tilde,
            },
        ),
        (
            "plus",
            RegexTokenizer {
                regex: "(?<plus>\\+)",
                match_to_token: |_| Token::Plus,
            },
        ),
        (
            "star",
            RegexTokenizer {
                regex: "(?<star>\\*)",
                match_to_token: |_| Token::Star,
            },
        ),
        (
            "slash",
            RegexTokenizer {
                regex: "(?<slash>/)",
                match_to_token: |_| Token::Slash,
            },
        ),
        (
            "percent",
            RegexTokenizer {
                regex: "(?<percent>%)",
                match_to_token: |_| Token::Percent,
            },
        ),
        (
            "less_than_equal",
            RegexTokenizer {
                regex: "(?<less_than_equal><=)",
                match_to_token: |_| Token::LessThanEqual,
            },
        ),
        (
            "less_than",
            RegexTokenizer {
                regex: "(?<less_than><)",
                match_to_token: |_| Token::LessThan,
            },
        ),
        (
            "greater_than_equal",
            RegexTokenizer {
                regex: "(?<greater_than_equal>>=)",
                match_to_token: |_| Token::GreaterThanEqual,
            },
        ),
        (
            "greater_than",
            RegexTokenizer {
                regex: "(?<greater_than>>)",
                match_to_token: |_| Token::GreaterThan,
            },
        ),
        (
            "double_equal",
            RegexTokenizer {
                regex: "(?<double_equal>==)",
                match_to_token: |_| Token::DoubleEqual,
            },
        ),
        (
            "equal",
            RegexTokenizer {
                regex: "(?<equal>=)",
                match_to_token: |_| Token::Equal,
            },
        ),
        (
            "exclamation_equal",
            RegexTokenizer {
                regex: "(?<exclamation_equal>!=)",
                match_to_token: |_| Token::ExclamationEqual,
            },
        ),
        (
            "exclamation",
            RegexTokenizer {
                regex: "(?<exclamation>!)",
                match_to_token: |_| Token::Exclamation,
            },
        ),
        (
            "double_ampersand",
            RegexTokenizer {
                regex: "(?<double_ampersand>&&)",
                match_to_token: |_| Token::DoubleAmpersand,
            },
        ),
        (
            "double_pipe",
            RegexTokenizer {
                regex: "(?<double_pipe>\\|\\|)",
                match_to_token: |_| Token::DoublePipe,
            },
        ),
        (
            "question",
            RegexTokenizer {
                regex: "(?<question>\\?)",
                match_to_token: |_| Token::Question,
            },
        ),
        (
            "colon",
            RegexTokenizer {
                regex: "(?<colon>:)",
                match_to_token: |_| Token::Colon,
            },
        ),
        (
            "comma",
            RegexTokenizer {
                regex: "(?<comma>,)",
                match_to_token: |_| Token::Comma,
            },
        ),
    ];

    let mut tokens = Vec::new();
    let mut current = source;
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

    while !current.is_empty() {
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
