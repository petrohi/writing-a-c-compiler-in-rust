use crate::lexer::Token;

#[derive(Debug, Clone)]
pub struct Identifier<'a>(pub &'a str);
#[derive(Debug, Clone)]
pub struct Constant<'a>(pub &'a str);

#[derive(Debug)]
pub enum UnaryOperator {
    Negate,
    Complement,
}

#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Constant(Constant<'a>),
    Unary {
        operator: UnaryOperator,
        expression: Box<Expression<'a>>,
    },
    Binary {
        operator: BinaryOperator,
        left: Box<Expression<'a>>,
        right: Box<Expression<'a>>,
    },
}
#[derive(Debug)]
pub enum Statement<'a> {
    Return(Expression<'a>),
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Identifier<'a>,
    pub body: Statement<'a>,
}
#[derive(Debug)]
pub struct Program<'a>(pub Function<'a>);

fn peek_token<'a, 'b>(tokens: &'b Vec<Token<'a>>) -> &'b Token<'a> {
    match tokens.last() {
        Some(token) => token,
        None => panic!("Expected token"),
    }
}

fn pop_token<'a>(tokens: &mut Vec<Token<'a>>) -> Token<'a> {
    match tokens.pop() {
        Some(token) => token,
        None => panic!("Expected token"),
    }
}

fn parse_unary(tokens: &mut Vec<Token>) -> UnaryOperator {
    match pop_token(tokens) {
        Token::Minus => UnaryOperator::Negate,
        Token::Tilde => UnaryOperator::Complement,
        _ => panic!("Unexpected token"),
    }
}

fn parse_binary(tokens: &mut Vec<Token>) -> BinaryOperator {
    match pop_token(tokens) {
        Token::Plus => BinaryOperator::Add,
        Token::Minus => BinaryOperator::Sub,
        Token::Star => BinaryOperator::Mul,
        Token::Slash => BinaryOperator::Div,
        Token::Percent => BinaryOperator::Rem,
        _ => panic!("Unexpected token"),
    }
}

fn parse_factor<'a>(tokens: &mut Vec<Token<'a>>) -> Expression<'a> {
    match peek_token(tokens) {
        Token::Constant(_) => {
            if let Token::Constant(constant) = pop_token(tokens) {
                Expression::Constant(constant)
            } else {
                panic!()
            }
        }
        Token::OParen => {
            if let Token::OParen = pop_token(tokens) {
                let expression = parse_expression(tokens, 0);
                if let Token::CParen = pop_token(tokens) {
                    expression
                } else {
                    panic!("Expected )");
                }
            } else {
                panic!()
            }
        }
        Token::Minus | Token::Tilde => Expression::Unary {
            operator: parse_unary(tokens),
            expression: Box::new(parse_factor(tokens)),
        },
        _ => panic!("Unexpected token"),
    }
}

fn is_binary(token: &Token) -> bool {
    match token {
        Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Percent => true,
        _ => false,
    }
}

fn precedence(token: &Token) -> usize {
    match token {
        Token::Plus | Token::Minus => 45,
        Token::Star | Token::Slash | Token::Percent => 50,
        _ => panic!(),
    }
}

fn parse_expression<'a>(tokens: &mut Vec<Token<'a>>, min_precedence: usize) -> Expression<'a> {
    let mut left = parse_factor(tokens);
    let mut next_token = peek_token(tokens);
    while is_binary(next_token) && precedence(next_token) >= min_precedence {
        let precedence = precedence(next_token);
        let operator = parse_binary(tokens);
        let right = parse_expression(tokens, precedence + 1);
        left = Expression::Binary {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        };
        next_token = peek_token(tokens);
    }
    left
}

fn parse_statement<'a>(tokens: &mut Vec<Token<'a>>) -> Statement<'a> {
    if let Token::Return = pop_token(tokens) {
        let expression = parse_expression(tokens, 0);
        if let Token::Semicolon = pop_token(tokens) {
            Statement::Return(expression)
        } else {
            panic!("Expected ;");
        }
    } else {
        panic!("Expected return");
    }
}

fn parse_function<'a>(tokens: &mut Vec<Token<'a>>) -> Function<'a> {
    if let Token::Int = pop_token(tokens) {
        if let Token::Identifier(identifier) = pop_token(tokens) {
            if let Token::OParen = pop_token(tokens) {
                if let Token::Void = pop_token(tokens) {
                    if let Token::CParen = pop_token(tokens) {
                        if let Token::OBrace = pop_token(tokens) {
                            let function = Function {
                                body: parse_statement(tokens),
                                name: identifier,
                            };

                            if let Token::CBrace = pop_token(tokens) {
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

pub fn parse_program<'a>(tokens: &mut Vec<Token<'a>>) -> Program<'a> {
    let program = Program(parse_function(tokens));

    if tokens.is_empty() {
        program
    } else {
        panic!("Unexpected tokens at the end")
    }
}
