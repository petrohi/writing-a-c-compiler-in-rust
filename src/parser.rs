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
#[derive(Debug)]
pub enum Expression<'a> {
    Constant(Constant<'a>),
    Unary {
        operator: UnaryOperator,
        expression: Box<Expression<'a>>,
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

fn parse_expression<'a>(tokens: &mut Vec<Token<'a>>) -> Expression<'a> {
    let token = tokens.pop();
    if let Some(Token::Constant(constant)) = token {
        Expression::Constant(constant)
    } else if let Some(Token::OParen) = token {
        let expression = parse_expression(tokens);
        if let Some(Token::CParen) = tokens.pop() {
            expression
        } else {
            panic!("Expected )");
        }
    } else if let Some(Token::Minus) = token {
        Expression::Unary {
            operator: UnaryOperator::Negate,
            expression: Box::new(parse_expression(tokens)),
        }
    } else if let Some(Token::Tilde) = token {
        Expression::Unary {
            operator: UnaryOperator::Complement,
            expression: Box::new(parse_expression(tokens)),
        }
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

pub fn parse_program<'a>(tokens: &mut Vec<Token<'a>>) -> Program<'a> {
    let program = Program(parse_function(tokens));

    if tokens.is_empty() {
        program
    } else {
        panic!("Unexpected tokens at the end")
    }
}
