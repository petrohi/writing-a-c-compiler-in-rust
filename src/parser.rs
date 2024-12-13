use std::collections::HashMap;

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub struct Identifier<'a>(pub &'a str);
#[derive(Debug, Clone)]
pub struct Constant<'a>(pub &'a str);

#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Negate,
    Complement,
    Not,
}

#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

pub struct Context<'a> {
    validate: bool,
    vars: HashMap<&'a str, usize>,
}

impl<'a> Context<'a> {
    pub fn new(validate: bool) -> Context<'a> {
        Context {
            validate,
            vars: HashMap::new(),
        }
    }

    fn declare_var(self: &mut Self, identifier: &Identifier<'a>) -> usize {
        if self.validate {
            if self.vars.contains_key(identifier.0) {
                panic!("Variable is already declared");
            }

            let var = self.vars.len();
            self.vars.insert(identifier.0, var);
            var
        } else {
            0
        }
    }

    fn get_var(self: &Self, identifier: &Identifier<'a>) -> Expression<'a> {
        if self.validate {
            if let Some(var) = self.vars.get(identifier.0) {
                Expression::Var(*var)
            } else {
                panic!("Undeclared variable")
            }
        } else {
            Expression::Var(0)
        }
    }

    fn is_valid_lvalue(self: &Self, lvalue: &Expression) -> bool {
        if self.validate {
            if let Expression::Var(_) = lvalue {
                true
            } else {
                false
            }
        } else {
            true
        }
    }
}
#[derive(Debug)]
pub struct Declaration<'a> {
    pub var: usize,
    pub expression: Option<Expression<'a>>,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Constant(Constant<'a>),
    Var(usize),
    Unary {
        operator: UnaryOperator,
        expression: Box<Expression<'a>>,
    },
    Binary {
        operator: BinaryOperator,
        left: Box<Expression<'a>>,
        right: Box<Expression<'a>>,
    },
    Assignment {
        lvalue: Box<Expression<'a>>,
        rvalue: Box<Expression<'a>>,
    },
    Conditional {
        condition: Box<Expression<'a>>,
        then: Box<Expression<'a>>,
        els: Box<Expression<'a>>,
    },
}
#[derive(Debug)]
pub enum Statement<'a> {
    Return(Expression<'a>),
    Expression(Expression<'a>),
    Null,
    If {
        condition: Expression<'a>,
        then: Box<Statement<'a>>,
        els: Option<Box<Statement<'a>>>,
    },
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Statement(Statement<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Identifier<'a>,
    pub body: Vec<BlockItem<'a>>,
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
        Token::Exclamation => UnaryOperator::Not,
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
        Token::LessThan => BinaryOperator::LessThan,
        Token::LessThanEqual => BinaryOperator::LessThanOrEqual,
        Token::GreaterThan => BinaryOperator::GreaterThan,
        Token::GreaterThanEqual => BinaryOperator::GreaterThanOrEqual,
        Token::DoubleEqual => BinaryOperator::Equal,
        Token::ExclamationEqual => BinaryOperator::NotEqual,
        Token::DoubleAmpersand => BinaryOperator::And,
        Token::DoublePipe => BinaryOperator::Or,
        _ => panic!("Unexpected token"),
    }
}

fn parse_factor<'a>(tokens: &mut Vec<Token<'a>>, context: &mut Context<'a>) -> Expression<'a> {
    match peek_token(tokens) {
        Token::Constant(_) => {
            if let Token::Constant(constant) = pop_token(tokens) {
                Expression::Constant(constant)
            } else {
                panic!()
            }
        }
        Token::Identifier(_) => {
            if let Token::Identifier(identifier) = pop_token(tokens) {
                context.get_var(&identifier)
            } else {
                panic!()
            }
        }
        Token::OParen => {
            if let Token::OParen = pop_token(tokens) {
                let expression = parse_expression(tokens, 0, context);
                if let Token::CParen = pop_token(tokens) {
                    expression
                } else {
                    panic!("Expected )");
                }
            } else {
                panic!()
            }
        }
        Token::Minus | Token::Tilde | Token::Exclamation => Expression::Unary {
            operator: parse_unary(tokens),
            expression: Box::new(parse_factor(tokens, context)),
        },
        _ => panic!("Unexpected token"),
    }
}

fn precedence(token: &Token) -> Option<usize> {
    match token {
        Token::Plus | Token::Minus => Some(45),
        Token::Star | Token::Slash | Token::Percent => Some(50),
        Token::LessThan | Token::LessThanEqual | Token::GreaterThan | Token::GreaterThanEqual => {
            Some(35)
        }
        Token::DoubleEqual | Token::ExclamationEqual => Some(30),
        Token::DoubleAmpersand => Some(10),
        Token::DoublePipe => Some(5),
        Token::Question => Some(3),
        Token::Equal => Some(1),
        _ => None,
    }
}

fn parse_expression<'a>(
    tokens: &mut Vec<Token<'a>>,
    min_precedence: usize,
    context: &mut Context<'a>,
) -> Expression<'a> {
    let mut left = parse_factor(tokens, context);
    let mut next_token = peek_token(tokens);
    let mut next_precedence = precedence(next_token);
    while next_precedence.is_some() && next_precedence.unwrap() >= min_precedence {
        if let Token::Equal = next_token {
            _ = pop_token(tokens);
            let right = parse_expression(tokens, next_precedence.unwrap(), context);

            if context.is_valid_lvalue(&left) {
                left = Expression::Assignment {
                    lvalue: Box::new(left),
                    rvalue: Box::new(right),
                };
            } else {
                panic!("Invalid lvalue in assignment");
            }
        } else if let Token::Question = next_token {
            _ = pop_token(tokens);
            let then = parse_expression(tokens, 0, context);

            if let Token::Colon = pop_token(tokens) {
                let els = parse_expression(tokens, next_precedence.unwrap(), context);
                left = Expression::Conditional {
                    condition: Box::new(left),
                    then: Box::new(then),
                    els: Box::new(els),
                };
            } else {
                panic!("Expected :");
            }
        } else {
            let operator = parse_binary(tokens);
            let right = parse_expression(tokens, next_precedence.unwrap() + 1, context);
            left = Expression::Binary {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        next_token = peek_token(tokens);
        next_precedence = precedence(next_token);
    }
    left
}

fn parse_statement<'a>(tokens: &mut Vec<Token<'a>>, context: &mut Context<'a>) -> Statement<'a> {
    if let Token::Return = peek_token(tokens) {
        _ = pop_token(tokens);
        let expression = parse_expression(tokens, 0, context);
        if let Token::Semicolon = pop_token(tokens) {
            Statement::Return(expression)
        } else {
            panic!("Expected ;");
        }
    } else if let Token::If = peek_token(tokens) {
        _ = pop_token(tokens);

        if let Token::OParen = pop_token(tokens) {
            let condition = parse_expression(tokens, 0, context);

            if let Token::CParen = pop_token(tokens) {
                let then = parse_statement(tokens, context);

                let els = if let Token::Else = peek_token(tokens) {
                    _ = pop_token(tokens);
                    Some(Box::new(parse_statement(tokens, context)))
                } else {
                    None
                };

                Statement::If {
                    condition,
                    then: Box::new(then),
                    els,
                }
            } else {
                panic!("Expected )");
            }
        } else {
            panic!("Expected (");
        }
    } else if let Token::Semicolon = peek_token(tokens) {
        _ = pop_token(tokens);
        Statement::Null
    } else {
        let expression = parse_expression(tokens, 0, context);

        if let Token::Semicolon = pop_token(tokens) {
            Statement::Expression(expression)
        } else {
            panic!("Expected ;");
        }
    }
}

fn parse_declaration<'a>(
    tokens: &mut Vec<Token<'a>>,
    context: &mut Context<'a>,
) -> Declaration<'a> {
    if let Token::Int = pop_token(tokens) {
        if let Token::Identifier(identifier) = pop_token(tokens) {
            let var = context.declare_var(&identifier);

            let expression = match pop_token(tokens) {
                Token::Equal => {
                    let expression = parse_expression(tokens, 0, context);
                    if let Token::Semicolon = pop_token(tokens) {
                        Some(expression)
                    } else {
                        panic!("Expected ;");
                    }
                }
                Token::Semicolon => None,
                _ => panic!("Expected = or ;"),
            };

            Declaration { var, expression }
        } else {
            panic!("Expected <identifier>");
        }
    } else {
        panic!("Expected int")
    }
}

fn parse_block_item<'a>(tokens: &mut Vec<Token<'a>>, context: &mut Context<'a>) -> BlockItem<'a> {
    if let Token::Int = peek_token(tokens) {
        BlockItem::Declaration(parse_declaration(tokens, context))
    } else {
        BlockItem::Statement(parse_statement(tokens, context))
    }
}

fn parse_function<'a>(tokens: &mut Vec<Token<'a>>, context: &mut Context<'a>) -> Function<'a> {
    if let Token::Int = pop_token(tokens) {
        if let Token::Identifier(identifier) = pop_token(tokens) {
            if let Token::OParen = pop_token(tokens) {
                if let Token::Void = pop_token(tokens) {
                    if let Token::CParen = pop_token(tokens) {
                        if let Token::OBrace = pop_token(tokens) {
                            let mut block_items = Vec::new();
                            loop {
                                if let Token::CBrace = peek_token(tokens) {
                                    _ = pop_token(tokens);
                                    break;
                                } else {
                                    block_items.push(parse_block_item(tokens, context));
                                }
                            }

                            Function {
                                body: block_items,
                                name: identifier,
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
            panic!("Expected <identifier>");
        }
    } else {
        panic!("Expected int");
    }
}

pub fn parse_program<'a>(tokens: &mut Vec<Token<'a>>, context: &mut Context<'a>) -> Program<'a> {
    let program = Program(parse_function(tokens, context));

    if tokens.is_empty() {
        program
    } else {
        panic!("Unexpected tokens at the end")
    }
}
