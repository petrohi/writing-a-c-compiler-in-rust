use std::collections::HashMap;

use crate::lexer;

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
    last_var_index: usize,
    var_scopes: Vec<HashMap<&'a str, usize>>,
    last_label_index: usize,
    break_scopes: Vec<usize>,
    continue_scopes: Vec<usize>,
}

impl<'a> Context<'a> {
    pub fn new(validate: bool) -> Context<'a> {
        Context {
            validate,
            last_var_index: 0,
            last_label_index: 0,
            var_scopes: vec![HashMap::new()],
            break_scopes: Vec::new(),
            continue_scopes: Vec::new(),
        }
    }

    fn push_var_scope(self: &mut Self) {
        if self.validate {
            self.var_scopes.push(HashMap::new());
        }
    }

    fn pop_var_scope(self: &mut Self) {
        if self.validate {
            self.var_scopes.pop();
        }
    }

    fn push_loop_scope(self: &mut Self) -> Label {
        if self.validate {
            self.break_scopes.push(self.last_label_index);
            self.continue_scopes.push(self.last_label_index);
            let label = Label(Some(self.last_label_index));
            self.last_label_index += 1;
            label
        } else {
            Label(None)
        }
    }

    fn pop_loop_scope(self: &mut Self) {
        if self.validate {
            self.break_scopes.pop();
            self.continue_scopes.pop();
        }
    }

    fn get_break_label(self: &mut Self) -> Label {
        if self.validate {
            if !self.break_scopes.is_empty() {
                Label(Some(*self.break_scopes.last().unwrap()))
            } else {
                panic!("Encountered break outside of loop")
            }
        } else {
            Label(None)
        }
    }

    fn get_continue_label(self: &mut Self) -> Label {
        if self.validate {
            if !self.continue_scopes.is_empty() {
                Label(Some(*self.continue_scopes.last().unwrap()))
            } else {
                panic!("Encountered continue outside of loop")
            }
        } else {
            Label(None)
        }
    }

    fn declare_var(self: &mut Self, identifier: &lexer::Identifier<'a>) -> Var {
        if self.validate {
            let scope = self.var_scopes.last_mut().unwrap();
            if scope.contains_key(identifier.0) {
                panic!("Variable is already declared");
            }

            let var = self.last_var_index;
            self.last_var_index += 1;
            scope.insert(identifier.0, var);
            Var(Some(var))
        } else {
            Var(None)
        }
    }

    fn get_var(self: &Self, identifier: &lexer::Identifier<'a>) -> Var {
        if self.validate {
            let mut var = None;
            for scope in self.var_scopes.iter().rev() {
                var = scope.get(identifier.0);
                if var.is_some() {
                    break;
                }
            }

            if let Some(var) = var {
                Var(Some(*var))
            } else {
                panic!("Undeclared variable")
            }
        } else {
            Var(None)
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
pub struct Var(pub Option<usize>);

#[derive(Debug)]
pub struct Func(pub Option<usize>);

#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub var: Var,
    pub expression: Option<Expression<'a>>,
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub func: Func,
    pub params: Vec<Var>,
    pub body: Option<Block<'a>>,
}

#[derive(Debug)]
pub enum Declaration<'a> {
    VariableDeclaration(VariableDeclaration<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
}

#[derive(Debug)]
pub enum Expression<'a> {
    Constant(lexer::Constant<'a>),
    Var(Var),
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
    FunctionCall {
        func: Func,
        args: Vec<Expression<'a>>,
    },
}

#[derive(Debug)]
pub struct Label(pub Option<usize>);

#[derive(Debug)]
pub enum ForInit<'a> {
    InitDecl(VariableDeclaration<'a>),
    InitExp(Expression<'a>),
    Null,
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
    Block(Block<'a>),
    Break(Label),
    Continue(Label),
    While {
        condition: Expression<'a>,
        body: Box<Statement<'a>>,
        label: Label,
    },
    DoWhile {
        body: Box<Statement<'a>>,
        condition: Expression<'a>,
        label: Label,
    },
    For {
        for_init: ForInit<'a>,
        condition: Option<Expression<'a>>,
        post: Option<Expression<'a>>,
        body: Box<Statement<'a>>,
        label: Label,
    },
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Statement(Statement<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug)]
pub struct Block<'a>(pub Vec<BlockItem<'a>>);

#[derive(Debug)]
pub struct Program<'a>(pub Vec<FunctionDeclaration<'a>>);

fn peek_token<'a, 'b>(tokens: &'b Vec<lexer::Token<'a>>) -> &'b lexer::Token<'a> {
    match tokens.last() {
        Some(token) => token,
        None => panic!("Expected token"),
    }
}

fn pop_token<'a>(tokens: &mut Vec<lexer::Token<'a>>) -> lexer::Token<'a> {
    match tokens.pop() {
        Some(token) => token,
        None => panic!("Expected token"),
    }
}

fn parse_unary(tokens: &mut Vec<lexer::Token>) -> UnaryOperator {
    match pop_token(tokens) {
        lexer::Token::Minus => UnaryOperator::Negate,
        lexer::Token::Tilde => UnaryOperator::Complement,
        lexer::Token::Exclamation => UnaryOperator::Not,
        _ => panic!("Unexpected token"),
    }
}

fn parse_binary(tokens: &mut Vec<lexer::Token>) -> BinaryOperator {
    match pop_token(tokens) {
        lexer::Token::Plus => BinaryOperator::Add,
        lexer::Token::Minus => BinaryOperator::Sub,
        lexer::Token::Star => BinaryOperator::Mul,
        lexer::Token::Slash => BinaryOperator::Div,
        lexer::Token::Percent => BinaryOperator::Rem,
        lexer::Token::LessThan => BinaryOperator::LessThan,
        lexer::Token::LessThanEqual => BinaryOperator::LessThanOrEqual,
        lexer::Token::GreaterThan => BinaryOperator::GreaterThan,
        lexer::Token::GreaterThanEqual => BinaryOperator::GreaterThanOrEqual,
        lexer::Token::DoubleEqual => BinaryOperator::Equal,
        lexer::Token::ExclamationEqual => BinaryOperator::NotEqual,
        lexer::Token::DoubleAmpersand => BinaryOperator::And,
        lexer::Token::DoublePipe => BinaryOperator::Or,
        _ => panic!("Unexpected token"),
    }
}

fn parse_factor<'a>(
    tokens: &mut Vec<lexer::Token<'a>>,
    context: &mut Context<'a>,
) -> Expression<'a> {
    match peek_token(tokens) {
        lexer::Token::Constant(_) => {
            if let lexer::Token::Constant(constant) = pop_token(tokens) {
                Expression::Constant(constant)
            } else {
                panic!()
            }
        }
        lexer::Token::Identifier(_) => {
            if let lexer::Token::Identifier(identifier) = pop_token(tokens) {
                if let lexer::Token::OParen = peek_token(tokens) {
                    _ = pop_token(tokens);
                    let mut args = Vec::new();

                    if let lexer::Token::CParen = peek_token(tokens) {
                        _ = pop_token(tokens);
                    } else {
                        loop {
                            args.push(parse_expression(tokens, 0, context));

                            match pop_token(tokens) {
                                lexer::Token::Comma => (),
                                lexer::Token::CParen => break,
                                _ => panic!("Expected ) or ,"),
                            }
                        }
                    }

                    Expression::FunctionCall {
                        func: Func(None),
                        args,
                    }
                } else {
                    Expression::Var(context.get_var(&identifier))
                }
            } else {
                panic!()
            }
        }
        lexer::Token::OParen => {
            if let lexer::Token::OParen = pop_token(tokens) {
                let expression = parse_expression(tokens, 0, context);
                if let lexer::Token::CParen = pop_token(tokens) {
                    expression
                } else {
                    panic!("Expected )");
                }
            } else {
                panic!()
            }
        }
        lexer::Token::Minus | lexer::Token::Tilde | lexer::Token::Exclamation => {
            Expression::Unary {
                operator: parse_unary(tokens),
                expression: Box::new(parse_factor(tokens, context)),
            }
        }
        _ => {
            dbg!(tokens);
            panic!("Unexpected token");
        }
    }
}

fn precedence(token: &lexer::Token) -> Option<usize> {
    match token {
        lexer::Token::Plus | lexer::Token::Minus => Some(45),
        lexer::Token::Star | lexer::Token::Slash | lexer::Token::Percent => Some(50),
        lexer::Token::LessThan
        | lexer::Token::LessThanEqual
        | lexer::Token::GreaterThan
        | lexer::Token::GreaterThanEqual => Some(35),
        lexer::Token::DoubleEqual | lexer::Token::ExclamationEqual => Some(30),
        lexer::Token::DoubleAmpersand => Some(10),
        lexer::Token::DoublePipe => Some(5),
        lexer::Token::Question => Some(3),
        lexer::Token::Equal => Some(1),
        _ => None,
    }
}

fn parse_expression<'a>(
    tokens: &mut Vec<lexer::Token<'a>>,
    min_precedence: usize,
    context: &mut Context<'a>,
) -> Expression<'a> {
    let mut left = parse_factor(tokens, context);
    let mut next_token = peek_token(tokens);
    let mut next_precedence = precedence(next_token);
    while next_precedence.is_some() && next_precedence.unwrap() >= min_precedence {
        if let lexer::Token::Equal = next_token {
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
        } else if let lexer::Token::Question = next_token {
            _ = pop_token(tokens);
            let then = parse_expression(tokens, 0, context);

            if let lexer::Token::Colon = pop_token(tokens) {
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

fn parse_block<'a>(tokens: &mut Vec<lexer::Token<'a>>, context: &mut Context<'a>) -> Block<'a> {
    _ = pop_token(tokens);
    let mut block_items = Vec::new();

    context.push_var_scope();

    loop {
        if let lexer::Token::CBrace = peek_token(tokens) {
            _ = pop_token(tokens);
            break;
        }
        let block_item = parse_block_item(tokens, context);
        block_items.push(block_item);
    }

    context.pop_var_scope();

    Block(block_items)
}
fn parse_statement<'a>(
    tokens: &mut Vec<lexer::Token<'a>>,
    context: &mut Context<'a>,
) -> Statement<'a> {
    if let lexer::Token::Return = peek_token(tokens) {
        _ = pop_token(tokens);
        let expression = parse_expression(tokens, 0, context);
        if let lexer::Token::Semicolon = pop_token(tokens) {
            Statement::Return(expression)
        } else {
            panic!("Expected ;");
        }
    } else if let lexer::Token::If = peek_token(tokens) {
        _ = pop_token(tokens);

        if let lexer::Token::OParen = pop_token(tokens) {
            let condition = parse_expression(tokens, 0, context);

            if let lexer::Token::CParen = pop_token(tokens) {
                let then = parse_statement(tokens, context);

                let els = if let lexer::Token::Else = peek_token(tokens) {
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
    } else if let lexer::Token::Break = peek_token(tokens) {
        _ = pop_token(tokens);
        if let lexer::Token::Semicolon = pop_token(tokens) {
            Statement::Break(context.get_break_label())
        } else {
            panic!("Expected ;")
        }
    } else if let lexer::Token::Continue = peek_token(tokens) {
        _ = pop_token(tokens);
        if let lexer::Token::Semicolon = pop_token(tokens) {
            Statement::Continue(context.get_continue_label())
        } else {
            panic!("Expected ;")
        }
    } else if let lexer::Token::While = peek_token(tokens) {
        _ = pop_token(tokens);

        if let lexer::Token::OParen = pop_token(tokens) {
            let condition = parse_expression(tokens, 0, context);

            if let lexer::Token::CParen = pop_token(tokens) {
                let label = context.push_loop_scope();

                let body = parse_statement(tokens, context);

                context.pop_loop_scope();

                Statement::While {
                    condition,
                    body: Box::new(body),
                    label,
                }
            } else {
                panic!("Expected )");
            }
        } else {
            panic!("Expected (");
        }
    } else if let lexer::Token::Do = peek_token(tokens) {
        _ = pop_token(tokens);

        let label = context.push_loop_scope();

        let body = parse_statement(tokens, context);

        context.pop_loop_scope();

        if let lexer::Token::While = pop_token(tokens) {
            if let lexer::Token::OParen = pop_token(tokens) {
                let condition = parse_expression(tokens, 0, context);

                if let lexer::Token::CParen = pop_token(tokens) {
                    if let lexer::Token::Semicolon = pop_token(tokens) {
                        Statement::DoWhile {
                            body: Box::new(body),
                            condition,
                            label,
                        }
                    } else {
                        panic!("Expected ;");
                    }
                } else {
                    panic!("Expected )");
                }
            } else {
                panic!("Expected (");
            }
        } else {
            panic!("Expected while");
        }
    } else if let lexer::Token::For = peek_token(tokens) {
        _ = pop_token(tokens);

        context.push_var_scope();

        if let lexer::Token::OParen = pop_token(tokens) {
            let for_init = if let lexer::Token::Semicolon = peek_token(tokens) {
                _ = pop_token(tokens);
                ForInit::Null
            } else {
                if let Some(declaration) = maybe_parse_declaration(tokens, context) {
                    if let Declaration::VariableDeclaration(declaration) = declaration {
                        ForInit::InitDecl(declaration)
                    } else {
                        panic!("Expected <variable declaration>")
                    }
                } else {
                    let expression = parse_expression(tokens, 0, context);
                    if let lexer::Token::Semicolon = pop_token(tokens) {
                        ForInit::InitExp(expression)
                    } else {
                        panic!("Expected ;");
                    }
                }
            };

            let condition = if let lexer::Token::Semicolon = peek_token(tokens) {
                _ = pop_token(tokens);
                None
            } else {
                let expression = parse_expression(tokens, 0, context);
                if let lexer::Token::Semicolon = pop_token(tokens) {
                    Some(expression)
                } else {
                    panic!("Expected ;");
                }
            };

            let post = if let lexer::Token::CParen = peek_token(tokens) {
                None
            } else {
                Some(parse_expression(tokens, 0, context))
            };

            if let lexer::Token::CParen = pop_token(tokens) {
                let label = context.push_loop_scope();

                let body = parse_statement(tokens, context);

                context.pop_loop_scope();
                context.pop_var_scope();

                Statement::For {
                    for_init,
                    condition,
                    post,
                    body: Box::new(body),
                    label,
                }
            } else {
                panic!("Expected )");
            }
        } else {
            panic!("Expected (");
        }
    } else if let lexer::Token::Semicolon = peek_token(tokens) {
        _ = pop_token(tokens);
        Statement::Null
    } else if let lexer::Token::OBrace = peek_token(tokens) {
        Statement::Block(parse_block(tokens, context))
    } else {
        let expression = parse_expression(tokens, 0, context);

        if let lexer::Token::Semicolon = pop_token(tokens) {
            Statement::Expression(expression)
        } else {
            panic!("Expected ;");
        }
    }
}

fn parse_declaration<'a>(
    tokens: &mut Vec<lexer::Token<'a>>,
    context: &mut Context<'a>,
) -> Declaration<'a> {
    if let lexer::Token::Int = pop_token(tokens) {
        if let lexer::Token::Identifier(identifier) = pop_token(tokens) {
            if let lexer::Token::OParen = peek_token(tokens) {
                _ = pop_token(tokens);

                let params = if let lexer::Token::Void = peek_token(tokens) {
                    _ = pop_token(tokens);
                    if let lexer::Token::CParen = pop_token(tokens) {
                        Vec::new()
                    } else {
                        panic!("Expected )");
                    }
                } else {
                    let mut params = Vec::new();

                    loop {
                        if let lexer::Token::Int = pop_token(tokens) {
                            if let lexer::Token::Identifier(param_identifier) = pop_token(tokens) {
                                params.push(context.get_var(&param_identifier));

                                match pop_token(tokens) {
                                    lexer::Token::Comma => (),
                                    lexer::Token::CParen => break,
                                    _ => panic!("Expected ) or ,"),
                                }
                            } else {
                                panic!("Expected <identifier>");
                            }
                        } else {
                            panic!("Expected int or void")
                        }
                    }

                    params
                };

                let body = if let lexer::Token::OBrace = peek_token(tokens) {
                    Some(parse_block(tokens, context))
                } else {
                    if let lexer::Token::Semicolon = pop_token(tokens) {
                        None
                    } else {
                        panic!("Expected ;");
                    }
                };

                Declaration::FunctionDeclaration(FunctionDeclaration {
                    func: Func(None),
                    params,
                    body,
                })
            } else {
                let var = context.declare_var(&identifier);

                let expression = match pop_token(tokens) {
                    lexer::Token::Equal => {
                        let expression = parse_expression(tokens, 0, context);
                        if let lexer::Token::Semicolon = pop_token(tokens) {
                            Some(expression)
                        } else {
                            panic!("Expected ;");
                        }
                    }
                    lexer::Token::Semicolon => None,
                    _ => panic!("Expected = or ;"),
                };

                Declaration::VariableDeclaration(VariableDeclaration { var, expression })
            }
        } else {
            panic!("Expected <identifier>");
        }
    } else {
        panic!("Expected int")
    }
}

fn maybe_parse_declaration<'a>(
    tokens: &mut Vec<lexer::Token<'a>>,
    context: &mut Context<'a>,
) -> Option<Declaration<'a>> {
    if let lexer::Token::Int = peek_token(tokens) {
        Some(parse_declaration(tokens, context))
    } else {
        None
    }
}

fn parse_block_item<'a>(
    tokens: &mut Vec<lexer::Token<'a>>,
    context: &mut Context<'a>,
) -> BlockItem<'a> {
    let declaration = maybe_parse_declaration(tokens, context);

    if let Some(declaration) = declaration {
        BlockItem::Declaration(declaration)
    } else {
        BlockItem::Statement(parse_statement(tokens, context))
    }
}

pub fn parse_program<'a>(
    tokens: &mut Vec<lexer::Token<'a>>,
    context: &mut Context<'a>,
) -> Program<'a> {
    let mut function_declarations = Vec::new();

    while !tokens.is_empty() {
        match parse_declaration(tokens, context) {
            Declaration::VariableDeclaration(_) => panic!("Expected <function declaration>"),
            Declaration::FunctionDeclaration(function_declaration) => {
                function_declarations.push(function_declaration)
            }
        }
    }

    Program(function_declarations)
}
