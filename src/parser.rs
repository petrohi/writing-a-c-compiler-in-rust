use std::collections::{HashMap, HashSet};

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

#[derive(Debug, Clone)]
enum InitialValue<'a> {
    Tentative,
    Init(Option<&'a str>),
    InitZero,
    NoInit,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
enum SymbolKey<'a> {
    NoLinkage(usize),
    Linkage(&'a str),
}

#[derive(Debug)]
enum Symbol<'a> {
    Func {
        arity: usize,
        defined: bool,
        global: bool,
    },
    Static {
        initial_value: InitialValue<'a>,
        global: bool,
    },
    Local,
}

pub struct Context<'a> {
    validate: bool,
    last_var_index: usize,
    symbols: HashMap<SymbolKey<'a>, Symbol<'a>>,
    id_scopes: Vec<HashMap<&'a str, Identifier<'a>>>,
    last_label_index: usize,
    break_scopes: Vec<usize>,
    continue_scopes: Vec<usize>,
}

fn identifier_to_symbol_key<'a>(identifier: &Identifier<'a>) -> SymbolKey<'a> {
    match identifier {
        Identifier::Var(var) => match var {
            Var::NoLinkage(index) => SymbolKey::NoLinkage(index.unwrap()),
            Var::Linkage(name) => SymbolKey::Linkage(name),
        },
        Identifier::Func(func) => {
            let Func(name) = func;
            SymbolKey::Linkage(name)
        }
    }
}

#[derive(Clone, Debug)]
pub enum StaticVarName<'a> {
    NoLinkage(usize),
    Linkage(&'a str),
}

#[derive(Clone, Debug)]
pub struct StaticVar<'a> {
    pub name: StaticVarName<'a>,
    pub global: bool,
    pub init: &'a str,
}

impl<'a> Context<'a> {
    pub fn new(validate: bool) -> Context<'a> {
        Context {
            validate,
            last_var_index: 0,
            last_label_index: 0,
            symbols: HashMap::new(),
            id_scopes: vec![HashMap::new()],
            break_scopes: Vec::new(),
            continue_scopes: Vec::new(),
        }
    }

    pub fn is_global_function(self: &Self, func: &Func) -> bool {
        let symbol_key = identifier_to_symbol_key(&Identifier::Func(func.clone()));
        if let Symbol::Func { global, .. } = self.symbols.get(&symbol_key).unwrap() {
            *global
        } else {
            false
        }
    }

    pub fn is_defined_function(self: &Self, func: &Func) -> bool {
        let symbol_key = identifier_to_symbol_key(&Identifier::Func(func.clone()));
        if let Symbol::Func { defined, .. } = self.symbols.get(&symbol_key).unwrap() {
            *defined
        } else {
            false
        }
    }

    pub fn is_no_linkage_static(self: &Self, var: &Var) -> bool {
        if let Var::NoLinkage(_) = var {
            let symbol_key = identifier_to_symbol_key(&Identifier::Var(var.clone()));
            if let Symbol::Static { .. } = self.symbols.get(&symbol_key).unwrap() {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn get_static_vars(self: &Self) -> Vec<StaticVar<'a>> {
        let mut static_vars = Vec::new();

        for (key, symbol) in self.symbols.iter() {
            match symbol {
                Symbol::Static {
                    initial_value,
                    global,
                } => {
                    let name = match key {
                        SymbolKey::NoLinkage(index) => StaticVarName::NoLinkage(*index),
                        SymbolKey::Linkage(name) => StaticVarName::Linkage(name),
                    };

                    let init = match initial_value {
                        InitialValue::Tentative | InitialValue::InitZero => Some("0"),
                        InitialValue::Init(value) => Some(value.unwrap()),
                        InitialValue::NoInit => None,
                    };

                    if let Some(init) = init {
                        static_vars.push(StaticVar {
                            name,
                            global: *global,
                            init,
                        });
                    }
                }
                _ => (),
            }
        }

        static_vars
    }

    fn push_var_scope(self: &mut Self) {
        if self.validate {
            self.id_scopes.push(HashMap::new());
        }
    }

    fn pop_var_scope(self: &mut Self) {
        if self.validate {
            self.id_scopes.pop();
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

    fn declare_or_define_var(
        self: &mut Self,
        lexical_identifier: &lexer::Identifier<'a>,
        declaration_specification: DeclarationSpecification,
        defined: bool,
        for_init: bool,
    ) -> Var<'a> {
        if self.validate {
            if self.id_scopes.len() == 1 {
                let var = Var::Linkage(lexical_identifier.0);
                let identifier = Identifier::Var(var.clone());
                let symbol_key = identifier_to_symbol_key(&identifier);

                let mut initial_value = if defined {
                    InitialValue::Init(None)
                } else {
                    if let DeclarationSpecification::Extern = declaration_specification {
                        InitialValue::NoInit
                    } else {
                        InitialValue::Tentative
                    }
                };

                let mut global = if let DeclarationSpecification::Static = declaration_specification
                {
                    false
                } else {
                    true
                };

                match self.symbols.get(&symbol_key) {
                    Some(Symbol::Static {
                        initial_value: symbol_initial_value,
                        global: symbol_global,
                    }) => {
                        if let DeclarationSpecification::Extern = declaration_specification {
                            global = *symbol_global
                        }

                        if *symbol_global != global {
                            panic!("Conflicting variable linkage")
                        }

                        match symbol_initial_value {
                            InitialValue::Init(_) | InitialValue::InitZero => match initial_value {
                                InitialValue::Init(_) | InitialValue::InitZero => {
                                    panic!("Variable redefined");
                                }
                                _ => {
                                    initial_value = symbol_initial_value.clone();
                                }
                            },
                            InitialValue::Tentative => match initial_value {
                                InitialValue::Init(_) | InitialValue::InitZero => (),
                                _ => {
                                    initial_value = symbol_initial_value.clone();
                                }
                            },
                            InitialValue::NoInit => (),
                        }
                    }
                    Some(_) => {
                        panic!("Existing symbol redeclared as variable");
                    }
                    None => (),
                }

                self.symbols.insert(
                    symbol_key,
                    Symbol::Static {
                        initial_value,
                        global,
                    },
                );

                match self.resolve_identifier(&lexical_identifier) {
                    Some((Identifier::Func(_), _)) => panic!("Function redeclared as variable"),
                    _ => {
                        self.associate_identifiers_in_current_scope(
                            &lexical_identifier,
                            identifier,
                        );
                    }
                }

                var
            } else {
                let var = if let DeclarationSpecification::Extern = declaration_specification {
                    Var::Linkage(lexical_identifier.0)
                } else {
                    let var = Var::NoLinkage(Some(self.last_var_index));
                    self.last_var_index += 1;
                    var
                };
                let identifier = Identifier::Var(var.clone());
                let symbol_key = identifier_to_symbol_key(&identifier);

                match declaration_specification {
                    DeclarationSpecification::Extern => {
                        if defined {
                            panic!("Initializer on local extern variable");
                        }

                        match self.symbols.get(&symbol_key) {
                            Some(Symbol::Static { .. }) => (),
                            Some(_) => {
                                panic!("Existing symbol redeclared as variable");
                            }
                            None => {
                                self.symbols.insert(
                                    symbol_key,
                                    Symbol::Static {
                                        initial_value: InitialValue::NoInit,
                                        global: true,
                                    },
                                );
                            }
                        }
                    }
                    DeclarationSpecification::Static => {
                        if for_init {
                            panic!("Can't use static in for init");
                        }

                        let initial_value = if defined {
                            InitialValue::Init(None)
                        } else {
                            InitialValue::InitZero
                        };

                        self.symbols.insert(
                            symbol_key,
                            Symbol::Static {
                                initial_value,
                                global: false,
                            },
                        );
                    }
                    DeclarationSpecification::None => {
                        self.symbols.insert(symbol_key, Symbol::Local);
                    }
                }

                match self.resolve_identifier(&lexical_identifier) {
                    Some((Identifier::Func(_), true)) => panic!("Function redeclared as variable"),
                    Some((Identifier::Var(var), true)) => {
                        if let Var::Linkage(_) = var {
                            match declaration_specification {
                                DeclarationSpecification::Extern => (),
                                _ => {
                                    panic!("Conflicting variable declarations");
                                }
                            }
                        } else {
                            panic!("Conflicting variable declarations");
                        }
                    }
                    _ => {
                        self.associate_identifiers_in_current_scope(
                            &lexical_identifier,
                            identifier,
                        );
                    }
                }

                var
            }
        } else {
            Var::NoLinkage(None)
        }
    }

    fn define_var_and_try_folding_expression(
        self: &mut Self,
        identifier: &Identifier<'a>,
        expression: &Expression<'a>,
    ) -> bool {
        if self.validate {
            let symbol_key = identifier_to_symbol_key(identifier);
            match self.symbols.get(&symbol_key) {
                None | Some(Symbol::Local) => false,
                Some(Symbol::Static {
                    initial_value,
                    global,
                }) => {
                    if let InitialValue::Init(None) = initial_value {
                        if let Expression::Constant(lexer::Constant(value)) = expression {
                            self.symbols.insert(
                                symbol_key,
                                Symbol::Static {
                                    initial_value: InitialValue::Init(Some(*value)),
                                    global: *global,
                                },
                            );

                            true
                        } else {
                            panic!("Non-constant initializer");
                        }
                    } else {
                        panic!("Unexpected inital value");
                    }
                }
                _ => panic!("Unexpected symbol"),
            }
        } else {
            false
        }
    }

    fn declare_or_define_func(
        self: &mut Self,
        lexical_identifier: &lexer::Identifier<'a>,
        param_identifiers: &Vec<lexer::Identifier<'a>>,
        declaration_specification: DeclarationSpecification,
        defined: bool,
    ) -> Func<'a> {
        let func = Func(lexical_identifier.0);
        let identifier = Identifier::Func(func.clone());
        let symbol_key = identifier_to_symbol_key(&identifier);

        if self.validate {
            if defined && self.id_scopes.len() != 1 {
                panic!("Nested function declaration not permitted");
            }

            let arity = param_identifiers.len();
            let mut global = if let DeclarationSpecification::Static = declaration_specification {
                if self.id_scopes.len() != 1 {
                    panic!("Can't declare static function on block scope");
                }

                false
            } else {
                true
            };
            let mut unique_param_identifiers = HashSet::new();

            for param_identifier in param_identifiers {
                if !unique_param_identifiers.insert(param_identifier.0) {
                    panic!("Parameter names must be unique");
                }
            }

            let mut defined = defined;

            match self.symbols.get(&symbol_key) {
                Some(Symbol::Func {
                    arity: symbol_arity,
                    defined: symbol_defined,
                    global: symbol_global,
                }) => {
                    if defined && *symbol_defined {
                        panic!("Function redefined");
                    }

                    if *symbol_arity != arity {
                        panic!("Function redeclared with different arity");
                    }

                    if let DeclarationSpecification::Static = declaration_specification {
                        if *symbol_global {
                            panic!("Static function declaration is followed by non-static")
                        }
                    }

                    global = *symbol_global;
                    defined = defined || *symbol_defined;
                }
                Some(_) => {
                    panic!("Existing symbol redeclared as function");
                }
                None => (),
            }

            self.symbols.insert(
                symbol_key,
                Symbol::Func {
                    arity,
                    defined,
                    global,
                },
            );

            match self.resolve_identifier(&lexical_identifier) {
                Some((Identifier::Func(_), true)) => (),
                Some((Identifier::Var(_), true)) => panic!("Variable redeclared as function"),
                _ => {
                    self.associate_identifiers_in_current_scope(&lexical_identifier, identifier);
                }
            }
        }

        func
    }

    fn get_var(self: &Self, identifier: &lexer::Identifier<'a>) -> Var<'a> {
        if self.validate {
            if let Some((id, _)) = self.resolve_identifier(identifier) {
                if let Identifier::Var(var) = id {
                    match var {
                        Var::NoLinkage(index) => Var::NoLinkage(index),
                        Var::Linkage(_) => Var::Linkage(identifier.0),
                    }
                } else {
                    panic!("Function redefined as variable")
                }
            } else {
                panic!("Undeclared variable")
            }
        } else {
            Var::NoLinkage(None)
        }
    }

    fn get_func(self: &Self, lexical_identifier: &lexer::Identifier<'a>, arity: usize) -> Func<'a> {
        let func = Func(lexical_identifier.0);
        let identifier = Identifier::Func(func.clone());
        let symbol_key = identifier_to_symbol_key(&identifier);

        if self.validate {
            match self.symbols.get(&symbol_key) {
                Some(Symbol::Func {
                    arity: symbol_arity,
                    ..
                }) => {
                    if *symbol_arity != arity {
                        panic!("Function called with different signature");
                    }
                }
                _ => {
                    panic!("Undeclared function")
                }
            };

            match self.resolve_identifier(&lexical_identifier) {
                Some((Identifier::Func(_), _)) => (),
                _ => {
                    panic!("Variable used as function");
                }
            }
        }

        func
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

    fn associate_identifiers_in_current_scope(
        self: &mut Self,
        lexical_identifier: &lexer::Identifier<'a>,
        identifier: Identifier<'a>,
    ) {
        self.id_scopes
            .last_mut()
            .unwrap()
            .insert(lexical_identifier.0, identifier);
    }

    fn resolve_identifier(
        self: &Self,
        lexical_identifier: &lexer::Identifier<'a>,
    ) -> Option<(Identifier, bool)> {
        let mut identifier = None;
        let mut current = true;
        for scope in self.id_scopes.iter().rev() {
            identifier = scope.get(lexical_identifier.0).cloned();
            if identifier.is_some() {
                break;
            }

            if current {
                current = false;
            }
        }
        identifier.map(|identifier| (identifier, current))
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Var<'a> {
    NoLinkage(Option<usize>),
    Linkage(&'a str),
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Func<'a>(pub &'a str);

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Identifier<'a> {
    Var(Var<'a>),
    Func(Func<'a>),
}

#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub var: Var<'a>,
    pub expression: Option<Expression<'a>>,
}

#[derive(Debug)]
pub struct FunctionDefinition<'a> {
    pub func: Func<'a>,
    pub params: Vec<Var<'a>>,
    pub body: Block<'a>,
}

#[derive(Debug)]
pub enum Declaration<'a> {
    VariableDeclaration(VariableDeclaration<'a>),
    FunctionDeclaration,
    FunctionDefinition(FunctionDefinition<'a>),
}

#[derive(Debug)]
pub enum DeclarationSpecification {
    None,
    Static,
    Extern,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Constant(lexer::Constant<'a>),
    Var(Var<'a>),
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
        func: Func<'a>,
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
pub struct Program<'a>(pub Vec<Declaration<'a>>);

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

                    let func = context.get_func(&identifier, args.len());

                    Expression::FunctionCall { func, args }
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

    loop {
        if let lexer::Token::CBrace = peek_token(tokens) {
            _ = pop_token(tokens);
            break;
        }
        let block_item = parse_block_item(tokens, context);
        block_items.push(block_item);
    }

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
                if let Some(declaration) = maybe_parse_declaration(tokens, context, true) {
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
        context.push_var_scope();

        let statement = Statement::Block(parse_block(tokens, context));

        context.pop_var_scope();

        statement
    } else {
        let expression = parse_expression(tokens, 0, context);

        if let lexer::Token::Semicolon = pop_token(tokens) {
            Statement::Expression(expression)
        } else {
            panic!("Expected ;");
        }
    }
}

fn maybe_parse_declaration<'a>(
    tokens: &mut Vec<lexer::Token<'a>>,
    context: &mut Context<'a>,
    for_init: bool,
) -> Option<Declaration<'a>> {
    let mut specifier_tokens = Vec::new();

    loop {
        match peek_token(tokens) {
            lexer::Token::Int | lexer::Token::Extern | lexer::Token::Static => {
                specifier_tokens.push(pop_token(tokens))
            }
            _ => break,
        }
    }

    if !specifier_tokens.is_empty() {
        let int_count = specifier_tokens
            .iter()
            .filter(|t| {
                if let lexer::Token::Int = t {
                    true
                } else {
                    false
                }
            })
            .count();
        let extern_count = specifier_tokens
            .iter()
            .filter(|t| {
                if let lexer::Token::Extern = t {
                    true
                } else {
                    false
                }
            })
            .count();
        let static_count = specifier_tokens
            .iter()
            .filter(|t| {
                if let lexer::Token::Static = t {
                    true
                } else {
                    false
                }
            })
            .count();

        if !(int_count == 1
            && ((extern_count == 0 && static_count == 0)
                || (extern_count == 1 && static_count == 0)
                || (extern_count == 0 && static_count == 1)))
        {
            panic!("Expected int with optional static or extern");
        }

        let declaration_specification = if static_count == 1 {
            DeclarationSpecification::Static
        } else if extern_count == 1 {
            DeclarationSpecification::Extern
        } else {
            DeclarationSpecification::None
        };

        let declaration = if let lexer::Token::Identifier(identifier) = pop_token(tokens) {
            if let lexer::Token::OParen = peek_token(tokens) {
                _ = pop_token(tokens);

                let param_identifiers = if let lexer::Token::Void = peek_token(tokens) {
                    _ = pop_token(tokens);
                    if let lexer::Token::CParen = pop_token(tokens) {
                        Vec::new()
                    } else {
                        panic!("Expected )");
                    }
                } else {
                    let mut param_identifiers = Vec::new();

                    loop {
                        if let lexer::Token::Int = pop_token(tokens) {
                            if let lexer::Token::Identifier(param_identifier) = pop_token(tokens) {
                                param_identifiers.push(param_identifier);

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

                    param_identifiers
                };

                if let lexer::Token::OBrace = peek_token(tokens) {
                    let func = context.declare_or_define_func(
                        &identifier,
                        &param_identifiers,
                        declaration_specification,
                        true,
                    );

                    context.push_var_scope();

                    let params = param_identifiers
                        .into_iter()
                        .map(|identifier| {
                            context.declare_or_define_var(
                                &identifier,
                                DeclarationSpecification::None,
                                false,
                                false,
                            )
                        })
                        .collect();
                    let body = parse_block(tokens, context);

                    context.pop_var_scope();

                    Declaration::FunctionDefinition(FunctionDefinition { func, params, body })
                } else {
                    if let lexer::Token::Semicolon = pop_token(tokens) {
                        _ = context.declare_or_define_func(
                            &identifier,
                            &param_identifiers,
                            declaration_specification,
                            false,
                        );

                        Declaration::FunctionDeclaration
                    } else {
                        panic!("Expected ;");
                    }
                }
            } else {
                let var = if let lexer::Token::Equal = peek_token(tokens) {
                    context.declare_or_define_var(
                        &identifier,
                        declaration_specification,
                        true,
                        for_init,
                    )
                } else {
                    context.declare_or_define_var(
                        &identifier,
                        declaration_specification,
                        false,
                        for_init,
                    )
                };

                let expression = match pop_token(tokens) {
                    lexer::Token::Equal => {
                        let expression = parse_expression(tokens, 0, context);

                        if let lexer::Token::Semicolon = pop_token(tokens) {
                            if context.define_var_and_try_folding_expression(
                                &Identifier::Var(var.clone()),
                                &expression,
                            ) {
                                None
                            } else {
                                Some(expression)
                            }
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
        };

        Some(declaration)
    } else {
        None
    }
}

fn parse_block_item<'a>(
    tokens: &mut Vec<lexer::Token<'a>>,
    context: &mut Context<'a>,
) -> BlockItem<'a> {
    let declaration = maybe_parse_declaration(tokens, context, false);

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
    let mut declarations = Vec::new();

    while !tokens.is_empty() {
        if let Some(declaration) = maybe_parse_declaration(tokens, context, false) {
            declarations.push(declaration);
        } else {
            panic!("Expected <declaration>")
        }
    }

    Program(declarations)
}
