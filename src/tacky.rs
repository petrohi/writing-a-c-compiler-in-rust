use std::collections::HashMap;

use crate::{lexer, parser};

#[derive(Debug, Clone)]
pub enum Val<'a> {
    Constant(lexer::Constant<'a>),
    Tmp(usize),
    Linkage(&'a str),
    NoLinkage(usize),
}

#[derive(Debug, Clone)]
pub struct Label(pub usize);

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
}

#[derive(Debug)]
pub enum Instruction<'a> {
    Unary {
        operator: UnaryOperator,
        src: Val<'a>,
        dst: Val<'a>,
    },
    Binary {
        operator: BinaryOperator,
        src1: Val<'a>,
        src2: Val<'a>,
        dst: Val<'a>,
    },
    Return(Val<'a>),
    Copy {
        src: Val<'a>,
        dst: Val<'a>,
    },
    Jump {
        target: Label,
    },
    JumpIfZero {
        condition: Val<'a>,
        target: Label,
    },
    JumpIfNotZero {
        condition: Val<'a>,
        target: Label,
    },
    Label(Label),
    Call {
        name: &'a str,
        args: Vec<Val<'a>>,
        result: Val<'a>,
        non_local: bool,
    },
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub params: Vec<Val<'a>>,
    pub instructions: Vec<Instruction<'a>>,
    pub global: bool,
}

#[derive(Debug)]
pub enum TopLevelItem<'a> {
    Function(Function<'a>),
    StaticVar(parser::StaticVar<'a>),
}

#[derive(Debug)]
pub struct Program<'a>(pub Vec<TopLevelItem<'a>>);

pub struct Context {
    last_tmp_index: usize,
    last_label_index: usize,
    var_to_tmp: HashMap<usize, usize>,
    break_labels: HashMap<usize, usize>,
    continue_labels: HashMap<usize, usize>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            last_tmp_index: 0,
            last_label_index: 0,
            var_to_tmp: HashMap::new(),
            break_labels: HashMap::new(),
            continue_labels: HashMap::new(),
        }
    }

    fn next_tmp<'b>(&mut self) -> Val<'b> {
        let tmp = Val::Tmp(self.last_tmp_index);
        self.last_tmp_index += 1;
        tmp
    }

    fn resolve_parser_var<'b>(
        &mut self,
        var: &parser::Var<'b>,
        parser_context: &parser::Context<'_>,
    ) -> Val<'b> {
        match var {
            parser::Var::NoLinkage(index) => {
                let index = index.unwrap();

                if parser_context.is_no_linkage_static(var) {
                    Val::NoLinkage(index)
                } else if let Some(tmp) = self.var_to_tmp.get(&index) {
                    Val::Tmp(*tmp)
                } else {
                    let tmp = self.last_tmp_index;
                    self.var_to_tmp.insert(index, tmp);
                    self.last_tmp_index += 1;
                    Val::Tmp(tmp)
                }
            }
            parser::Var::Linkage(name) => Val::Linkage(name),
        }
    }

    fn resolve_break_label(&mut self, break_label: &parser::Label) -> Label {
        let break_label = break_label.0.unwrap();

        if let Some(label) = self.break_labels.get(&break_label) {
            Label(*label)
        } else {
            let label = self.last_label_index;
            self.break_labels.insert(break_label, label);
            self.last_label_index += 1;
            Label(label)
        }
    }

    fn resolve_continue_label(&mut self, continue_label: &parser::Label) -> Label {
        let continue_label = continue_label.0.unwrap();

        if let Some(label) = self.continue_labels.get(&continue_label) {
            Label(*label)
        } else {
            let label = self.last_label_index;
            self.continue_labels.insert(continue_label, label);
            self.last_label_index += 1;
            Label(label)
        }
    }

    fn next_label(&mut self) -> Label {
        let label = Label(self.last_label_index);
        self.last_label_index += 1;
        label
    }
}

fn gen_val<'a>(
    expression: parser::Expression<'a>,
    context: &mut Context,
    parser_context: &parser::Context,
) -> (Val<'a>, Vec<Instruction<'a>>) {
    match expression {
        parser::Expression::Constant(constant) => (Val::Constant(constant), Vec::new()),
        parser::Expression::Unary {
            operator,
            expression,
        } => {
            let unary_operator = match operator {
                parser::UnaryOperator::Complement => Some(UnaryOperator::Complement),
                parser::UnaryOperator::Negate => Some(UnaryOperator::Negate),
                parser::UnaryOperator::Not => Some(UnaryOperator::Not),
            };

            if let Some(unary_operator) = unary_operator {
                let dst = context.next_tmp();
                let (src, mut instructions) = gen_val(*expression, context, parser_context);
                instructions.push(Instruction::Unary {
                    operator: unary_operator,
                    src,
                    dst: dst.clone(),
                });
                (dst, instructions)
            } else {
                unimplemented!()
            }
        }
        parser::Expression::Binary {
            operator,
            left,
            right,
        } => {
            let binary_operator = match operator {
                parser::BinaryOperator::Add => Some(BinaryOperator::Add),
                parser::BinaryOperator::Sub => Some(BinaryOperator::Sub),
                parser::BinaryOperator::Mul => Some(BinaryOperator::Mul),
                parser::BinaryOperator::Div => Some(BinaryOperator::Div),
                parser::BinaryOperator::Rem => Some(BinaryOperator::Rem),
                parser::BinaryOperator::LessThan => Some(BinaryOperator::LessThan),
                parser::BinaryOperator::LessThanOrEqual => Some(BinaryOperator::LessThanOrEqual),
                parser::BinaryOperator::GreaterThan => Some(BinaryOperator::GreaterThan),
                parser::BinaryOperator::GreaterThanOrEqual => {
                    Some(BinaryOperator::GreaterThanOrEqual)
                }
                parser::BinaryOperator::Equal => Some(BinaryOperator::Equal),
                parser::BinaryOperator::NotEqual => Some(BinaryOperator::NotEqual),
                _ => None,
            };

            if let Some(binary_operator) = binary_operator {
                let dst = context.next_tmp();
                let (src1, mut src1_instructions) = gen_val(*left, context, parser_context);
                let (src2, src2_instructions) = gen_val(*right, context, parser_context);
                src1_instructions.extend(src2_instructions);
                src1_instructions.push(Instruction::Binary {
                    operator: binary_operator,
                    src1,
                    src2,
                    dst: dst.clone(),
                });
                (dst, src1_instructions)
            } else if let parser::BinaryOperator::And = operator {
                let dst = context.next_tmp();
                let test1 = context.next_tmp();
                let test2 = context.next_tmp();

                let is_false = context.next_label();
                let end = context.next_label();

                let (src1, mut src1_instructions) = gen_val(*left, context, parser_context);
                let (src2, src2_instructions) = gen_val(*right, context, parser_context);

                src1_instructions.push(Instruction::Copy {
                    src: src1,
                    dst: test1.clone(),
                });
                src1_instructions.push(Instruction::JumpIfZero {
                    condition: test1,
                    target: is_false.clone(),
                });
                src1_instructions.extend(src2_instructions);
                src1_instructions.push(Instruction::Copy {
                    src: src2,
                    dst: test2.clone(),
                });
                src1_instructions.push(Instruction::JumpIfZero {
                    condition: test2,
                    target: is_false.clone(),
                });
                src1_instructions.push(Instruction::Copy {
                    src: Val::Constant(lexer::Constant("1")),
                    dst: dst.clone(),
                });
                src1_instructions.push(Instruction::Jump {
                    target: end.clone(),
                });
                src1_instructions.push(Instruction::Label(is_false));
                src1_instructions.push(Instruction::Copy {
                    src: Val::Constant(lexer::Constant("0")),
                    dst: dst.clone(),
                });
                src1_instructions.push(Instruction::Label(end));

                (dst, src1_instructions)
            } else if let parser::BinaryOperator::Or = operator {
                let dst = context.next_tmp();
                let test1 = context.next_tmp();
                let test2 = context.next_tmp();

                let is_true = context.next_label();
                let end = context.next_label();

                let (src1, mut src1_instructions) = gen_val(*left, context, parser_context);
                let (src2, src2_instructions) = gen_val(*right, context, parser_context);

                src1_instructions.push(Instruction::Copy {
                    src: src1,
                    dst: test1.clone(),
                });
                src1_instructions.push(Instruction::JumpIfNotZero {
                    condition: test1,
                    target: is_true.clone(),
                });
                src1_instructions.extend(src2_instructions);
                src1_instructions.push(Instruction::Copy {
                    src: src2,
                    dst: test2.clone(),
                });
                src1_instructions.push(Instruction::JumpIfNotZero {
                    condition: test2,
                    target: is_true.clone(),
                });
                src1_instructions.push(Instruction::Copy {
                    src: Val::Constant(lexer::Constant("0")),
                    dst: dst.clone(),
                });
                src1_instructions.push(Instruction::Jump {
                    target: end.clone(),
                });
                src1_instructions.push(Instruction::Label(is_true));
                src1_instructions.push(Instruction::Copy {
                    src: Val::Constant(lexer::Constant("1")),
                    dst: dst.clone(),
                });
                src1_instructions.push(Instruction::Label(end));

                (dst, src1_instructions)
            } else {
                unimplemented!()
            }
        }
        parser::Expression::Var(var) => {
            let dst = context.resolve_parser_var(&var, parser_context);
            (dst, Vec::new())
        }
        parser::Expression::Assignment { lvalue, rvalue } => {
            let (lvalue, mut lvalue_instructions) = gen_val(*lvalue, context, parser_context);
            let (rvalue, rvalue_instructions) = gen_val(*rvalue, context, parser_context);
            lvalue_instructions.extend(rvalue_instructions);
            lvalue_instructions.push(Instruction::Copy {
                src: rvalue,
                dst: lvalue.clone(),
            });
            (lvalue, lvalue_instructions)
        }
        parser::Expression::Conditional {
            condition,
            then,
            els,
        } => {
            let dst = context.next_tmp();
            let (condition, mut condition_instructions) =
                gen_val(*condition, context, parser_context);
            let els_label = context.next_label();
            let end = context.next_label();
            condition_instructions.push(Instruction::JumpIfZero {
                condition,
                target: els_label.clone(),
            });

            let (then, then_instructions) = gen_val(*then, context, parser_context);
            condition_instructions.extend(then_instructions);
            condition_instructions.extend([
                Instruction::Copy {
                    src: then,
                    dst: dst.clone(),
                },
                Instruction::Jump {
                    target: end.clone(),
                },
                Instruction::Label(els_label),
            ]);

            let (els, els_instructions) = gen_val(*els, context, parser_context);
            condition_instructions.extend(els_instructions);
            condition_instructions.extend([
                Instruction::Copy {
                    src: els,
                    dst: dst.clone(),
                },
                Instruction::Label(end),
            ]);

            (dst, condition_instructions)
        }
        parser::Expression::FunctionCall { func, args } => {
            let mut instructions = Vec::new();
            let args = args
                .into_iter()
                .map(|arg| {
                    let (arg, arg_instructions) = gen_val(arg, context, parser_context);
                    instructions.extend(arg_instructions);
                    arg
                })
                .collect();
            let result = context.next_tmp();
            let parser::Func(name) = func;
            instructions.push(Instruction::Call {
                name,
                args,
                result: result.clone(),
                non_local: !parser_context.is_defined_function(&func),
            });

            (result, instructions)
        }
    }
}

fn gen_statement<'a>(
    statement: parser::Statement<'a>,
    context: &mut Context,
    parser_context: &parser::Context,
) -> Vec<Instruction<'a>> {
    match statement {
        parser::Statement::Return(expression) => {
            let (val, mut instructions) = gen_val(expression, context, parser_context);
            instructions.push(Instruction::Return(val));
            instructions
        }
        parser::Statement::Expression(expression) => {
            let (_, instructions) = gen_val(expression, context, parser_context);
            instructions
        }
        parser::Statement::Null => Vec::new(),
        parser::Statement::If {
            condition,
            then,
            els,
        } => {
            let (condition, mut condition_instructions) =
                gen_val(condition, context, parser_context);
            let els_or_end_label = context.next_label();
            condition_instructions.push(Instruction::JumpIfZero {
                condition,
                target: els_or_end_label.clone(),
            });

            condition_instructions.extend(gen_statement(*then, context, parser_context));

            if let Some(els) = els {
                let end = context.next_label();
                condition_instructions.extend([
                    Instruction::Jump {
                        target: end.clone(),
                    },
                    Instruction::Label(els_or_end_label),
                ]);

                condition_instructions.extend(gen_statement(*els, context, parser_context));
                condition_instructions.push(Instruction::Label(end));
            } else {
                condition_instructions.push(Instruction::Label(els_or_end_label));
            }

            condition_instructions
        }
        parser::Statement::Block(block) => gen_block(block, context, parser_context),
        parser::Statement::Break(label) => vec![Instruction::Jump {
            target: context.resolve_break_label(&label),
        }],
        parser::Statement::Continue(label) => vec![Instruction::Jump {
            target: context.resolve_continue_label(&label),
        }],
        parser::Statement::While {
            condition,
            body,
            label,
        } => {
            let mut instructions = Vec::new();
            let start = context.resolve_continue_label(&label);
            let end = context.resolve_break_label(&label);

            instructions.push(Instruction::Label(start.clone()));
            let (condition, condition_instructions) = gen_val(condition, context, parser_context);

            instructions.extend(condition_instructions);
            instructions.push(Instruction::JumpIfZero {
                condition,
                target: end.clone(),
            });
            instructions.extend(gen_statement(*body, context, parser_context));
            instructions.extend([Instruction::Jump { target: start }, Instruction::Label(end)]);

            instructions
        }
        parser::Statement::DoWhile {
            body,
            condition,
            label,
        } => {
            let mut instructions = Vec::new();
            let start = context.next_label();

            instructions.push(Instruction::Label(start.clone()));
            instructions.extend(gen_statement(*body, context, parser_context));
            instructions.push(Instruction::Label(context.resolve_continue_label(&label)));

            let (condition, condition_instructions) = gen_val(condition, context, parser_context);

            instructions.extend(condition_instructions);
            instructions.extend([
                Instruction::JumpIfNotZero {
                    condition,
                    target: start,
                },
                Instruction::Label(context.resolve_break_label(&label)),
            ]);

            instructions
        }
        parser::Statement::For {
            for_init,
            condition,
            post,
            body,
            label,
        } => {
            let mut instructions = match for_init {
                parser::ForInit::InitDecl(declaration) => {
                    gen_variable_declaration(declaration, context, parser_context)
                }
                parser::ForInit::InitExp(expression) => {
                    let (_, init_instructions) = gen_val(expression, context, parser_context);
                    init_instructions
                }
                parser::ForInit::Null => Vec::new(),
            };

            let start = context.next_label();
            let end = context.resolve_break_label(&label);

            instructions.push(Instruction::Label(start.clone()));

            if let Some(condition) = condition {
                let (condition, condition_instructions) =
                    gen_val(condition, context, parser_context);

                instructions.extend(condition_instructions);
                instructions.push(Instruction::JumpIfZero {
                    condition,
                    target: end.clone(),
                });
            }

            instructions.extend(gen_statement(*body, context, parser_context));
            instructions.push(Instruction::Label(context.resolve_continue_label(&label)));

            if let Some(post) = post {
                let (_, post_instructions) = gen_val(post, context, parser_context);
                instructions.extend(post_instructions);
            }

            instructions.extend([Instruction::Jump { target: start }, Instruction::Label(end)]);

            instructions
        }
    }
}

fn gen_variable_declaration<'a>(
    declaration: parser::VariableDeclaration<'a>,
    context: &mut Context,
    parser_context: &parser::Context,
) -> Vec<Instruction<'a>> {
    let parser::VariableDeclaration { var, expression } = declaration;

    if let Some(expression) = expression {
        let (src, mut instructions) = gen_val(expression, context, parser_context);
        let dst = context.resolve_parser_var(&var, parser_context);
        instructions.push(Instruction::Copy { src, dst });
        instructions
    } else {
        Vec::new()
    }
}

fn gen_block<'a>(
    block: parser::Block<'a>,
    context: &mut Context,
    parser_context: &parser::Context,
) -> Vec<Instruction<'a>> {
    let mut instructions = Vec::new();

    for block_item in block.0 {
        match block_item {
            parser::BlockItem::Statement(statement) => {
                instructions.extend(gen_statement(statement, context, parser_context));
            }
            parser::BlockItem::Declaration(declaration) => match declaration {
                parser::Declaration::Variable(variable_declaration) => instructions.extend(
                    gen_variable_declaration(variable_declaration, context, parser_context),
                ),
                parser::Declaration::Function => (),
                parser::Declaration::FunctionDefinition(_) => panic!(),
            },
        }
    }

    instructions
}

fn maybe_gen_function<'a>(
    declaration: parser::Declaration<'a>,
    context: &mut Context,
    parser_context: &parser::Context,
) -> Option<Function<'a>> {
    match declaration {
        parser::Declaration::Variable(_) => None,
        parser::Declaration::Function => None,
        parser::Declaration::FunctionDefinition(function_definition) => {
            let parser::FunctionDefinition { func, body, params } = function_definition;
            let mut instructions = Vec::new();
            instructions.extend(gen_block(body, context, parser_context));
            instructions.push(Instruction::Return(Val::Constant(lexer::Constant("0"))));

            let params = params
                .into_iter()
                .map(|param| context.resolve_parser_var(&param, parser_context))
                .collect();
            let parser::Func(name) = func;
            Some(Function {
                name,
                params,
                instructions,
                global: parser_context.is_global_function(&func),
            })
        }
    }
}

pub fn gen_program<'a>(
    program: parser::Program<'a>,
    context: &mut Context,
    parser_context: &parser::Context<'a>,
) -> Program<'a> {
    let parser::Program(function_declarations) = program;
    let mut top_level_items = Vec::new();

    for function_declaration in function_declarations {
        if let Some(function) = maybe_gen_function(function_declaration, context, parser_context) {
            top_level_items.push(TopLevelItem::Function(function));
        }
    }

    top_level_items.extend(
        parser_context
            .get_static_vars()
            .iter()
            .map(|static_var| TopLevelItem::StaticVar(static_var.clone())),
    );

    Program(top_level_items)
}
