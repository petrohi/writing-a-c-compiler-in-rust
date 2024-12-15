use std::collections::HashMap;

use crate::{
    lexer,
    parser,
};

#[derive(Debug, Clone)]
pub enum Val<'a> {
    Constant(lexer::Constant<'a>),
    Tmp(usize),
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
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: lexer::Identifier<'a>,
    pub instructions: Vec<Instruction<'a>>,
}

#[derive(Debug)]
pub struct Program<'a>(pub Vec<Function<'a>>);

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

    fn next_tmp<'a, 'b>(self: &'a mut Self) -> Val<'b> {
        let tmp = Val::Tmp(self.last_tmp_index);
        self.last_tmp_index += 1;
        tmp
    }

    fn resolve_parser_var<'a, 'b>(self: &'a mut Self, var: &parser::Var) -> Val<'b> {
        let var = var.0.unwrap();

        if let Some(tmp) = self.var_to_tmp.get(&var) {
            Val::Tmp(*tmp)
        } else {
            let tmp = self.last_tmp_index;
            self.var_to_tmp.insert(var, tmp);
            self.last_tmp_index += 1;
            Val::Tmp(tmp)
        }
    }

    fn resolve_break_label<'a, 'b>(self: &'a mut Self, break_label: &parser::Label) -> Label {
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

    fn resolve_continue_label<'a, 'b>(self: &'a mut Self, continue_label: &parser::Label) -> Label {
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

    fn next_label(self: &mut Self) -> Label {
        let label = Label(self.last_label_index);
        self.last_label_index += 1;
        label
    }
}

fn gen_val<'a, 'b>(
    expression: parser::Expression<'a>,
    context: &'b mut Context,
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
                let (src, mut instructions) = gen_val(*expression, context);
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
                let (src1, mut src1_instructions) = gen_val(*left, context);
                let (src2, src2_instructions) = gen_val(*right, context);
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

                let (src1, mut src1_instructions) = gen_val(*left, context);
                let (src2, src2_instructions) = gen_val(*right, context);

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

                let (src1, mut src1_instructions) = gen_val(*left, context);
                let (src2, src2_instructions) = gen_val(*right, context);

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
            let dst = context.resolve_parser_var(&var);
            (dst, Vec::new())
        }
        parser::Expression::Assignment { lvalue, rvalue } => {
            let (lvalue, mut lvalue_instructions) = gen_val(*lvalue, context);
            let (rvalue, rvalue_instructions) = gen_val(*rvalue, context);
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
            let (condition, mut condition_instructions) = gen_val(*condition, context);
            let els_label = context.next_label();
            let end = context.next_label();
            condition_instructions.push(Instruction::JumpIfZero {
                condition,
                target: els_label.clone(),
            });

            let (then, then_instructions) = gen_val(*then, context);
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

            let (els, els_instructions) = gen_val(*els, context);
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
        parser::Expression::FunctionCall { func, args } => todo!(),
    }
}

fn gen_statement<'a, 'b>(
    statement: parser::Statement<'a>,
    context: &'b mut Context,
) -> Vec<Instruction<'a>> {
    match statement {
        parser::Statement::Return(expression) => {
            let (val, mut instructions) = gen_val(expression, context);
            instructions.push(Instruction::Return(val));
            instructions
        }
        parser::Statement::Expression(expression) => {
            let (_, instructions) = gen_val(expression, context);
            instructions
        }
        parser::Statement::Null => Vec::new(),
        parser::Statement::If {
            condition,
            then,
            els,
        } => {
            let (condition, mut condition_instructions) = gen_val(condition, context);
            let els_or_end_label = context.next_label();
            condition_instructions.push(Instruction::JumpIfZero {
                condition,
                target: els_or_end_label.clone(),
            });

            condition_instructions.extend(gen_statement(*then, context));

            if let Some(els) = els {
                let end = context.next_label();
                condition_instructions.extend([
                    Instruction::Jump {
                        target: end.clone(),
                    },
                    Instruction::Label(els_or_end_label),
                ]);

                condition_instructions.extend(gen_statement(*els, context));
                condition_instructions.push(Instruction::Label(end));
            } else {
                condition_instructions.push(Instruction::Label(els_or_end_label));
            }

            condition_instructions
        }
        parser::Statement::Block(block) => gen_block(block, context),
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
            let (condition, condition_instructions) = gen_val(condition, context);

            instructions.extend(condition_instructions);
            instructions.push(Instruction::JumpIfZero {
                condition,
                target: end.clone(),
            });
            instructions.extend(gen_statement(*body, context));
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
            instructions.extend(gen_statement(*body, context));
            instructions.push(Instruction::Label(context.resolve_continue_label(&label)));

            let (condition, condition_instructions) = gen_val(condition, context);

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
                    gen_variable_declaration(declaration, context)
                }
                parser::ForInit::InitExp(expression) => {
                    let (_, init_instructions) = gen_val(expression, context);
                    init_instructions
                }
                parser::ForInit::Null => Vec::new(),
            };

            let start = context.next_label();
            let end = context.resolve_break_label(&label);

            instructions.push(Instruction::Label(start.clone()));

            if let Some(condition) = condition {
                let (condition, condition_instructions) = gen_val(condition, context);

                instructions.extend(condition_instructions);
                instructions.push(Instruction::JumpIfZero {
                    condition,
                    target: end.clone(),
                });
            }

            instructions.extend(gen_statement(*body, context));
            instructions.push(Instruction::Label(context.resolve_continue_label(&label)));

            if let Some(post) = post {
                let (_, post_instructions) = gen_val(post, context);
                instructions.extend(post_instructions);
            }

            instructions.extend([Instruction::Jump { target: start }, Instruction::Label(end)]);

            instructions
        }
    }
}

fn gen_variable_declaration<'a, 'b>(
    declaration: parser::VariableDeclaration<'a>,
    context: &'b mut Context,
) -> Vec<Instruction<'a>> {
    let parser::VariableDeclaration { var, expression } = declaration;

    if let Some(expression) = expression {
        let (src, mut instructions) = gen_val(expression, context);
        let dst = context.resolve_parser_var(&var);
        instructions.push(Instruction::Copy { src, dst });
        instructions
    } else {
        Vec::new()
    }
}

fn gen_block<'a, 'b>(block: parser::Block<'a>, context: &'b mut Context) -> Vec<Instruction<'a>> {
    let mut instructions = Vec::new();

    for block_item in block.0 {
        match block_item {
            parser::BlockItem::Statement(statement) => {
                instructions.extend(gen_statement(statement, context));
            }
            parser::BlockItem::Declaration(declaration) => match declaration {
                parser::Declaration::VariableDeclaration(variable_declaration) => {
                    instructions.extend(gen_variable_declaration(variable_declaration, context))
                }
                parser::Declaration::FunctionDeclaration(function_declaration) => todo!(),
            },
        }
    }

    instructions
}

fn maybe_gen_function<'a, 'b>(
    function: parser::FunctionDeclaration<'a>,
    context: &'b mut Context,
) -> Option<Function<'a>> {
    let parser::FunctionDeclaration { body, .. } = function;

    if let Some(body) = body {
        let mut instructions = Vec::new();
        instructions.extend(gen_block(body, context));
        instructions.push(Instruction::Return(Val::Constant(lexer::Constant("0"))));

        Some(Function {
            name: lexer::Identifier("main"),
            instructions,
        })
    } else {
        None
    }
}

pub fn gen_program<'a, 'b>(program: parser::Program<'a>, context: &'b mut Context) -> Program<'a> {
    let parser::Program(function_declarations) = program;
    let mut functions = Vec::new();

    for function_declaration in function_declarations {
        if let Some(function_declaration) = maybe_gen_function(function_declaration, context) {
            functions.push(function_declaration);
        }
    }

    Program(functions)
}
