use std::collections::HashMap;

use crate::parser::{self, Constant, Declaration};

#[derive(Debug, Clone)]
pub enum Val<'a> {
    Constant(parser::Constant<'a>),
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
    pub name: parser::Identifier<'a>,
    pub instructions: Vec<Instruction<'a>>,
}
#[derive(Debug)]
pub struct Program<'a>(pub Function<'a>);

pub struct Context {
    last_tmp_index: usize,
    last_label_index: usize,
    var_to_tmp: HashMap<usize, usize>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            last_tmp_index: 0,
            last_label_index: 0,
            var_to_tmp: HashMap::new(),
        }
    }

    fn next_tmp<'a, 'b>(self: &'a mut Self) -> Val<'b> {
        let tmp = Val::Tmp(self.last_tmp_index);
        self.last_tmp_index += 1;
        tmp
    }

    fn var_tmp<'a, 'b>(self: &'a mut Self, var: usize) -> Val<'b> {
        if let Some(tmp) = self.var_to_tmp.get(&var) {
            Val::Tmp(*tmp)
        } else {
            let tmp = self.last_tmp_index;
            self.var_to_tmp.insert(var, tmp);
            self.last_tmp_index += 1;
            Val::Tmp(tmp)
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
                    src: Val::Constant(Constant("1")),
                    dst: dst.clone(),
                });
                src1_instructions.push(Instruction::Jump {
                    target: end.clone(),
                });
                src1_instructions.push(Instruction::Label(is_false));
                src1_instructions.push(Instruction::Copy {
                    src: Val::Constant(Constant("0")),
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
                    src: Val::Constant(Constant("0")),
                    dst: dst.clone(),
                });
                src1_instructions.push(Instruction::Jump {
                    target: end.clone(),
                });
                src1_instructions.push(Instruction::Label(is_true));
                src1_instructions.push(Instruction::Copy {
                    src: Val::Constant(Constant("1")),
                    dst: dst.clone(),
                });
                src1_instructions.push(Instruction::Label(end));

                (dst, src1_instructions)
            } else {
                unimplemented!()
            }
        }
        parser::Expression::Var(var) => {
            let dst = context.var_tmp(var);
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
        parser::Statement::Break(loop_label) => todo!(),
        parser::Statement::Continue(loop_label) => todo!(),
        parser::Statement::While {
            condition,
            body,
            label,
        } => todo!(),
        parser::Statement::DoWhile {
            body,
            condition,
            label,
        } => todo!(),
        parser::Statement::For {
            for_init,
            condition,
            post,
            body,
            label,
        } => todo!(),
    }
}

fn gen_declaration<'a, 'b>(
    declaration: parser::Declaration<'a>,
    context: &'b mut Context,
) -> Vec<Instruction<'a>> {
    let Declaration { var, expression } = declaration;

    if let Some(expression) = expression {
        let (src, mut instructions) = gen_val(expression, context);
        let dst = context.var_tmp(var);
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
            parser::BlockItem::Declaration(declaration) => {
                instructions.extend(gen_declaration(declaration, context));
            }
        }
    }

    instructions
}

fn gen_function<'a, 'b>(function: parser::Function<'a>, context: &'b mut Context) -> Function<'a> {
    let parser::Function { body, name } = function;

    let mut instructions = Vec::new();
    instructions.extend(gen_block(body, context));
    instructions.push(Instruction::Return(Val::Constant(Constant("0"))));

    Function { name, instructions }
}

pub fn gen_program<'a, 'b>(program: parser::Program<'a>, context: &'b mut Context) -> Program<'a> {
    let parser::Program(function) = program;
    Program(gen_function(function, context))
}
