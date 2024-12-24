use std::{cmp::min, collections::HashMap};

use crate::{lexer, parser, tacky};

#[derive(Clone, Debug)]
pub enum Register {
    Ax,
    Dx,
    Cx,
    Di,
    Si,
    R8,
    R9,
    R10,
    R11,
}

#[derive(Clone, Debug)]
pub enum OperandSize {
    _1B,
    _8B,
    _4B,
}

#[derive(Clone, Debug)]
pub enum Operand<'a> {
    Imm(lexer::Constant<'a>),
    Register(Register),
    Pseudo(usize),
    Data(String),
    Stack(isize),
}

#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Negate,
    Complement,
}

#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
}

#[derive(Clone, Debug)]
pub enum Condition {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

#[derive(Debug)]
pub enum Instruction<'a> {
    Unary {
        operator: UnaryOperator,
        dst: Operand<'a>,
    },
    Binary {
        operator: BinaryOperator,
        src: Operand<'a>,
        dst: Operand<'a>,
    },
    Cmp {
        src1: Operand<'a>,
        src2: Operand<'a>,
    },
    Move {
        src: Operand<'a>,
        dst: Operand<'a>,
    },
    Idiv {
        operand: Operand<'a>,
    },
    Ret,
    Cdq,
    Jmp {
        target: tacky::Label,
    },
    JmpCC {
        condition: Condition,
        target: tacky::Label,
    },
    SetCC {
        condition: Condition,
        operand: Operand<'a>,
    },
    Label(tacky::Label),
    Push(Operand<'a>),
    AllocateStack(usize),
    DeallocateStack(usize),
    Call {
        name: &'a str,
        plt: bool,
    },
}

#[derive(Debug)]
pub struct Function<'a> {
    name: &'a str,
    instructions: Vec<Instruction<'a>>,
    global: bool,
}

#[derive(Debug)]
pub enum TopLevelItem<'a> {
    Function(Function<'a>),
    StaticVar(parser::StaticVar<'a>),
}

#[derive(Debug)]
pub struct Program<'a>(Vec<TopLevelItem<'a>>);

fn align_stack(size: usize) -> usize {
    ((size + 15) / 16) * 16
}

const ARG_REGISTERS: [Register; 6] = [
    Register::Di,
    Register::Si,
    Register::Dx,
    Register::Cx,
    Register::R8,
    Register::R9,
];

fn no_linkage_static_name_from_index(index: usize) -> String {
    format!("static.{}", index)
}

fn gen_operand(val: tacky::Val) -> Operand {
    match val {
        tacky::Val::Constant(constant) => Operand::Imm(constant),
        tacky::Val::Tmp(index) => Operand::Pseudo(index),
        tacky::Val::Linkage(name) => Operand::Data(name.to_string()),
        tacky::Val::NoLinkage(index) => Operand::Data(no_linkage_static_name_from_index(index)),
    }
}

fn gen_instructions(instructions: Vec<tacky::Instruction>) -> Vec<Instruction> {
    let mut asm_instructions = Vec::new();

    for instruction in instructions {
        match instruction {
            tacky::Instruction::Return(val) => asm_instructions.extend([
                Instruction::Move {
                    src: gen_operand(val),
                    dst: Operand::Register(Register::Ax),
                },
                Instruction::Ret,
            ]),
            tacky::Instruction::Unary { operator, src, dst } => {
                if let tacky::UnaryOperator::Not = operator {
                    let dst = gen_operand(dst);
                    asm_instructions.extend([
                        Instruction::Cmp {
                            src1: Operand::Imm(lexer::Constant("0")),
                            src2: gen_operand(src),
                        },
                        Instruction::Move {
                            src: Operand::Imm(lexer::Constant("0")),
                            dst: dst.clone(),
                        },
                        Instruction::SetCC {
                            condition: Condition::E,
                            operand: dst,
                        },
                    ]);
                } else {
                    let unary_operator = match operator {
                        tacky::UnaryOperator::Complement => Some(UnaryOperator::Complement),
                        tacky::UnaryOperator::Negate => Some(UnaryOperator::Negate),
                        _ => None,
                    };

                    if let Some(unary_operator) = unary_operator {
                        let dst = gen_operand(dst);
                        asm_instructions.extend([
                            Instruction::Move {
                                src: gen_operand(src),
                                dst: dst.clone(),
                            },
                            Instruction::Unary {
                                operator: unary_operator,
                                dst,
                            },
                        ]);
                    } else {
                        unimplemented!()
                    }
                }
            }
            tacky::Instruction::Binary {
                operator,
                src1,
                src2,
                dst,
            } => {
                if let tacky::BinaryOperator::Div = operator {
                    asm_instructions.extend([
                        Instruction::Move {
                            src: gen_operand(src1),
                            dst: Operand::Register(Register::Ax),
                        },
                        Instruction::Cdq,
                        Instruction::Idiv {
                            operand: gen_operand(src2),
                        },
                        Instruction::Move {
                            src: Operand::Register(Register::Ax),
                            dst: gen_operand(dst),
                        },
                    ]);
                } else if let tacky::BinaryOperator::Rem = operator {
                    asm_instructions.extend([
                        Instruction::Move {
                            src: gen_operand(src1),
                            dst: Operand::Register(Register::Ax),
                        },
                        Instruction::Cdq,
                        Instruction::Idiv {
                            operand: gen_operand(src2),
                        },
                        Instruction::Move {
                            src: Operand::Register(Register::Dx),
                            dst: gen_operand(dst),
                        },
                    ]);
                } else {
                    let binary_operator = match operator {
                        tacky::BinaryOperator::Add => Some(BinaryOperator::Add),
                        tacky::BinaryOperator::Sub => Some(BinaryOperator::Sub),
                        tacky::BinaryOperator::Mul => Some(BinaryOperator::Mul),
                        _ => None,
                    };

                    if let Some(binary_operator) = binary_operator {
                        let dst = gen_operand(dst);
                        asm_instructions.extend([
                            Instruction::Move {
                                src: gen_operand(src1),
                                dst: dst.clone(),
                            },
                            Instruction::Binary {
                                operator: binary_operator,
                                src: gen_operand(src2),
                                dst: dst.clone(),
                            },
                        ])
                    } else {
                        let condition = match operator {
                            tacky::BinaryOperator::LessThan => Some(Condition::L),
                            tacky::BinaryOperator::LessThanOrEqual => Some(Condition::LE),
                            tacky::BinaryOperator::GreaterThan => Some(Condition::G),
                            tacky::BinaryOperator::GreaterThanOrEqual => Some(Condition::GE),
                            tacky::BinaryOperator::Equal => Some(Condition::E),
                            tacky::BinaryOperator::NotEqual => Some(Condition::NE),
                            _ => None,
                        };

                        if let Some(condition) = condition {
                            let dst = gen_operand(dst);
                            asm_instructions.extend([
                                Instruction::Cmp {
                                    src1: gen_operand(src2),
                                    src2: gen_operand(src1),
                                },
                                Instruction::Move {
                                    src: Operand::Imm(lexer::Constant("0")),
                                    dst: dst.clone(),
                                },
                                Instruction::SetCC {
                                    condition,
                                    operand: dst,
                                },
                            ]);
                        } else {
                            unimplemented!()
                        }
                    }
                }
            }
            tacky::Instruction::Copy { src, dst } => asm_instructions.push(Instruction::Move {
                src: gen_operand(src),
                dst: gen_operand(dst),
            }),
            tacky::Instruction::Jump { target } => {
                asm_instructions.push(Instruction::Jmp { target })
            }
            tacky::Instruction::JumpIfZero { condition, target } => {
                asm_instructions.extend([
                    Instruction::Cmp {
                        src1: Operand::Imm(lexer::Constant("0")),
                        src2: gen_operand(condition),
                    },
                    Instruction::JmpCC {
                        condition: Condition::E,
                        target,
                    },
                ]);
            }
            tacky::Instruction::JumpIfNotZero { condition, target } => {
                asm_instructions.extend([
                    Instruction::Cmp {
                        src1: Operand::Imm(lexer::Constant("0")),
                        src2: gen_operand(condition),
                    },
                    Instruction::JmpCC {
                        condition: Condition::NE,
                        target,
                    },
                ]);
            }
            tacky::Instruction::Label(label) => asm_instructions.push(Instruction::Label(label)),
            tacky::Instruction::Call {
                name,
                args,
                result,
                non_local,
            } => {
                let (register_args, stack_args) =
                    args.split_at(min(ARG_REGISTERS.len(), args.len()));

                let unaligned_stack_size = stack_args.len() * 8;
                let stack_size = align_stack(unaligned_stack_size);
                let stack_padding = stack_size - unaligned_stack_size;

                if stack_padding != 0 {
                    asm_instructions.push(Instruction::AllocateStack(stack_padding));
                }

                for (i, arg) in register_args.iter().enumerate() {
                    let arg = gen_operand(arg.clone());

                    asm_instructions.push(Instruction::Move {
                        src: arg,
                        dst: Operand::Register(ARG_REGISTERS[i].clone()),
                    })
                }

                for arg in stack_args.iter().rev() {
                    let arg = gen_operand(arg.clone());

                    asm_instructions.push(Instruction::Push(arg))
                }

                asm_instructions.push(Instruction::Call {
                    name,
                    plt: non_local,
                });

                if stack_size != 0 {
                    asm_instructions.push(Instruction::DeallocateStack(stack_size));
                };

                let result = gen_operand(result);
                asm_instructions.push(Instruction::Move {
                    src: Operand::Register(Register::Ax),
                    dst: result,
                });
            }
        }
    }

    asm_instructions
}

fn gen_function(function: tacky::Function) -> Function {
    let tacky::Function {
        instructions: function_instructions,
        name,
        params,
        global,
    } = function;
    let mut instructions = Vec::new();

    for (i, param) in params.into_iter().enumerate() {
        let param = gen_operand(param);

        if i < ARG_REGISTERS.len() {
            instructions.push(Instruction::Move {
                src: Operand::Register(ARG_REGISTERS[i].clone()),
                dst: param,
            })
        } else {
            instructions.push(Instruction::Move {
                src: Operand::Stack(((i - ARG_REGISTERS.len()) * 8 + 16) as isize),
                dst: param,
            })
        }
    }

    instructions.extend(gen_instructions(function_instructions));

    Function {
        name,
        instructions,
        global,
    }
}

pub fn gen_program(program: tacky::Program) -> Program {
    let tacky::Program(top_level_items) = program;
    Program(
        top_level_items
            .into_iter()
            .map(|item| match item {
                tacky::TopLevelItem::Function(function) => {
                    TopLevelItem::Function(gen_function(function))
                }
                tacky::TopLevelItem::StaticVar(static_var) => TopLevelItem::StaticVar(static_var),
            })
            .collect(),
    )
}

fn rewrite_operand_to_eliminate_psedo<'a>(
    operand: Operand<'a>,
    stack: &mut HashMap<usize, usize>,
) -> Operand<'a> {
    match operand {
        Operand::Pseudo(index) => {
            let position = if let Some(position) = stack.get(&index) {
                *position
            } else {
                let position = (stack.len() + 1) * 4;
                stack.insert(index, position);
                position
            };

            Operand::Stack(-(position as isize))
        }
        o => o,
    }
}

fn rewrite_function_to_eliminate_psedo(function: Function) -> Function {
    let Function {
        instructions,
        name: func,
        global,
    } = function;
    let mut rewritten_instructions = Vec::new();
    let mut stack = HashMap::new();

    for instruction in instructions {
        rewritten_instructions.push(match instruction {
            Instruction::Unary { operator, dst } => Instruction::Unary {
                operator,
                dst: rewrite_operand_to_eliminate_psedo(dst, &mut stack),
            },
            Instruction::Move { src, dst } => Instruction::Move {
                src: rewrite_operand_to_eliminate_psedo(src, &mut stack),
                dst: rewrite_operand_to_eliminate_psedo(dst, &mut stack),
            },
            Instruction::Binary { src, dst, operator } => Instruction::Binary {
                operator,
                src: rewrite_operand_to_eliminate_psedo(src, &mut stack),
                dst: rewrite_operand_to_eliminate_psedo(dst, &mut stack),
            },
            Instruction::Cmp { src1, src2 } => Instruction::Cmp {
                src1: rewrite_operand_to_eliminate_psedo(src1, &mut stack),
                src2: rewrite_operand_to_eliminate_psedo(src2, &mut stack),
            },
            Instruction::Idiv { operand } => Instruction::Idiv {
                operand: rewrite_operand_to_eliminate_psedo(operand, &mut stack),
            },
            Instruction::SetCC { condition, operand } => Instruction::SetCC {
                condition,
                operand: rewrite_operand_to_eliminate_psedo(operand, &mut stack),
            },
            Instruction::Push(operand) => {
                Instruction::Push(rewrite_operand_to_eliminate_psedo(operand, &mut stack))
            }
            i => i,
        });
    }

    rewritten_instructions.insert(0, Instruction::AllocateStack(align_stack(stack.len() * 4)));

    Function {
        name: func,
        instructions: rewritten_instructions,
        global,
    }
}

pub fn rewrite_program_to_eliminate_psedo(program: Program) -> Program {
    let Program(top_level_items) = program;
    Program(
        top_level_items
            .into_iter()
            .map(|item| match item {
                TopLevelItem::Function(function) => {
                    TopLevelItem::Function(rewrite_function_to_eliminate_psedo(function))
                }
                TopLevelItem::StaticVar(_) => item,
            })
            .collect(),
    )
}

fn rewrite_function_to_fixup_instructions(function: Function) -> Function {
    let Function {
        instructions,
        name: func,
        global,
    } = function;
    let mut rewritten_instructions = Vec::new();

    for instruction in instructions {
        match instruction {
            Instruction::Move { ref src, ref dst } => match (src, dst) {
                (Operand::Stack(_), Operand::Stack(_))
                | (Operand::Data(_), Operand::Data(_))
                | (Operand::Stack(_), Operand::Data(_))
                | (Operand::Data(_), Operand::Stack(_)) => rewritten_instructions.extend([
                    Instruction::Move {
                        src: src.clone(),
                        dst: Operand::Register(Register::R10),
                    },
                    Instruction::Move {
                        src: Operand::Register(Register::R10),
                        dst: dst.clone(),
                    },
                ]),
                _ => rewritten_instructions.push(instruction),
            },

            Instruction::Binary {
                ref src,
                ref dst,
                ref operator,
            } => {
                if let BinaryOperator::Mul = operator {
                    match (src, dst) {
                        (_, Operand::Stack(_)) | (_, Operand::Data(_)) => rewritten_instructions
                            .extend([
                                Instruction::Move {
                                    src: dst.clone(),
                                    dst: Operand::Register(Register::R11),
                                },
                                Instruction::Binary {
                                    operator: BinaryOperator::Mul,
                                    src: src.clone(),
                                    dst: Operand::Register(Register::R11),
                                },
                                Instruction::Move {
                                    src: Operand::Register(Register::R11),
                                    dst: dst.clone(),
                                },
                            ]),
                        _ => rewritten_instructions.push(instruction),
                    }
                } else {
                    match (src, dst) {
                        (Operand::Stack(_), Operand::Stack(_))
                        | (Operand::Data(_), Operand::Data(_))
                        | (Operand::Stack(_), Operand::Data(_))
                        | (Operand::Data(_), Operand::Stack(_)) => rewritten_instructions.extend([
                            Instruction::Move {
                                src: src.clone(),
                                dst: Operand::Register(Register::R10),
                            },
                            Instruction::Binary {
                                operator: operator.clone(),
                                src: Operand::Register(Register::R10),
                                dst: dst.clone(),
                            },
                        ]),
                        _ => rewritten_instructions.push(instruction),
                    }
                }
            }

            Instruction::Cmp { ref src1, ref src2 } => match (src1, src2) {
                (Operand::Stack(_), Operand::Stack(_))
                | (Operand::Data(_), Operand::Data(_))
                | (Operand::Stack(_), Operand::Data(_))
                | (Operand::Data(_), Operand::Stack(_)) => rewritten_instructions.extend([
                    Instruction::Move {
                        src: src1.clone(),
                        dst: Operand::Register(Register::R10),
                    },
                    Instruction::Cmp {
                        src1: Operand::Register(Register::R10),
                        src2: src2.clone(),
                    },
                ]),
                (_, Operand::Imm(_)) => rewritten_instructions.extend([
                    Instruction::Move {
                        src: src2.clone(),
                        dst: Operand::Register(Register::R11),
                    },
                    Instruction::Cmp {
                        src1: src1.clone(),
                        src2: Operand::Register(Register::R11),
                    },
                ]),
                _ => rewritten_instructions.push(instruction),
            },

            Instruction::Idiv { ref operand } => match operand {
                Operand::Imm(_) => rewritten_instructions.extend([
                    Instruction::Move {
                        src: operand.clone(),
                        dst: Operand::Register(Register::R10),
                    },
                    Instruction::Idiv {
                        operand: Operand::Register(Register::R10),
                    },
                ]),
                _ => rewritten_instructions.push(instruction),
            },

            Instruction::Push(ref operand) => match operand {
                Operand::Stack(_) | Operand::Data(_) | Operand::Imm(_) => rewritten_instructions
                    .extend([
                        Instruction::Move {
                            src: operand.clone(),
                            dst: Operand::Register(Register::Ax),
                        },
                        Instruction::Push(Operand::Register(Register::Ax)),
                    ]),
                _ => rewritten_instructions.push(instruction),
            },

            _ => rewritten_instructions.push(instruction),
        }
    }

    Function {
        name: func,
        instructions: rewritten_instructions,
        global,
    }
}

pub fn rewrite_program_to_fixup_instructions(program: Program) -> Program {
    let Program(top_level_items) = program;
    Program(
        top_level_items
            .into_iter()
            .map(|item| match item {
                TopLevelItem::Function(function) => {
                    TopLevelItem::Function(rewrite_function_to_fixup_instructions(function))
                }
                TopLevelItem::StaticVar(_) => item,
            })
            .collect(),
    )
}

#[derive(Clone, Debug)]
pub enum Fragment<'a> {
    Str(&'a str),
    String(String),
}

fn emit_register(register: Register, size: OperandSize) -> Fragment<'static> {
    match size {
        OperandSize::_8B => match register {
            Register::Ax => Fragment::Str("%rax"),
            Register::Dx => Fragment::Str("%rdx"),
            Register::Cx => Fragment::Str("%rcx"),
            Register::Di => Fragment::Str("%rdi"),
            Register::Si => Fragment::Str("%rsi"),
            Register::R8 => Fragment::Str("%r8"),
            Register::R9 => Fragment::Str("%r9"),
            Register::R10 => Fragment::Str("%r10"),
            Register::R11 => Fragment::Str("%r11"),
        },

        OperandSize::_4B => match register {
            Register::Ax => Fragment::Str("%eax"),
            Register::Dx => Fragment::Str("%edx"),
            Register::Cx => Fragment::Str("%ecx"),
            Register::Di => Fragment::Str("%edi"),
            Register::Si => Fragment::Str("%esi"),
            Register::R8 => Fragment::Str("%r8d"),
            Register::R9 => Fragment::Str("%r9d"),
            Register::R10 => Fragment::Str("%r10d"),
            Register::R11 => Fragment::Str("%r11d"),
        },
        OperandSize::_1B => match register {
            Register::Ax => Fragment::Str("%al"),
            Register::Dx => Fragment::Str("%dl"),
            Register::Cx => Fragment::Str("%cl"),
            Register::Di => Fragment::Str("%dil"),
            Register::Si => Fragment::Str("%sil"),
            Register::R8 => Fragment::Str("%r8b"),
            Register::R9 => Fragment::Str("%r9b"),
            Register::R10 => Fragment::Str("%r10b"),
            Register::R11 => Fragment::Str("%r11b"),
        },
    }
}

fn emit_operand(operand: Operand, size: OperandSize) -> Vec<Fragment> {
    let mut text = Vec::new();
    match operand {
        Operand::Imm(constant) => {
            text.push(Fragment::Str("$"));
            text.push(Fragment::Str(constant.0));
        }

        Operand::Register(register) => text.push(emit_register(register, size)),
        Operand::Stack(position) => text.push(Fragment::String(format!("{}(%rbp)", position))),
        Operand::Data(name) => text.push(Fragment::String(format!("{}(%rip)", name))),
        Operand::Pseudo(_) => panic!("Pseudo operand"),
    }

    text
}

fn emit_label(label: tacky::Label) -> Fragment<'static> {
    Fragment::String(format!(".L{}", label.0))
}

fn emit_condition(condition: Condition) -> Fragment<'static> {
    match condition {
        Condition::E => Fragment::Str("e"),
        Condition::NE => Fragment::Str("ne"),
        Condition::G => Fragment::Str("g"),
        Condition::GE => Fragment::Str("ge"),
        Condition::L => Fragment::Str("l"),
        Condition::LE => Fragment::Str("le"),
    }
}

fn emit_unary_mnemonic(operator: UnaryOperator) -> Fragment<'static> {
    match operator {
        UnaryOperator::Negate => Fragment::Str("\tnegl "),
        UnaryOperator::Complement => Fragment::Str("\tnotl "),
    }
}

fn emit_binary_mnemonic(operator: BinaryOperator) -> Fragment<'static> {
    match operator {
        BinaryOperator::Add => Fragment::Str("\taddl "),
        BinaryOperator::Sub => Fragment::Str("\tsubl "),
        BinaryOperator::Mul => Fragment::Str("\timull "),
    }
}

fn emit_instruction(instruction: Instruction) -> Vec<Fragment> {
    let mut text = Vec::new();

    match instruction {
        Instruction::Move { src, dst } => {
            text.push(Fragment::Str("\tmovl "));
            text.extend(emit_operand(src, OperandSize::_4B));
            text.push(Fragment::Str(", "));
            text.extend(emit_operand(dst, OperandSize::_4B));
            text.push(Fragment::Str("\n"));
        }
        Instruction::Ret => {
            text.push(Fragment::Str("\tmovq %rbp, %rsp\n"));
            text.push(Fragment::Str("\tpopq %rbp\n"));
            text.push(Fragment::Str("\tret\n"));
        }
        Instruction::Unary { operator, dst } => {
            text.push(emit_unary_mnemonic(operator));
            text.extend(emit_operand(dst, OperandSize::_4B));
            text.push(Fragment::Str("\n"));
        }
        Instruction::Binary { operator, src, dst } => {
            text.push(emit_binary_mnemonic(operator));
            text.extend(emit_operand(src, OperandSize::_4B));
            text.push(Fragment::Str(", "));
            text.extend(emit_operand(dst, OperandSize::_4B));
            text.push(Fragment::Str("\n"));
        }
        Instruction::Cmp { src1, src2 } => {
            text.push(Fragment::Str("\tcmpl "));
            text.extend(emit_operand(src1, OperandSize::_4B));
            text.push(Fragment::Str(", "));
            text.extend(emit_operand(src2, OperandSize::_4B));
            text.push(Fragment::Str("\n"));
        }
        Instruction::Idiv { operand } => {
            text.push(Fragment::Str("\tidivl "));
            text.extend(emit_operand(operand, OperandSize::_4B));
            text.push(Fragment::Str("\n"));
        }
        Instruction::Cdq => text.push(Fragment::Str("\tcdq\n")),
        Instruction::Jmp { target } => {
            text.push(Fragment::Str("\tjmp "));
            text.push(emit_label(target));
            text.push(Fragment::Str("\n"));
        }
        Instruction::JmpCC { condition, target } => {
            text.push(Fragment::Str("\tj"));
            text.push(emit_condition(condition));
            text.push(Fragment::Str(" "));
            text.push(emit_label(target));
            text.push(Fragment::Str("\n"));
        }
        Instruction::SetCC { condition, operand } => {
            text.push(Fragment::Str("\tset"));
            text.push(emit_condition(condition));
            text.push(Fragment::Str(" "));
            text.extend(emit_operand(operand, OperandSize::_1B));
            text.push(Fragment::Str("\n"));
        }
        Instruction::Label(label) => {
            text.push(emit_label(label));
            text.push(Fragment::Str(":\n"));
        }
        Instruction::Push(operand) => {
            text.push(Fragment::Str("\tpushq "));
            text.extend(emit_operand(operand, OperandSize::_8B));
            text.push(Fragment::Str("\n"));
        }
        Instruction::AllocateStack(stack_size) => {
            text.push(Fragment::String(format!("\tsubq ${}, %rsp\n", stack_size)))
        }
        Instruction::DeallocateStack(stack_size) => {
            text.push(Fragment::String(format!("\taddq ${}, %rsp\n", stack_size)))
        }
        Instruction::Call { name, plt } => {
            text.push(Fragment::Str("\tcall "));
            text.push(Fragment::Str(name));
            if plt {
                text.push(Fragment::Str("@PLT"));
            }
            text.push(Fragment::Str("\n"));
        }
    }

    text
}

fn emit_function(function: Function) -> Vec<Fragment> {
    let Function {
        name,
        instructions,
        global,
    } = function;
    let mut text = Vec::new();

    if global {
        text.push(Fragment::Str("\t.globl "));
        text.push(Fragment::Str(name));
        text.push(Fragment::Str("\n"));
    }

    text.push(Fragment::Str("\t.text\n"));
    text.push(Fragment::Str(name));
    text.push(Fragment::Str(":\n"));

    text.push(Fragment::Str("\tpushq %rbp\n"));
    text.push(Fragment::Str("\tmovq %rsp, %rbp\n"));

    for instruction in instructions {
        text.extend(emit_instruction(instruction));
    }

    text.push(Fragment::Str("\n"));

    text
}

fn emit_static_var(static_var: parser::StaticVar) -> Vec<Fragment> {
    let parser::StaticVar { name, global, init } = static_var;
    let mut text = Vec::new();

    let name = match name {
        parser::StaticVarName::NoLinkage(index) => {
            Fragment::String(no_linkage_static_name_from_index(index))
        }
        parser::StaticVarName::Linkage(name) => Fragment::Str(name),
    };

    if global {
        text.push(Fragment::Str("\t.globl "));
        text.push(name.clone());
        text.push(Fragment::Str("\n"));
    }

    if init == "0" {
        text.push(Fragment::Str("\t.bss\n"));
    } else {
        text.push(Fragment::Str("\t.data\n"));
    }

    text.push(Fragment::Str("\t.align 4\n"));
    text.push(name);
    text.push(Fragment::Str(":\n"));

    if init == "0" {
        text.push(Fragment::Str("\t.zero 4\n"));
    } else {
        text.push(Fragment::Str("\t.long "));
        text.push(Fragment::Str(init));
        text.push(Fragment::Str("\n"));
    }

    text.push(Fragment::Str("\n"));

    text
}

pub fn emit_program(program: Program) -> Vec<Fragment> {
    let mut text = Vec::new();
    let Program(top_level_items) = program;

    for item in top_level_items {
        match item {
            TopLevelItem::Function(function) => text.extend(emit_function(function)),
            TopLevelItem::StaticVar(static_var) => text.extend(emit_static_var(static_var)),
        }
    }
    text.push(Fragment::Str("\n.section .note.GNU-stack,\"\",@progbits\n"));

    text
}
