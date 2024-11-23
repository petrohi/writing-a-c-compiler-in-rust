use std::collections::HashMap;

use crate::{
    parser::{self, UnaryOperator},
    tacky,
};

#[derive(Clone, Debug)]
pub enum Register {
    Ax,
    R10,
}

#[derive(Clone, Debug)]
pub enum Operand<'a> {
    Imm(parser::Constant<'a>),
    Register(Register),
    Pseudo(usize),
    Stack(usize),
}

#[derive(Debug)]
pub enum Instruction<'a> {
    Unary {
        operator: parser::UnaryOperator,
        dst: Operand<'a>,
    },
    Move {
        src: Operand<'a>,
        dst: Operand<'a>,
    },
    Ret,
}

#[derive(Debug)]
pub struct Function<'a> {
    name: parser::Identifier<'a>,
    instructions: Vec<Instruction<'a>>,
    stack_size: usize,
}
#[derive(Debug)]
pub struct Program<'a>(Function<'a>);

fn gen_operand(val: tacky::Val) -> Operand {
    match val {
        tacky::Val::Constant(constant) => Operand::Imm(constant),
        tacky::Val::Tmp(index) => Operand::Pseudo(index),
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
                let dst = gen_operand(dst);
                asm_instructions.extend([
                    Instruction::Move {
                        src: gen_operand(src),
                        dst: dst.clone(),
                    },
                    Instruction::Unary { operator, dst },
                ]);
            }
        }
    }

    asm_instructions
}

fn gen_function(function: tacky::Function) -> Function {
    let tacky::Function { instructions, name } = function;
    Function {
        name,
        instructions: gen_instructions(instructions),
        stack_size: 0,
    }
}

pub fn gen_program(program: tacky::Program) -> Program {
    let tacky::Program(function) = program;
    Program(gen_function(function))
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

            Operand::Stack(position)
        }
        o => o,
    }
}

fn rewrite_function_to_eliminate_psedo(function: Function) -> Function {
    let Function {
        instructions, name, ..
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
            i => i,
        });
    }

    Function {
        name,
        instructions: rewritten_instructions,
        stack_size: stack.len() * 4,
    }
}

pub fn rewrite_program_to_eliminate_psedo(program: Program) -> Program {
    let Program(function) = program;
    Program(rewrite_function_to_eliminate_psedo(function))
}

fn rewrite_function_to_fixup_instructions(function: Function) -> Function {
    let Function {
        instructions,
        name,
        stack_size,
    } = function;
    let mut rewritten_instructions = Vec::new();

    for instruction in instructions {
        match instruction {
            Instruction::Move { ref src, ref dst } => match (src, dst) {
                (Operand::Stack(_), Operand::Stack(_)) => rewritten_instructions.extend([
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

            _ => rewritten_instructions.push(instruction),
        }
    }

    Function {
        name,
        instructions: rewritten_instructions,
        stack_size,
    }
}

pub fn rewrite_program_to_fixup_instructions(program: Program) -> Program {
    let Program(function) = program;
    Program(rewrite_function_to_fixup_instructions(function))
}

#[derive(Debug)]
pub enum Fragment<'a> {
    Str(&'a str),
    String(String),
}

fn emit_register(register: Register) -> Fragment<'static> {
    match register {
        Register::Ax => Fragment::Str("%eax"),
        Register::R10 => Fragment::Str("%r10d"),
    }
}

fn emit_operand(operand: Operand) -> Vec<Fragment> {
    let mut text = Vec::new();
    match operand {
        Operand::Imm(constant) => {
            text.push(Fragment::Str("$"));
            text.push(Fragment::Str(constant.0));
        }

        Operand::Register(register) => text.push(emit_register(register)),
        Operand::Stack(position) => text.push(Fragment::String(format!("-{}(%rbp)", position))),
        Operand::Pseudo(_) => panic!("Pseudo operand"),
    }

    text
}

fn emit_unary_mnemonic(operator: parser::UnaryOperator) -> Fragment<'static> {
    match operator {
        UnaryOperator::Negate => Fragment::Str("\tnegl "),
        UnaryOperator::Complement => Fragment::Str("\tnotl "),
    }
}

fn emit_instruction(instruction: Instruction) -> Vec<Fragment> {
    let mut text = Vec::new();

    match instruction {
        Instruction::Move { src, dst } => {
            text.push(Fragment::Str("\tmovl "));
            text.extend(emit_operand(src));
            text.push(Fragment::Str(", "));
            text.extend(emit_operand(dst));
            text.push(Fragment::Str("\n"));
        }
        Instruction::Ret => {
            text.push(Fragment::Str("\tmovq %rbp, %rsp\n"));
            text.push(Fragment::Str("\tpopq %rbp\n"));
            text.push(Fragment::Str("\tret\n"));
        }
        Instruction::Unary { operator, dst } => {
            text.push(emit_unary_mnemonic(operator));
            text.extend(emit_operand(dst));
            text.push(Fragment::Str("\n"));
        }
    }

    text
}

fn emit_function(function: Function) -> Vec<Fragment> {
    let Function {
        name,
        instructions,
        stack_size,
    } = function;
    let mut text = Vec::new();

    text.push(Fragment::Str("\t.globl "));
    text.push(Fragment::Str(name.0));
    text.push(Fragment::Str("\n"));
    text.push(Fragment::Str(name.0));
    text.push(Fragment::Str(":\n"));

    text.push(Fragment::Str("\tpushq %rbp\n"));
    text.push(Fragment::Str("\tmovq %rsp, %rbp\n"));
    text.push(Fragment::String(format!("\tsubq ${}, %rsp\n", stack_size)));

    for instruction in instructions {
        text.extend(emit_instruction(instruction));
    }

    text
}

pub fn emit_program(program: Program) -> Vec<Fragment> {
    let mut text = Vec::new();
    let Program(function) = program;

    text.extend(emit_function(function));
    text.push(Fragment::Str("\n.section .note.GNU-stack,\"\",@progbits\n"));

    text
}
