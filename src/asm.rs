use crate::{parser, tacky};

#[derive(Debug)]
pub enum Register {
    Ax,
}

#[derive(Debug)]
pub enum Operand<'a> {
    Imm(parser::Constant<'a>),
    Register(Register),
}

#[derive(Debug)]
pub enum Instruction<'a> {
    Move { src: Operand<'a>, dst: Operand<'a> },
    Ret,
}

#[derive(Debug)]
pub struct Function<'a> {
    name: parser::Identifier<'a>,
    instructions: Vec<Instruction<'a>>,
}
#[derive(Debug)]
pub struct Program<'a>(Function<'a>);

fn gen_instructions(instructions: Vec<tacky::Instruction>) -> Vec<Instruction> {
    let mut asm_instructions = Vec::new();

    for instruction in instructions {
        match instruction {
            tacky::Instruction::Return(val) => match val {
                tacky::Val::Constant(constant) => asm_instructions.extend([
                    Instruction::Move {
                        src: Operand::Imm(constant),
                        dst: Operand::Register(Register::Ax),
                    },
                    Instruction::Ret,
                ]),
                tacky::Val::Tmp(_index) => todo!(),
            },
            tacky::Instruction::Unary {
                operator: _operator,
                src: _src,
                dst: _dst,
            } => todo!(),
        }
    }

    asm_instructions
}

fn gen_function(function: tacky::Function) -> Function {
    let tacky::Function { instructions, name } = function;
    Function {
        name,
        instructions: gen_instructions(instructions),
    }
}

pub fn gen_program(program: tacky::Program) -> Program {
    let tacky::Program(function) = program;
    Program(gen_function(function))
}

fn emit_register(register: Register) -> &'static str {
    match register {
        Register::Ax => "%eax",
    }
}

fn emit_operand(operand: Operand) -> Vec<&str> {
    let mut text = Vec::new();
    match operand {
        Operand::Imm(constant) => {
            text.push("$");
            text.push(constant.0);
        }

        Operand::Register(register) => text.push(emit_register(register)),
    }

    text
}

fn emit_instruction(instruction: Instruction) -> Vec<&str> {
    let mut text = Vec::new();
    text.push("\t");

    match instruction {
        Instruction::Move { src, dst } => {
            text.push("movl ");
            text.extend(emit_operand(src));
            text.push(", ");
            text.extend(emit_operand(dst));
            text.push("\n");
        }
        Instruction::Ret => text.push("ret\n"),
    }

    text
}

fn emit_function(function: Function) -> Vec<&str> {
    let Function { name, instructions } = function;
    let mut text = Vec::new();

    text.push("\t.globl ");
    text.push(name.0);
    text.push("\n");
    text.push(name.0);
    text.push(":\n");

    for instruction in instructions {
        text.extend(emit_instruction(instruction));
    }

    text
}

pub fn emit_program(program: Program) -> Vec<&str> {
    let mut text = Vec::new();
    let Program(function) = program;

    text.extend(emit_function(function));
    text.push("\n.section .note.GNU-stack,\"\",@progbits\n");

    text
}
