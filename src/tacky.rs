use crate::parser;

#[derive(Debug, Clone)]
pub enum Val<'a> {
    Constant(parser::Constant<'a>),
    Tmp(usize),
}

#[derive(Debug)]
pub enum Instruction<'a> {
    Unary {
        operator: parser::UnaryOperator,
        src: Val<'a>,
        dst: Val<'a>,
    },
    Binary {
        operator: parser::BinaryOperator,
        src1: Val<'a>,
        src2: Val<'a>,
        dst: Val<'a>,
    },
    Return(Val<'a>),
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
}

impl Context {
    pub fn new() -> Context {
        Context { last_tmp_index: 0 }
    }

    pub fn next_tmp<'a, 'b>(self: &'a mut Self) -> Val<'b> {
        let tmp = Val::Tmp(self.last_tmp_index);
        self.last_tmp_index += 1;
        tmp
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
            let dst = context.next_tmp();
            let (src, mut instructions) = gen_val(*expression, context);
            instructions.push(Instruction::Unary {
                operator,
                src,
                dst: dst.clone(),
            });
            (dst, instructions)
        }
        parser::Expression::Binary {
            operator,
            left,
            right,
        } => {
            let dst = context.next_tmp();
            let (src1, mut src1_instructions) = gen_val(*left, context);
            let (src2, src2_instructions) = gen_val(*right, context);
            src1_instructions.extend(src2_instructions);
            src1_instructions.push(Instruction::Binary {
                operator,
                src1,
                src2,
                dst: dst.clone(),
            });
            (dst, src1_instructions)
        }
    }
}

fn gen_statement<'a>(
    statement: parser::Statement<'a>,
    context: &'a mut Context,
) -> Vec<Instruction<'a>> {
    match statement {
        parser::Statement::Return(expression) => {
            let (val, mut instructions) = gen_val(expression, context);
            instructions.push(Instruction::Return(val));
            instructions
        }
    }
}

fn gen_function<'a>(function: parser::Function<'a>, context: &'a mut Context) -> Function<'a> {
    let parser::Function { body, name } = function;
    Function {
        name,
        instructions: gen_statement(body, context),
    }
}

pub fn gen_program<'a>(program: parser::Program<'a>, context: &'a mut Context) -> Program<'a> {
    let parser::Program(function) = program;
    Program(gen_function(function, context))
}
