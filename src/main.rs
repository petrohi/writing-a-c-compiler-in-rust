use clap::Parser;
use regex::Regex;
use std::fs::{read_to_string, File};
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, ExitStatus};
use tempfile::tempdir;

#[derive(Debug, Parser)]
struct Cli {
    path: String,
    #[clap(long, action)]
    lex: bool,
    #[clap(long, action)]
    parse: bool,
    #[clap(long, action)]
    codegen: bool,
}

impl Cli {
    fn do_parse(self: &Self) -> bool {
        !self.lex
    }

    fn do_codegen(self: &Self) -> bool {
        !self.lex && !self.parse
    }

    fn do_emit(self: &Self) -> bool {
        !self.lex && !self.parse && !self.codegen
    }
}

#[derive(Debug)]
struct Identifier<'a>(&'a str);
#[derive(Debug)]
struct Constant<'a>(&'a str);

#[derive(Debug)]
enum Token<'a> {
    Int,
    Void,
    Return,
    Identifier(Identifier<'a>),
    Constant(Constant<'a>),
    OParen,
    CParen,
    OBrace,
    CBrace,
    Semicolon,
    Minus,
    DoubleMinus,
    Tilde,
}

#[derive(Debug)]
enum UnaryOperator {
    Negate,
    Complement,
}
#[derive(Debug)]
enum CExpression<'a> {
    Constant(Constant<'a>),
    Unary {
        unary_operator: UnaryOperator,
        expression: Box<CExpression<'a>>,
    },
}
#[derive(Debug)]
enum CStatement<'a> {
    Return(CExpression<'a>),
}

#[derive(Debug)]
struct CFunction<'a> {
    name: Identifier<'a>,
    body: CStatement<'a>,
}
#[derive(Debug)]
struct CProgram<'a>(CFunction<'a>);

fn lex(source: &String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut current = &source[..];
    current = current.trim_start();
    let re =
        Regex::new("\\A(?:(?<int>int\\b)|(?<void>void\\b)|(?<return>return\\b)|(?<identifier>[a-zA-Z_]\\w*\\b)|(?<constant>[0-9]+\\b)|(?<o_paren>\\()|(?<c_paren>\\))|(?<o_brace>\\{)|(?<c_brace>\\})|(?<semicolon>;)|(?<minus>-)|(?<double_minus>--)|(?<tilde>~))").unwrap();
    while current.len() != 0 {
        let ca = re.captures(current);

        if let Some(ca) = ca {
            let (token, end) = if let Some(m) = ca.name("int") {
                (Token::Int, m.end())
            } else if let Some(m) = ca.name("void") {
                (Token::Void, m.end())
            } else if let Some(m) = ca.name("return") {
                (Token::Return, m.end())
            } else if let Some(m) = ca.name("identifier") {
                (Token::Identifier(Identifier(m.as_str())), m.end())
            } else if let Some(m) = ca.name("constant") {
                (Token::Constant(Constant(m.as_str())), m.end())
            } else if let Some(m) = ca.name("o_paren") {
                (Token::OParen, m.end())
            } else if let Some(m) = ca.name("c_paren") {
                (Token::CParen, m.end())
            } else if let Some(m) = ca.name("o_brace") {
                (Token::OBrace, m.end())
            } else if let Some(m) = ca.name("c_brace") {
                (Token::CBrace, m.end())
            } else if let Some(m) = ca.name("semicolon") {
                (Token::Semicolon, m.end())
            } else if let Some(m) = ca.name("minus") {
                (Token::Minus, m.end())
            } else if let Some(m) = ca.name("double_minus") {
                (Token::DoubleMinus, m.end())
            } else if let Some(m) = ca.name("tilde") {
                (Token::Tilde, m.end())
            } else {
                panic!()
            };

            tokens.push(token);

            current = &current[end..];
            current = current.trim_start();
        } else {
            panic!();
        }
    }

    tokens
}

fn parse_expression<'a>(tokens: &mut Vec<Token<'a>>) -> CExpression<'a> {
    let token = tokens.pop();
    if let Some(Token::Constant(constant)) = token {
        CExpression::Constant(constant)
    } else if let Some(Token::OParen) = token {
        let expression = parse_expression(tokens);
        if let Some(Token::CParen) = tokens.pop() {
            expression
        } else {
            panic!("Expected )");
        }
    } else if let Some(Token::Minus) = token {
        CExpression::Unary {
            unary_operator: UnaryOperator::Negate,
            expression: Box::new(parse_expression(tokens)),
        }
    } else if let Some(Token::Tilde) = token {
        CExpression::Unary {
            unary_operator: UnaryOperator::Complement,
            expression: Box::new(parse_expression(tokens)),
        }
    } else {
        panic!("Expected constant");
    }
}

fn parse_statement<'a>(tokens: &mut Vec<Token<'a>>) -> CStatement<'a> {
    if let Some(Token::Return) = tokens.pop() {
        let expression = parse_expression(tokens);
        if let Some(Token::Semicolon) = tokens.pop() {
            CStatement::Return(expression)
        } else {
            panic!("Expected ;");
        }
    } else {
        panic!("Expected return");
    }
}

fn parse_function<'a>(tokens: &mut Vec<Token<'a>>) -> CFunction<'a> {
    if let Some(Token::Int) = tokens.pop() {
        if let Some(Token::Identifier(identifier)) = tokens.pop() {
            if let Some(Token::OParen) = tokens.pop() {
                if let Some(Token::Void) = tokens.pop() {
                    if let Some(Token::CParen) = tokens.pop() {
                        if let Some(Token::OBrace) = tokens.pop() {
                            let function = CFunction {
                                body: parse_statement(tokens),
                                name: identifier,
                            };

                            if let Some(Token::CBrace) = tokens.pop() {
                                function
                            } else {
                                panic!("Expected }}");
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
            panic!("Expected identifier");
        }
    } else {
        panic!("Expected int");
    }
}

fn parse_program<'a>(tokens: &mut Vec<Token<'a>>) -> CProgram<'a> {
    let program = CProgram(parse_function(tokens));

    if tokens.is_empty() {
        program
    } else {
        panic!("Unexpected tokens at the end")
    }
}

#[derive(Debug)]
enum AsmRegister {
    EAX,
}

#[derive(Debug)]
enum AsmOperand<'a> {
    Imm(Constant<'a>),
    Register(AsmRegister),
}

#[derive(Debug)]
enum AsmInstruction<'a> {
    Move {
        src: AsmOperand<'a>,
        dst: AsmOperand<'a>,
    },
    Ret,
}

#[derive(Debug)]
struct AsmFunction<'a> {
    name: Identifier<'a>,
    instructions: Vec<AsmInstruction<'a>>,
}
#[derive(Debug)]
struct AsmProgram<'a>(AsmFunction<'a>);

fn codegen_statement(statement: CStatement) -> Vec<AsmInstruction> {
    let mut instructions = Vec::new();
    match statement {
        CStatement::Return(expression) => match expression {
            CExpression::Constant(constant) => instructions.extend([
                AsmInstruction::Move {
                    src: AsmOperand::Imm(constant),
                    dst: AsmOperand::Register(AsmRegister::EAX),
                },
                AsmInstruction::Ret,
            ]),
            CExpression::Unary {
                unary_operator: _unary_operator,
                expression: _expression,
            } => todo!(),
        },
    }

    instructions
}

fn codegen_function(function: CFunction) -> AsmFunction {
    let CFunction { body, name } = function;
    AsmFunction {
        name,
        instructions: codegen_statement(body),
    }
}

fn codegen_program(program: CProgram) -> AsmProgram {
    let CProgram(function) = program;
    AsmProgram(codegen_function(function))
}

fn emit_register(register: AsmRegister) -> &'static str {
    match register {
        AsmRegister::EAX => "%eax",
    }
}

fn emit_operand(operand: AsmOperand) -> Vec<&str> {
    let mut text = Vec::new();
    match operand {
        AsmOperand::Imm(constant) => {
            text.push("$");
            text.push(constant.0);
        }

        AsmOperand::Register(register) => text.push(emit_register(register)),
    }

    text
}

fn emit_instruction(instruction: AsmInstruction) -> Vec<&str> {
    let mut text = Vec::new();
    text.push("\t");

    match instruction {
        AsmInstruction::Move { src, dst } => {
            text.push("movl ");
            text.extend(emit_operand(src));
            text.push(", ");
            text.extend(emit_operand(dst));
            text.push("\n");
        }
        AsmInstruction::Ret => text.push("ret\n"),
    }

    text
}

fn emit_function(function: AsmFunction) -> Vec<&str> {
    let AsmFunction { name, instructions } = function;
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

fn emit_program(program: AsmProgram) -> Vec<&str> {
    let mut text = Vec::new();
    let AsmProgram(function) = program;

    text.extend(emit_function(function));
    text.push("\n.section .note.GNU-stack,\"\",@progbits\n");

    text
}

fn run_gcc<'a, I>(args: I) -> ExitStatus
where
    I: IntoIterator<Item = &'a str>,
{
    if let Ok(status) = Command::new("gcc").args(args).status() {
        status
    } else {
        panic!("GCC not found")
    }
}

fn write_asm_text(path: &PathBuf, text: Vec<&str>) {
    let mut file = File::create(path).unwrap();
    for s in text {
        file.write(s.as_bytes()).unwrap();
    }
}

fn main() {
    let args = Cli::parse();
    let source_path = PathBuf::from(&args.path);

    if let Ok(temp_dir) = tempdir() {
        let preprocessed_path = temp_dir.path().join(source_path.file_name().unwrap());

        if run_gcc([
            "-E",
            "-P",
            source_path.to_str().unwrap(),
            "-o",
            preprocessed_path.to_str().unwrap(),
        ])
        .success()
        {
            match read_to_string(preprocessed_path) {
                Ok(source) => {
                    let mut tokens = lex(&source);
                    dbg!(&tokens);

                    if args.do_parse() {
                        tokens.reverse();
                        let c_program = parse_program(&mut tokens);
                        dbg!(&c_program);

                        if args.do_codegen() {
                            let asm_program = codegen_program(c_program);
                            dbg!(&asm_program);

                            if args.do_emit() {
                                let asm_text = emit_program(asm_program);
                                dbg!(&asm_text);

                                let asm_path = temp_dir
                                    .path()
                                    .join(source_path.file_stem().unwrap())
                                    .with_extension("s");

                                write_asm_text(&asm_path, asm_text);

                                let exe_path = source_path
                                    .parent()
                                    .unwrap()
                                    .join(source_path.file_stem().unwrap());

                                if !run_gcc([
                                    asm_path.to_str().unwrap(),
                                    "-o",
                                    exe_path.to_str().unwrap(),
                                ])
                                .success()
                                {
                                    panic!("Assembly compilation failed")
                                }
                            }
                        }
                    }
                }
                Err(_) => panic!("Cannot read preprocessed source"),
            }
        } else {
            panic!("Preprocessing failed")
        }
    } else {
        panic!("Error creating temp dir")
    }
}
