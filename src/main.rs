mod asm;
mod lexer;
mod parser;
mod tacky;

use clap::Parser;
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
    validate: bool,
    #[clap(long, action)]
    tacky: bool,
    #[clap(long, action)]
    codegen: bool,
    #[clap(short, action)]
    compile_only: bool,
}

impl Cli {
    fn do_parse(&self) -> bool {
        !self.lex
    }

    fn do_validate(&self) -> bool {
        !self.lex && !self.parse
    }

    fn do_tacky(&self) -> bool {
        !self.lex && !self.parse && !self.validate
    }

    fn do_codegen(&self) -> bool {
        !self.lex && !self.parse && !self.validate && !self.tacky
    }

    fn do_emit(&self) -> bool {
        !self.lex && !self.parse && !self.validate && !self.tacky && !self.codegen
    }
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

fn write_asm_fragments(w: &mut impl Write, fragments: &Vec<asm::Fragment>) {
    for fragment in fragments {
        match fragment {
            asm::Fragment::Str(s) => w.write(s.as_bytes()).unwrap(),
            asm::Fragment::String(s) => w.write(s.as_bytes()).unwrap(),
        };
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
                    let mut tokens = lexer::lex(&source);
                    //dbg!(&tokens);

                    if args.do_parse() {
                        tokens.reverse();
                        let mut parser_context = parser::Context::new(args.do_validate());

                        let parsed_program =
                            parser::parse_program(&mut tokens, &mut parser_context);
                        //dbg!(&parsed_program);

                        if args.do_tacky() {
                            let mut tacky_context = tacky::Context::new();
                            let tacky_program = tacky::gen_program(
                                parsed_program,
                                &mut tacky_context,
                                &parser_context,
                            );
                            //dbg!(&tacky_program);

                            if args.do_codegen() {
                                let asm_program = asm::gen_program(tacky_program);
                                //dbg!(&asm_program);
                                let asm_program =
                                    asm::rewrite_program_to_eliminate_psedo(asm_program);
                                //dbg!(&asm_program);
                                let asm_program =
                                    asm::rewrite_program_to_fixup_instructions(asm_program);
                                //dbg!(&asm_program);

                                if args.do_emit() {
                                    let asm_fragments = asm::emit_program(asm_program);

                                    let asm_path = temp_dir
                                        .path()
                                        .join(source_path.file_stem().unwrap())
                                        .with_extension("s");

                                    let asm_file = File::create(&asm_path).unwrap();
                                    write_asm_fragments(&mut &asm_file, &asm_fragments);

                                    if args.compile_only {
                                        let obj_path = source_path
                                            .parent()
                                            .unwrap()
                                            .join(source_path.file_stem().unwrap())
                                            .with_extension("o");

                                        if !run_gcc([
                                            "-c",
                                            asm_path.to_str().unwrap(),
                                            "-o",
                                            obj_path.to_str().unwrap(),
                                        ])
                                        .success()
                                        {
                                            panic!("Assembly compilation failed")
                                        }
                                    } else {
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
