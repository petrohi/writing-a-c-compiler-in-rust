mod asm;
mod lexer;
mod parser;
mod tacky;

use clap::Parser;
use std::fs::{read_to_string, File};
use std::io::{stdout, Write};
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
    tacky: bool,
    #[clap(long, action)]
    codegen: bool,
}

impl Cli {
    fn do_parse(self: &Self) -> bool {
        !self.lex
    }

    fn do_tacky(self: &Self) -> bool {
        !self.lex && !self.parse
    }

    fn do_codegen(self: &Self) -> bool {
        !self.lex && !self.parse && !self.tacky
    }

    fn do_emit(self: &Self) -> bool {
        !self.lex && !self.parse && !self.tacky && !self.codegen
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
                    dbg!(&tokens);

                    if args.do_parse() {
                        tokens.reverse();
                        let parsed_program = parser::parse_program(&mut tokens);
                        dbg!(&parsed_program);

                        if args.do_tacky() {
                            let mut tacky_context = tacky::Context::new();
                            let tacky_program =
                                tacky::gen_program(parsed_program, &mut tacky_context);
                            dbg!(&tacky_program);

                            if args.do_codegen() {
                                let asm_program = asm::gen_program(tacky_program);
                                dbg!(&asm_program);
                                let asm_program =
                                    asm::rewrite_program_to_eliminate_psedo(asm_program);
                                dbg!(&asm_program);
                                let asm_program =
                                    asm::rewrite_program_to_fixup_instructions(asm_program);
                                dbg!(&asm_program);

                                if args.do_emit() {
                                    let asm_fragments = asm::emit_program(asm_program);

                                    let asm_path = temp_dir
                                        .path()
                                        .join(source_path.file_stem().unwrap())
                                        .with_extension("s");

                                    let asm_file = File::create(&asm_path).unwrap();
                                    write_asm_fragments(&mut &asm_file, &asm_fragments);

                                    let mut stdout = stdout().lock();
                                    write_asm_fragments(&mut stdout, &asm_fragments);

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
                Err(_) => panic!("Cannot read preprocessed source"),
            }
        } else {
            panic!("Preprocessing failed")
        }
    } else {
        panic!("Error creating temp dir")
    }
}
