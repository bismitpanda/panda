#![warn(clippy::pedantic, clippy::nursery, clippy::all)]
#![allow(
    clippy::too_many_lines,
    clippy::missing_const_for_fn,
    clippy::cognitive_complexity
)]

mod ast;
mod cmd;
mod code;
mod compiler;
mod eval;
mod formatter;
mod lexer;
mod object;
mod parser;
mod token;
mod vm;

use std::{io::Write, process::exit};

use clap::Parser;
use cmd::{DebugOut, Engine};
use code::instructions_to_string;
use compiler::{symbol_table::SymbolTable, Compiler};
use eval::{eval, Environment};
use object::{builtins::BUILTINS, Object, DIR_ENV_VAR_NAME};
use vm::{VirtualMachine, GLOBAL_SIZE};

fn main() {
    let cli = cmd::Cli::parse();

    match cli.command {
        cmd::Commands::Run(run_args) => {
            std::env::set_var(
                DIR_ENV_VAR_NAME,
                std::fs::canonicalize(&run_args.file_name)
                    .unwrap()
                    .parent()
                    .unwrap()
                    .to_str()
                    .unwrap(),
            );
            eval_file(run_args.file_name, run_args.engine).unwrap();
        }

        cmd::Commands::Repl(repl_args) => {
            std::env::set_var(
                DIR_ENV_VAR_NAME,
                std::env::current_dir().unwrap().to_str().unwrap(),
            );
            start_repl(repl_args.engine).unwrap();
        }

        cmd::Commands::Format(format_args) => {
            match formatter::formatter(&format_args.file_name) {
                Ok(pretty) => {
                    if let Some(out) = format_args.out {
                        std::fs::write(out, pretty).unwrap();
                    } else {
                        println!("{pretty}");
                    }
                }
                Err(err) => println!("{err}"),
            };
        }

        cmd::Commands::Debug(debug_args) => {
            let mut input = String::new();
            if let Some(file_name) = debug_args.file {
                input = std::fs::read_to_string(file_name).unwrap();
            } else {
                std::io::stdin().read_line(&mut input).unwrap();
            }

            let mut lexer = lexer::Lexer::new(&input);
            let mut parser = parser::Parser::new(&mut lexer);

            let program = parser.parse_program().map_or_else(
                || {
                    println!("couldn't parse. returned `None`");
                    exit(0);
                },
                |prog| prog,
            );

            if !parser.errors.is_empty() {
                println!("parser errors:");
                for msg in &parser.errors {
                    println!("\t{msg}");
                }
                return;
            }

            let debug_out = match debug_args.format {
                DebugOut::Ast => format!("{program:#?}"),
                DebugOut::ByteCode => {
                    let mut comp = Compiler::new();
                    if let Err(err) = comp.compile(program) {
                        println!("compiler error:\n\t{err}");
                        return;
                    };

                    instructions_to_string(&comp.byte_code().instructions)
                }

                DebugOut::Stack => {
                    let mut comp = Compiler::new();
                    if let Err(err) = comp.compile(program) {
                        println!("compiler error:\n\t{err}");
                        return;
                    };

                    let mut machine = VirtualMachine::new(&comp.byte_code());
                    if let Err(err) = machine.run() {
                        println!("vm error:\n\t{err}");
                    }

                    format!("{:#?}", machine.get_stack())
                }
            };

            if let Some(path) = debug_args.out_file {
                std::fs::write(path, debug_out).expect("Couldn't write output to file.");
            } else {
                println!("{debug_out}");
            }
        }
    }
}

fn start_repl(engine: Engine) -> std::io::Result<()> {
    println!(
        "Hello {}!, This is Panda Programming Language (v0.1.0)[{}-{}]",
        whoami::username(),
        whoami::arch(),
        whoami::platform()
    );
    println!("Type `exit()` to exit from the repl.");

    match engine {
        Engine::Eval => {
            let mut env = Environment::new();

            loop {
                print!("|> ");
                std::io::stdout().flush()?;
                let mut input = String::new();
                std::io::stdin().read_line(&mut input)?;

                let mut lexer = lexer::Lexer::new(&input);
                let mut parser = parser::Parser::new(&mut lexer);

                let program = parser.parse_program();

                if !parser.errors.is_empty() {
                    println!("parser errors:");
                    for msg in &parser.errors {
                        println!("\t{msg}");
                    }
                    continue;
                }

                let evaluated = eval(program.unwrap(), &mut env);

                if let Some(evaluated) = evaluated {
                    println!("{}", evaluated.inspect());
                }
            }
        }
        Engine::VM => {
            let mut constants = Vec::new();
            let mut globals = Vec::with_capacity(GLOBAL_SIZE);
            let mut symbol_table = SymbolTable::new();

            for (i, (name, _)) in BUILTINS.iter().enumerate() {
                symbol_table.define_builtin(name, i);
            }

            loop {
                print!("|> ");
                std::io::stdout().flush()?;
                let mut input = String::new();
                std::io::stdin().read_line(&mut input)?;

                let mut lexer = lexer::Lexer::new(&input);
                let mut parser = parser::Parser::new(&mut lexer);

                let program = parser.parse_program();

                if !parser.errors.is_empty() {
                    println!("parser errors:");
                    for msg in &parser.errors {
                        println!("\t{msg}");
                    }
                    continue;
                }

                let mut comp = compiler::new_with_state(symbol_table.clone(), &constants);
                if let Err(err) = comp.compile(program.unwrap()) {
                    println!("compiler error:\n\t{err}");
                    continue;
                };
                symbol_table = comp.get_symbol_table();

                let code = comp.byte_code();
                constants = code.constants.clone();

                let mut machine = vm::VirtualMachine::new_with_global_store(&code, &globals);
                if let Err(err) = machine.run() {
                    println!("vm error:\n\t{err}");
                    continue;
                }

                globals = machine.get_globals();

                let stack_top = machine.last_popped_stack_elem;
                if let Some(obj) = stack_top {
                    println!("{}", obj.inspect());
                }
            }
        }
    }
}

fn eval_file(fname: String, engine: Engine) -> std::io::Result<()> {
    let input = std::fs::read_to_string(fname)?;
    let mut env = Environment::new();

    let mut lexer = lexer::Lexer::new(&input);
    let mut parser = parser::Parser::new(&mut lexer);

    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        println!("parser errors:");
        for msg in &parser.errors {
            println!("\t{msg}");
        }
        return Ok(());
    }

    match engine {
        Engine::Eval => {
            let evaluated = eval::eval(program.unwrap(), &mut env);

            if let Some(evaluated) = evaluated {
                if matches!(evaluated, Object::Error { .. }) {
                    println!("{}", evaluated.inspect());
                }
            }
        }

        Engine::VM => {
            let mut comp = compiler::Compiler::new();
            if let Err(err) = comp.compile(program.unwrap()) {
                println!("compiler error:\n\t{err}");
                return Ok(());
            };

            let mut machine = vm::VirtualMachine::new(&comp.byte_code());
            if let Err(err) = machine.run() {
                println!("vm error:\n\t{err}");
                return Ok(());
            }

            let stack_top = machine.stack_top();
            if let Some(top) = stack_top {
                println!("{top}");
            }
        }
    }

    Ok(())
}
