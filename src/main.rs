mod lexer;
mod parser;
mod codegen;

use clap::Parser;
use codegen::codegen::*;

#[derive(Parser, Debug)]
#[command(version, about="Compiler for the AlexC language", long_about = None)]
struct Args {
    // Source file
    #[arg(required=true,help="Source file path")]
    source: String,
    // Output file name
    #[arg(short='o',long="out",default_value="a.s",help="Output file path")]
    out: String,
    // Generate IR
    #[arg(short='i',long="gen-ir",action,help="Generate intermediate representation code")]
    ir: bool
}

fn main() {
    // Parse arguments
    let args = Args::parse();
    // Read source file as string
    let input = match std::fs::read_to_string(args.source) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.to_string());
            std::process::exit(1)
        }
    };
    // Check if in ir mode
    if args.ir {
        // Parse
        let mut p = parser::parser::Parser::new(input);
        let program = match p.parse() {
            Ok(prog) => prog,
            Err(s) => { eprintln!("{}", s); std::process::exit(1) }
        };
        // Generate IR from program
        let ir = match program_to_ir(program) {
            Ok(ir) => ir,
            Err(s) => { eprintln!("{}", s); std::process::exit(1) }
        };
        // Write IR to file
        match ir_to_file(ir.0, args.out) {
            Ok(_) => (),
            Err(e) => {
                eprintln!("{}", e.to_string());
                std::process::exit(1)
            }
        };
    } else {
        // Parse
        let mut p = parser::parser::Parser::new(input);
        let program = match p.parse() {
            Ok(prog) => prog,
            Err(s) => { eprintln!("{}", s); std::process::exit(1) }
        };
        // Generate IR from program
        let ir = match program_to_ir(program) {
            Ok(ir) => ir,
            Err(s) => { eprintln!("{}", s); std::process::exit(1) }
        };
        let x86 = match ir_to_x86(ir.0, ir.1) {
            Ok(x86) => x86,
            Err(s) => { eprintln!("{}", s); std::process::exit(1) }
        };
        // Generate x86 instructions from IR
        match x86_to_file(x86, args.out) {
            Ok(_) => (),
            Err(e) => {
                eprintln!("{}", e.to_string());
                std::process::exit(1)
            }
        };
    }
}
