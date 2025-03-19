mod lexer;
mod parser;
mod codegen;

use clap::Parser;

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
}
