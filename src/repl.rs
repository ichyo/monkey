use ast::Code;
use parser::Parser;
use std::io::prelude::*;
use std::io::BufReader;

const PROMPT: &str = ">> ";

pub fn start<R: Read, W: Write>(input: R, mut output: W) {
    let mut reader = BufReader::new(input);
    loop {
        write!(output, "{}", PROMPT).unwrap();
        output.flush().unwrap();
        let mut line = String::new();
        reader.read_line(&mut line).unwrap();
        let mut p = Parser::new(&line);
        match p.parse_program() {
            Ok(program) => println!("{}", program.code()),
            Err(e) => println!("{}", e),
        }
    }
}
