use ast::Code;
use eval::eval;
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
        let program = match p.parse_program() {
            Ok(program) => program,
            Err(e) => {
                println!("Parse failed: {}", e);
                continue;
            }
        };

        let evaluated = eval(&program);
        println!("{}", evaluated.inspect());
    }
}
