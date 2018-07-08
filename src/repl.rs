use lexer::Lexer;
use std::io::prelude::*;
use std::io::BufReader;
use token::Token;

const PROMPT: &str = ">> ";

pub fn start<R: Read, W: Write>(input: R, mut output: W) {
    let mut reader = BufReader::new(input);
    loop {
        write!(output, "{}", PROMPT).unwrap();
        output.flush().unwrap();
        let mut line = String::new();
        reader.read_line(&mut line).unwrap();
        let mut l = Lexer::new(&line);
        loop {
            let tok = l.next_token();
            writeln!(output, "{:?}", tok).unwrap();
            if tok == Token::Eof {
                break;
            }
        }
    }
}