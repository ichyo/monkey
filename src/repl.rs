use lexer::Lexer;
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
        let l = Lexer::new(&line);
        for tok in l {
            writeln!(output, "{:?}", tok).unwrap();
        }
    }
}
