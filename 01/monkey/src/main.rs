extern crate monkey;

use monkey::repl;
use std::env;
use std::io;

fn main() {
    match env::var("USER") {
        Ok(val) => println!("Hello {}! This is the Monkey programming language!", val),
        Err(_) => println!("This is the Monkey programming language!"),
    };
    println!("Feel free to type in commands");

    let stdin = io::stdin();
    let stdout = io::stdout();

    repl::start(stdin.lock(), stdout.lock());
}
