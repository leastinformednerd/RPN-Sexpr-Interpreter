mod interpreter;
mod parsing;

pub mod sexpr;
use interpreter::InterpreterState;
use sexpr::*;

use parsing::parse_unit;
use std::env::args;

fn main() {
    let example = "(def println+1 (n) (println (+ n 1))) (println+1 5)".to_owned();
    let doc = match args().collect::<Vec<_>>()[..] {
        [] => unreachable!("The executable path is expected to be passed as the first argument"),
        [_] => example,
        [_, ref p, ..] => std::fs::read_to_string(p).unwrap(),
    };

    let parse = parse_unit(doc.as_str());

    // println!("{:?}", parse);

    if let Ok((_, roots)) = parse {
        let interpreter = InterpreterState::new(roots, None);
        for _val in interpreter {
            // println!("interpreter value = {val}");
        }
    }
}
