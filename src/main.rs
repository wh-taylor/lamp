extern crate rustyline;

use runner::Runner;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use rustyline::Result;

mod tokens;
mod lexer;
mod nodes;
mod parser;
mod manipulator;
mod runner;

fn main() -> Result<()> {
    let mut runner = Runner::new();
    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline("lamp % ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                if let Err(e) = runner.run_text(line) {
                    runner.print_error_under_prompt(e);
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
                break;
            },
            Err(ReadlineError::Eof) => {
                break;
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    Ok(())
}
