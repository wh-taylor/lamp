use std::collections::HashMap;

use crate::manipulator::Definitions;
use crate::parser::{Fixity, FixityLayer, Parser, FixityType};
use crate::nodes::{Output, Expr, ParseError};
use crate::tokens::Context;

use std::fs::read_to_string;

pub enum RunnerErrorType {
    InvalidArgCount(usize, Vec<String>),
    NoFilesProvided,
    MissingFile(String),
    AppExpectedMatch(Expr),
    RevAppExpectedMatch(Expr),
    ParseError(ParseError),
    UnknownDefinition(String),
    UnknownCommand(String),
}

impl std::fmt::Display for RunnerErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunnerErrorType::InvalidArgCount(n, args) => write!(f, "Expected {} arguments, received {} ({})", n, args.len(), args.join(", ")),
            RunnerErrorType::NoFilesProvided => write!(f, "No files provided"),
            RunnerErrorType::MissingFile(file_name) => write!(f, "Could not find file with name {}", file_name),
            RunnerErrorType::AppExpectedMatch(expr) => write!(f, "Expected match expression (a => b) after ':>', found {}", expr),
            RunnerErrorType::RevAppExpectedMatch(expr) => write!(f, "Expected match expression (a => b) after ':<', found {}", expr),
            RunnerErrorType::ParseError(ParseError(e, _)) => write!(f, "{}", e),
            RunnerErrorType::UnknownDefinition(s) => write!(f, "Definition {} is not defined", s),
            RunnerErrorType::UnknownCommand(command) => write!(f, "Unknown command \"{}\"", command),
        }
    }
}

pub struct RunnerError(RunnerErrorType, Context);

pub struct Runner {
    pub definitions: Definitions,
    pub def_saves: Vec<Definitions>,
    pub undone_def_saves: Vec<Definitions>,
    pub fixity: Fixity,
}

impl Runner {
    pub fn new() -> Self {
        Runner {
            definitions: HashMap::new(),
            def_saves: Vec::new(),
            undone_def_saves: Vec::new(),
            fixity: Fixity::new(),
        }
    }

    fn use_output(&mut self, output: Output) -> Result<(), RunnerError> {
        match output {
            Output::App { name, expr } => match self.definitions.get(&name) {
                Some(v) => {
                    let def_expr = expr.substitute_definitions(self.definitions.clone());
                    if let Expr::Match(pattern, result) = def_expr {
                        let new_expr = v.substitute_patterns(&pattern, &result);
                        println!("{} :: {}", name, new_expr);
                        self.undone_def_saves.clear();
                        self.def_saves.push(self.definitions.clone());
                        self.definitions.insert(name, new_expr);
                        Ok(())
                    } else {
                        Err(RunnerError(RunnerErrorType::AppExpectedMatch(expr.clone()), expr.get_context()))
                    }
                },
                None => Err(RunnerError(RunnerErrorType::UnknownDefinition(name), expr.get_context())),
            },
            Output::RevApp { name, expr } => match self.definitions.get(&name) {
                Some(v) => {
                    let def_expr = expr.substitute_definitions(self.definitions.clone());
                    if let Expr::Match(pattern, result) = def_expr {
                        let new_expr = v.substitute_patterns(&result, &pattern);
                        println!("{} :: {}", name, new_expr);
                        self.undone_def_saves.clear();
                        self.def_saves.push(self.definitions.clone());
                        self.definitions.insert(name, new_expr);
                        Ok(())
                    } else {
                        Err(RunnerError(RunnerErrorType::RevAppExpectedMatch(expr.clone()), expr.get_context()))
                    }
                },
                None => Err(RunnerError(RunnerErrorType::UnknownDefinition(name), expr.get_context())),
            },
            Output::Def { name, expr } => {
                let def_expr = expr.substitute_definitions(self.definitions.clone());
                println!("{} :: {}", name, def_expr);
                self.undone_def_saves.clear();
                self.def_saves.push(self.definitions.clone());
                self.definitions.insert(name, def_expr.substitute_definitions(self.definitions.clone()));
                Ok(())
            },
            Output::Command { command, args } if command == ":undo" || command == ":u" => {
                Self::expect_n_args(args.clone(), 0)?;
                if let Some(last_def) = self.def_saves.pop() {
                    self.undone_def_saves.push(self.definitions.clone());
                    self.definitions = last_def;
                    println!("Last definition change undone")
                } else {
                    println!("Nothing to undo");
                }
                Ok(())
            },
            Output::Command { command, args } if command == ":redo" || command == ":r" => {
                Self::expect_n_args(args.clone(), 0)?;
                if let Some(redone_def) = self.undone_def_saves.pop() {
                    self.def_saves.push(redone_def.clone());
                    self.definitions = redone_def;
                    println!("Last definition change redone")
                } else {
                    println!("Nothing to redo");
                }
                Ok(())
            },
            Output::Command { command, args } if command == ":import" || command == ":i" => {
                if args.len() == 0 { return Err(RunnerError(RunnerErrorType::NoFilesProvided, Context::empty())); }
                for file_name in args {
                    let Ok(string) = read_to_string(file_name.clone()) else {
                        return Err(RunnerError(RunnerErrorType::MissingFile(file_name), Context::empty()));
                    };
                    let lines = string.lines().filter(|s| !s.chars().all(|c| c.is_whitespace()));
                    for line in lines {
                        let code = line.to_string();
                        if let Err(e) = self.run_text(code) {
                            println!("{}", line);
                            self.print_error(e)
                        }
                    }
                }
                Ok(())
            },
            Output::Command { command, args } if command == ":circumfix" => {
                Self::expect_n_args(args.clone(), 2)?;

                let l_operator = args.get(0).unwrap();
                let r_operator = args.get(1).unwrap();
                self.fixity.circumfix.push((l_operator.to_string(), r_operator.to_string()));
                Ok(())
            },
            Output::Command { command, args } if command == ":infix" || command == ":infixl" || command == ":infixr" || command == ":prefix" || command == ":postfix" => {
                Self::expect_n_args(args.clone(), 2)?;

                let fixity_type = match command.as_str() {
                    ":infix" => FixityType::Infix,
                    ":infixl" => FixityType::InfixL,
                    ":infixr" => FixityType::InfixR,
                    ":prefix" => FixityType::Prefix,
                    ":postfix" => FixityType::Postfix,
                    _ => unreachable!(),
                };
                let fixity_layer: usize = args.get(0).unwrap().parse().unwrap();
                let operator: String = args.get(1).unwrap().to_string();

                match self.fixity.layers.get_mut(fixity_layer) {
                    Some(layer) => {
                        layer.add_operator(fixity_type, operator);
                        Ok(())
                    },
                    None => {
                        let current_len = self.fixity.layers.len();
                        for _ in 0..fixity_layer - current_len {
                            self.fixity.layers.push(FixityLayer::new());
                        }
                        let mut last_layer = FixityLayer::new();
                        last_layer.add_operator(fixity_type, operator);
                        self.fixity.layers.push(last_layer);
                        Ok(())
                    },
                }
            },
            Output::Command { command, args } if command == ":quit" || command == ":q" => {
                Self::expect_n_args(args.clone(), 0)?;
                std::process::exit(1);
            },
            Output::Command { command, args } if command == ":browse" || command == ":b" => {
                Self::expect_n_args(args.clone(), 0)?;
                let mut defs = self.definitions.iter().collect::<Vec<_>>();
                defs.sort_by_key(|(s, _)| if *s == "ans" {s.to_lowercase()} else {(0 as char).to_string()});
                for (s, a) in defs {
                    println!("{} :: {}", s, a);
                }
                Ok(())
            },
            Output::Command { command, args } if command == ":fixity" || command == ":f" => {
                Self::expect_n_args(args.clone(), 0)?;
                for (layer, li) in self.fixity.layers.iter().zip(0..) {
                    for op in layer.infix.clone() {
                        println!("infix {} {}", li, op)
                    }
                    for op in layer.infixl.clone() {
                        println!("infixl {} {}", li, op)
                    }
                    for op in layer.infixr.clone() {
                        println!("infixr {} {}", li, op)
                    }
                    for op in layer.prefix.clone() {
                        println!("prefix {} {}", li, op)
                    }
                    for op in layer.postfix.clone() {
                        println!("postfix {} {}", li, op)
                    }
                }
                for (op1, op2) in self.fixity.circumfix.clone() {
                    println!("circumfix {} {}", op1, op2)
                }
                Ok(())
            },
            Output::Command { command, args } if command == ":help" || command == ":h" || command == ":?" => {
                Self::expect_n_args(args.clone(), 0)?;
                println!("");
                println!("-- Commands:");
                println!("   :h | :? | :help         Print help");
                println!("   :q | :quit              Quit REPL");
                println!("   :i | :import            Run the contents of a file throught the REPL");
                println!("   :b | :browse            Print definitions");
                println!("   :f | :fixity            Print operators with their fixity and precedence");
                println!("   :cd <dir>               Change working directory to <dir>");
                println!("   :infix     [num] <op>   Define infix non-associative operator <op>");
                println!("   :infixl    [num] <op>   Define infix left-associative operator (_ <op> _)");
                println!("   :infixr    [num] <op>   Define infix right-associative operator (_ <op> _)");
                println!("   :prefix    [num] <op>   Define prefix operator (<op> _)");
                println!("   :postfix   [num] <op>   Define postfix operator (_ <op>)");
                println!("   :circumfix <op1> <op2>  Define circumfix operator (<op1> _ <op2>)");
                println!("");
                Ok(())
            },
            Output::Command { command, args: _ } => {
                return Err(RunnerError(RunnerErrorType::UnknownCommand(command), Context::empty()));
            },
        }
    }

    fn expect_n_args(args: Vec<String>, n: usize) -> Result<(), RunnerError> {
        if args.len() != n {
            Err(RunnerError(RunnerErrorType::InvalidArgCount(n, args), Context::empty()))
        } else {
            Ok(())
        }
    }

    fn text_to_output(&mut self, code: String) -> Result<Output, RunnerError> {
        match Parser::new(code, self.fixity.clone()).parse() {
            Err(ParseError(e, c)) => Err(RunnerError(RunnerErrorType::ParseError(ParseError(e, c.clone())), c)),
            Ok(a) => Ok(a),
        }
    }

    pub fn print_error_under_prompt(&self, runner_error: RunnerError) {
        print!("{}", (0..7).map(|_| ' ').collect::<String>());
        self.print_error(runner_error)
    }

    fn print_error(&self, RunnerError(e, c): RunnerError) {
        println!("{}{} {}", (0..c.init).map(|_| ' ').collect::<String>(), (c.init..c.tail+1).map(|_| '^').collect::<String>(), e);
    }

    pub fn run_text(&mut self, code: String) -> Result<(), RunnerError> {
        let output = self.text_to_output(code)?;
        self.use_output(output)
    }
}
