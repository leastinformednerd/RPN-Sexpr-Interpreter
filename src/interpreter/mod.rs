use std::fmt::Debug;
use std::io::Write;
use std::rc::Rc;
use std::{collections::HashMap, fmt::Display};

mod builtins;
use builtins::builtins;

use crate::{ConsCell, Sexpr};

#[derive(Clone)]
pub enum Value {
    Sexpr(Sexpr),
    NativeFn(String, Rc<dyn Fn(&mut InterpreterState, Sexpr) -> EResult>),
    DefinedFn {
        // name is purely here for debug purposes, for interpreter and programs
        name: Rc<str>,
        args: Rc<[Rc<str>]>,
        body: Sexpr,
    },
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sexpr(sexpr) => write!(f, "Sexpr({sexpr})"),
            Self::NativeFn(name, _) => write!(f, "NativeFn({name})"),
            Self::DefinedFn { name, args, body } => {
                write!(
                    f,
                    "DefinedFn{{ name: {name}, args: {args:?}, body: {body:?}}}"
                )
            }
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sexpr(sexpr) => write!(f, "{sexpr}"),
            Self::NativeFn(name, _) => write!(f, "{name}"),
            Self::DefinedFn { name, args, .. } => {
                write!(f, "{name}({args:?})")
            }
        }
    }
}

pub enum EvaluationError {
    Precise {
        kind: EvaluationErrKind,
        origin: Sexpr,
    },
    Local(EvaluationErrKind),
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write(f)
    }
}

impl EvaluationError {
    fn write(&self, fmtr: &mut impl std::fmt::Write) -> std::fmt::Result {
        fmtr.write_str("interpreter error: ")?;
        match self {
            Self::Precise { kind, origin } => write!(fmtr, "{kind} in {origin}"),
            Self::Local(kind) => kind.write(fmtr),
        }
    }
}

pub enum EvaluationErrKind {
    ArgumentCountError {
        name: Rc<str>,
        expected: usize,
        found: usize,
    },
    TypeError {
        expected: String,
        found: Value,
    },
}

impl Display for EvaluationErrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write(f)
    }
}

impl EvaluationErrKind {
    fn write(&self, fmtr: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Self::ArgumentCountError {
                name,
                expected,
                found,
            } => write!(fmtr, "{name} expected {expected} arguments, found {found}"),
            Self::TypeError { expected, found } => write!(
                fmtr,
                "Expected an argument of type {expected}, found {found}"
            ),
        }
    }
}

type EResult = Result<Value, EvaluationError>;

pub struct InterpreterState {
    names: HashMap<Rc<str>, Value>,
    cursor: <Vec<Sexpr> as IntoIterator>::IntoIter,
    writer: Box<dyn std::fmt::Write>,
}

struct StdoutWrapper(std::io::Stdout);

impl std::fmt::Write for StdoutWrapper {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        if self.0.write(s.as_bytes()).is_err() {
            return Err(std::fmt::Error);
        } else {
            return Ok(());
        }
    }
}

fn create_stdout_wrapper_() -> StdoutWrapper {
    StdoutWrapper(std::io::stdout())
}

fn create_stdout_wrapper() -> Box<dyn std::fmt::Write> {
    Box::new(create_stdout_wrapper_())
}

impl InterpreterState {
    pub fn new(prog_roots: Vec<Sexpr>, writer: Option<Box<dyn std::fmt::Write>>) -> Self {
        InterpreterState {
            names: builtins(),
            cursor: prog_roots.into_iter(),
            writer: writer.unwrap_or_else(create_stdout_wrapper),
        }
    }

    pub fn step(&mut self) -> Option<Value> {
        let root_sexpr = self.cursor.next()?;
        let err = match self.eval(root_sexpr) {
            Ok(v) => return Some(v),
            Err(err) => err,
        };

        if let Err(err_log_failure) = err.write(&mut self.writer.as_mut()) {
            panic!("Failed to write error log. Exiting. {err_log_failure:?}")
        };

        None
    }

    fn eval(&mut self, sexpr: Sexpr) -> EResult {
        let res = match sexpr {
            Sexpr::Atom(ref atom) => self.eval_atom(atom),
            Sexpr::List(ref list) => self.eval_list(list.as_ref()),
            Sexpr::Nil => Ok(Value::Sexpr(Sexpr::Nil)),
        };

        match res {
            Err(EvaluationError::Local(kind)) => Err(EvaluationError::Precise {
                kind,
                origin: sexpr,
            }),
            _ => res,
        }
    }

    fn reduce(&mut self, sexpr: &Sexpr) -> Sexpr {
        match sexpr {
            Sexpr::Atom(atom) => match self.names.get(atom.as_ref()) {
                Some(Value::Sexpr(s)) => s.clone(),
                _ => Sexpr::Atom(atom.clone()),
            },
            l @ Sexpr::List(_) => {
                if let Ok(Value::Sexpr(s)) = self.eval(l.clone()) {
                    s
                } else {
                    println!("Didn't eval {l}");
                    l.map(|s| {
                        if let Ok(Value::Sexpr(s)) = self.eval(s.clone()) {
                            s
                        } else {
                            self.reduce(s)
                        }
                    })
                }
            }
            Sexpr::Nil => Sexpr::Nil,
        }
    }

    fn eval_atom(&mut self, atom: &Rc<str>) -> EResult {
        match self.names.get(atom.as_ref()) {
            Some(val) => Ok(val.clone()),
            None => Ok(Value::Sexpr(Sexpr::Atom(atom.clone()))),
        }
    }

    fn eval_list(&mut self, contents: &ConsCell) -> EResult {
        let ConsCell { head, tail } = contents;

        let val = self.eval(head.clone())?;

        self.eval_val_tail(val, tail)
    }

    fn eval_val_tail(&mut self, val: Value, tail: &Rc<Sexpr>) -> EResult {
        if let Sexpr::Nil = tail.as_ref() {
            return Ok(val);
        }

        match val {
            Value::Sexpr(head @ Sexpr::Atom(_)) => {
                Ok(Value::Sexpr(Sexpr::List(Rc::new(ConsCell {
                    head,
                    tail: (*tail).clone(),
                }))))
            }

            Value::Sexpr(head) => {
                let val = self.eval(head)?;
                self.eval_val_tail(val, tail)
            }

            Value::NativeFn(_, f) => f(self, (**tail).clone()),

            Value::DefinedFn { name, args, body } => {
                let passed: Vec<_> = self.reduce(tail.as_ref()).into_iter().collect();

                if passed.len() != args.len() {
                    return Err(EvaluationError::Local(
                        EvaluationErrKind::ArgumentCountError {
                            name,
                            expected: args.len(),
                            found: passed.len(),
                        },
                    ));
                }

                let bindings: HashMap<_, _> = args.into_iter().zip(passed.into_iter()).collect();
                self.eval(body.beta_reduce(&bindings))
            }
        }
    }
}

impl Iterator for InterpreterState {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.step()
    }
}
