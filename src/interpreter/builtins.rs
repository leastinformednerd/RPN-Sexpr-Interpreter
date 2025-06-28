use super::{EResult, EvaluationErrKind, EvaluationError, InterpreterState, Value};
use crate::{ConsCell, Sexpr};
use std::collections::HashMap;
use std::rc::Rc;

fn print(state: &mut InterpreterState, sexpr: Sexpr) -> EResult {
    let val = state.reduce(&sexpr);

    let _ = write!(state.writer, "{val}");

    Ok(Value::Sexpr(Sexpr::Nil))
}

fn println(state: &mut InterpreterState, sexpr: Sexpr) -> EResult {
    let val = state.reduce(&sexpr);

    let _ = write!(state.writer, "{val}\n");

    Ok(Value::Sexpr(Sexpr::Nil))
}

fn assoc_int_fold(
    operation: impl Fn(i64, i64) -> i64,
    init: i64,
) -> impl Fn(&mut InterpreterState, Sexpr) -> EResult {
    move |state, sexpr| {
        let val =
            sexpr
                .into_iter()
                .try_fold(init, |acc, sexpr| match state.eval(sexpr.clone())? {
                    Value::Sexpr(Sexpr::Atom(symbol)) => symbol
                        .as_ref()
                        .parse::<i64>()
                        .or_else(|_| {
                            Err(EvaluationError::Local(EvaluationErrKind::TypeError {
                                expected: "Integer".into(),
                                found: Value::Sexpr(Sexpr::Atom(symbol)),
                            }))
                        })
                        .map(|this| operation(acc, this)),
                    _ => Err(EvaluationError::Local(EvaluationErrKind::TypeError {
                        expected: "Integer".into(),
                        found: Value::Sexpr(sexpr),
                    })),
                })?;

        Ok(Value::Sexpr(Sexpr::Atom(val.to_string().into())))
    }
}

fn assoc_int_reduce(
    operation: impl Fn(i64, i64) -> i64 + Clone,
) -> impl Fn(&mut InterpreterState, Sexpr) -> EResult {
    move |state, sexpr| match sexpr {
        Sexpr::List(rc) => {
            let ConsCell { head, tail } = rc.as_ref();

            let init = match state.eval(head.clone())? {
                Value::Sexpr(Sexpr::Atom(symbol)) => {
                    symbol.as_ref().parse::<i64>().or_else(|_| {
                        Err(EvaluationError::Local(EvaluationErrKind::TypeError {
                            expected: "Integer".into(),
                            found: Value::Sexpr(Sexpr::Atom(symbol)),
                        }))
                    })?
                }
                _ => {
                    return Err(EvaluationError::Local(EvaluationErrKind::TypeError {
                        expected: "Integer".into(),
                        found: Value::Sexpr(head.clone()),
                    }));
                }
            };

            assoc_int_fold(operation.clone(), init)(state, tail.as_ref().clone())
        }
        other => Ok(Value::Sexpr(other)),
    }
}

fn value_to_bool(val: Value) -> bool {
    match val {
        Value::Sexpr(Sexpr::Atom(b)) => {
            if b.as_ref() == "false" {
                false
            } else {
                true
            }
        }
        _ => false,
    }
}

fn cond_if(state: &mut InterpreterState, sexpr: Sexpr) -> EResult {
    let mut it = sexpr.into_iter();

    let args = (it.next(), it.next(), it.next(), it.next());

    match args {
        (None, _, _, _) => Err(EvaluationError::Local(
            EvaluationErrKind::ArgumentCountError {
                name: "def".into(),
                expected: 3,
                found: 1,
            },
        )),
        (_, None, _, _) => Err(EvaluationError::Local(
            EvaluationErrKind::ArgumentCountError {
                name: "def".into(),
                expected: 3,
                found: 2,
            },
        )),
        (Some(cond), Some(t_body), Some(f_body), None) => {
            let cond = value_to_bool(state.eval(cond)?);

            if cond {
                state.eval(t_body)
            } else {
                state.eval(f_body)
            }
        }
        _ => Err(EvaluationError::Local(
            EvaluationErrKind::ArgumentCountError {
                name: "def".into(),
                expected: 3,
                found: 4,
            },
        )),
    }
}

fn sexpr_eq(state: &mut InterpreterState, sexpr: Sexpr) -> EResult {
    let mut it = sexpr.into_iter();
    let args = (it.next(), it.next(), it.next());

    match args {
        (None, _, _) => Err(EvaluationError::Local(
            EvaluationErrKind::ArgumentCountError {
                name: "==".into(),
                expected: 2,
                found: 0,
            },
        )),
        (_, None, _) => Err(EvaluationError::Local(
            EvaluationErrKind::ArgumentCountError {
                name: "==".into(),
                expected: 2,
                found: 1,
            },
        )),
        (Some(lhs), Some(rhs), None) => match (state.eval(lhs)?, state.eval(rhs)?) {
            (Value::Sexpr(Sexpr::Atom(lhs)), Value::Sexpr(Sexpr::Atom(rhs))) => {
                if lhs.as_ref() == rhs.as_ref() {
                    Ok(Value::Sexpr(Sexpr::Atom("true".into())))
                } else {
                    Ok(Value::Sexpr(Sexpr::Atom("false".into())))
                }
            }
            (Value::Sexpr(_), Value::Sexpr(_)) => Ok(Value::Sexpr(Sexpr::Atom("false".into()))),
            (Value::Sexpr(_), rhs) => Err(EvaluationError::Local(EvaluationErrKind::TypeError {
                expected: "Atom".into(),
                found: rhs,
            })),
            (lhs, _) => Err(EvaluationError::Local(EvaluationErrKind::TypeError {
                expected: "Atom".into(),
                found: lhs,
            })),
        },
        (_, _, Some(_)) => Err(EvaluationError::Local(
            EvaluationErrKind::ArgumentCountError {
                name: "==".into(),
                expected: 2,
                found: 3,
            },
        )),
    }
}

fn define_user_function(state: &mut InterpreterState, sexpr: Sexpr) -> EResult {
    let mut it = sexpr.into_iter();

    let args = (it.next(), it.next(), it.next(), it.next());

    match args {
        (None, _, _, _) => Err(EvaluationError::Local(
            EvaluationErrKind::ArgumentCountError {
                name: "def".into(),
                expected: 3,
                found: 0,
            },
        )),
        (_, None, _, _) => Err(EvaluationError::Local(
            EvaluationErrKind::ArgumentCountError {
                name: "def".into(),
                expected: 3,
                found: 1,
            },
        )),
        (_, _, None, _) => Err(EvaluationError::Local(
            EvaluationErrKind::ArgumentCountError {
                name: "def".into(),
                expected: 3,
                found: 2,
            },
        )),
        (Some(name), Some(args), Some(body), None) => {
            let name = match state.eval(name)? {
                Value::Sexpr(Sexpr::Atom(name)) => name,
                other => {
                    return Err(EvaluationError::Local(EvaluationErrKind::TypeError {
                        expected: "Atom".into(),
                        found: other,
                    }));
                }
            };

            let args = args
                .into_iter()
                .map(|sexpr| match state.eval(sexpr)? {
                    Value::Sexpr(Sexpr::Atom(param)) => Ok(param),
                    other => {
                        return Err(EvaluationError::Local(EvaluationErrKind::TypeError {
                            expected: "Atom".into(),
                            found: other,
                        }));
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;

            let f = Value::DefinedFn {
                name: name.clone(),
                args: args.into(),
                body,
            };

            state.names.insert(name, f);

            Ok(Value::Sexpr(Sexpr::Nil))
        }
        _ => Err(EvaluationError::Local(
            EvaluationErrKind::ArgumentCountError {
                name: "def".into(),
                expected: 3,
                found: 4,
            },
        )),
    }
}

pub fn builtins() -> HashMap<Rc<str>, Value> {
    let it: [(_, Rc<dyn for<'a> Fn(&'a mut InterpreterState, Sexpr) -> _>); 9] = [
        ("print", Rc::new(print)),
        ("println", Rc::new(println)),
        ("if", Rc::new(cond_if)),
        ("+", Rc::new(assoc_int_fold(std::ops::Add::add, 0))),
        ("*", Rc::new(assoc_int_fold(std::ops::Mul::mul, 1))),
        ("-", Rc::new(assoc_int_reduce(std::ops::Sub::sub))),
        ("/", Rc::new(assoc_int_reduce(std::ops::Div::div))),
        ("==", Rc::new(sexpr_eq)),
        ("def", Rc::new(define_user_function)),
    ];

    let it = it
        .into_iter()
        .map(|(name, f)| (name.into(), Value::NativeFn(name.into(), f)));

    let mut map = HashMap::from_iter(it);

    map.insert("nil".into(), Value::Sexpr(Sexpr::Nil));

    map
}
