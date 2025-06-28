use std::{fmt::Write, rc::Rc};

#[derive(Debug, Clone)]
pub enum Sexpr {
    Atom(Rc<str>),
    List(ConsList),
    Nil,
}

impl Sexpr {
    pub fn beta_reduce(&self, bindings: &std::collections::HashMap<&Rc<str>, Sexpr>) -> Sexpr {
        self.map(|sexpr| match sexpr {
            Self::Atom(name) => bindings.get(name).unwrap_or(&sexpr).clone(),
            list @ Self::List(_) => list.beta_reduce(&bindings),
            Self::Nil => Self::Nil,
        })
    }

    pub fn map(&self, mut f: impl FnMut(&Self) -> Self) -> Self {
        match self {
            Self::Atom(_) => f(self),
            Self::List(rc) => {
                let ConsCell { head, tail } = rc.as_ref();
                Sexpr::List(Rc::new(ConsCell {
                    head: f(head),
                    tail: Rc::new(tail.map(f)),
                }))
            }
            Self::Nil => Self::Nil,
        }
    }
}

pub type ConsList = Rc<ConsCell>;

#[derive(Debug, Clone)]
pub struct ConsCell {
    pub head: Sexpr,
    pub tail: Rc<Sexpr>,
}

impl FromIterator<Sexpr> for Sexpr {
    fn from_iter<T: IntoIterator<Item = Sexpr>>(iter: T) -> Self {
        let v: Vec<_> = iter.into_iter().collect();
        let mut sl = v.as_slice();
        let mut cur = Sexpr::Nil;
        while let [head @ .., tail] = sl {
            cur = Sexpr::List(Rc::new(ConsCell {
                head: tail.clone(),
                tail: Rc::new(cur),
            }));

            sl = head;
        }
        cur
    }
}

impl std::fmt::Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let is_list = if let Self::List(_) = self {
            true
        } else {
            false
        };

        if is_list {
            f.write_char('(')?;
        }

        let mut cur = self;
        let mut first = true;

        while let Sexpr::List(rc) = cur {
            if first {
                write!(f, "{}", rc.head)?;
                first = false;
            } else {
                write!(f, " {}", rc.head)?;
            }
            cur = rc.tail.as_ref();
        }

        match cur {
            Self::Atom(s) => write!(f, "{}", s)?,
            // This one should be unreachable
            Self::List(_) => panic!("Exited too early in formatting list"),

            Self::Nil => {
                if !is_list {
                    f.write_str("()")?
                }
            }
        }

        if is_list {
            f.write_char(')')?;
        }

        Ok(())
    }
}

impl IntoIterator for Sexpr {
    type IntoIter = ListIter;
    type Item = Sexpr;

    fn into_iter(self) -> Self::IntoIter {
        ListIter {
            current: Some(self),
        }
    }
}

pub struct ListIter {
    current: Option<Sexpr>,
}

impl Iterator for ListIter {
    type Item = Sexpr;

    fn next(&mut self) -> Option<Self::Item> {
        let cur;
        if let Some(ref current) = self.current {
            cur = current.clone();
        } else {
            return None;
        }

        match cur {
            s @ Sexpr::Atom(_) => {
                self.current = None;
                Some(s)
            }
            Sexpr::List(rc) => {
                let ConsCell { head, tail } = rc.as_ref();
                self.current = Some((**tail).clone());
                Some(head.clone())
            }
            Sexpr::Nil => None,
        }
    }
}
