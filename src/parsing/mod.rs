use crate::Sexpr;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::take_till1,
    character::complete::{char, multispace0, multispace1},
    combinator::cut,
    error::ParseError,
    multi::{many0, separated_list0},
    sequence::delimited,
};

pub fn parse_unit(text: &str) -> IResult<&str, Vec<Sexpr>> {
    many0(ws(sexpr)).parse(text)
}

fn sexpr(text: &str) -> IResult<&str, Sexpr> {
    alt((list, atom)).parse(text)
}

fn list(text: &str) -> IResult<&str, Sexpr> {
    let res = delimited(
        (char('('), multispace0),
        cut(separated_list0(multispace1, sexpr)),
        (multispace0, char(')')),
    );

    let res = res.map(|v| v.into_iter().collect()).parse(text);

    res
}

fn atom(text: &str) -> IResult<&str, Sexpr> {
    take_till1(|c: char| c.is_whitespace() || c == '(' || c == ')')
        .map(|s: &str| Sexpr::Atom(s.into()))
        .parse(text)
}

// Lifted fully from the nom recipes page
pub fn ws<'a, O, E: ParseError<&'a str>, F>(inner: F) -> impl Parser<&'a str, Output = O, Error = E>
where
    F: Parser<&'a str, Output = O, Error = E>,
{
    delimited(multispace0, inner, multispace0)
}
