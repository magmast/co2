use bumpalo::Bump;
use winnow::{
    Parser, Result,
    ascii::{digit1, multispace0},
    combinator::{alt, delimited, fail, repeat},
    error::{ContextError, ParserError, StrContext, StrContextValue},
    stream::{AsChar, Stream, StreamIsPartial},
    token::one_of,
};

use crate::ast::{Add, Div, Expr, Group, Int, Mul, Sub};

pub fn expr<'arena, 'input: 'arena>(
    bump: &'arena Bump,
) -> impl Parser<&'input str, Expr<'arena, 'input>, ContextError> {
    ExprParser { bump }
}

/// Expr parser is a separate structure to eliminate recursive opaque return type.
struct ExprParser<'arena> {
    bump: &'arena Bump,
}

impl<'arena, 'input> Parser<&'input str, Expr<'arena, 'input>, ContextError>
    for ExprParser<'arena>
{
    fn parse_next(
        &mut self,
        input: &mut &'input str,
    ) -> Result<Expr<'arena, 'input>, ContextError> {
        additive(self.bump)
            .context(StrContext::Label("expression"))
            .parse_next(input)
    }
}

fn additive<'arena, 'input: 'arena>(
    bump: &'arena Bump,
) -> impl Parser<&'input str, Expr<'arena, 'input>, ContextError> {
    move |input: &mut &'input str| {
        let init = multiplicative(bump).parse_next(input)?;

        repeat(
            0..,
            (
                token(one_of(b"+-"))
                    .context(StrContext::Label("operator"))
                    .context(StrContext::Expected(StrContextValue::CharLiteral('+')))
                    .context(StrContext::Expected(StrContextValue::CharLiteral('-'))),
                multiplicative(bump),
            ),
        )
        .fold(
            || init.clone(),
            |lhs, (op, rhs)| match op {
                '+' => Expr::Add(bump.alloc(Add(lhs, rhs))),
                '-' => Expr::Sub(bump.alloc(Sub(lhs, rhs))),
                _ => unreachable!(),
            },
        )
        .parse_next(input)
    }
}

fn multiplicative<'arena, 'input: 'arena>(
    bump: &'arena Bump,
) -> impl Parser<&'input str, Expr<'arena, 'input>, ContextError> {
    move |input: &mut &'input str| {
        let init = primary(bump).parse_next(input)?;

        repeat(0.., (token(one_of(b"*/")), primary(bump)))
            .fold(
                || init.clone(),
                |lhs, (op, rhs)| match op {
                    '*' => Expr::Mul(bump.alloc(Mul(lhs, rhs))),
                    '/' => Expr::Div(bump.alloc(Div(lhs, rhs))),
                    _ => unreachable!(),
                },
            )
            .parse_next(input)
    }
}

fn primary<'arena, 'input: 'arena>(
    bump: &'arena Bump,
) -> impl Parser<&'input str, Expr<'arena, 'input>, ContextError> {
    alt((
        group(bump).map(|group| Expr::Group(bump.alloc(group))),
        int.map(Expr::Int),
        fail.context(StrContext::Label("primary expression"))
            .context(StrContext::Expected(StrContextValue::StringLiteral(
                "group",
            )))
            .context(StrContext::Expected(StrContextValue::StringLiteral(
                "integer",
            ))),
    ))
}

fn group<'arena, 'input: 'arena>(
    bump: &'arena Bump,
) -> impl Parser<&'input str, Group<'arena, 'input>, ContextError> {
    delimited(
        token('(')
            .context(StrContext::Label("group delimiter"))
            .context(StrContext::Expected(StrContextValue::CharLiteral('('))),
        expr(bump),
        token(')')
            .context(StrContext::Label("group delimiter"))
            .context(StrContext::Expected(StrContextValue::CharLiteral(')'))),
    )
    .map(Group)
}

fn int<'input>(input: &mut &'input str) -> Result<Int<'input>> {
    token(digit1)
        .map(Int)
        .context(StrContext::Label("integer"))
        .context(StrContext::Expected(StrContextValue::StringLiteral(
            "decimal digit",
        )))
        .parse_next(input)
}

fn token<I, O, E>(parser: impl Parser<I, O, E>) -> impl Parser<I, O, E>
where
    I: Stream + StreamIsPartial,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    delimited(multispace0, parser, multispace0)
}
