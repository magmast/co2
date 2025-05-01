use bumpalo::{Bump, collections::Vec, vec};
use unicode_ident::{is_xid_continue, is_xid_start};
use winnow::{
    Parser, Result,
    ascii::{digit1, hex_digit1, multispace0, oct_digit1},
    combinator::{alt, delimited, fail, opt, preceded, repeat, terminated},
    error::{ContextError, ParserError, StrContext, StrContextValue},
    stream::{AsChar, Compare, Stream, StreamIsPartial},
    token::{any, one_of, take_while},
};

use crate::ast::{Expr, Func, Int, IntRadix, IntSuffix, Param, Stmt, Type};

pub fn file<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Vec<'bump, Func<'bump, 'input>>, ContextError> {
    repeat(.., func(bump)).fold(
        || Vec::new_in(bump),
        |mut acc, func| {
            acc.push(func);
            acc
        },
    )
}

fn func<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Func<'bump, 'input>, ContextError> {
    (
        r#type,
        ident,
        delimited(token('('), params(bump), token(')')),
        delimited(
            token('{'),
            repeat(.., stmt(bump)).fold(
                || Vec::new_in(bump),
                |mut acc, stmt| {
                    acc.push(stmt);
                    acc
                },
            ),
            token('}'),
        ),
    )
        .map(|(r#type, ident, params, body)| Func {
            r#type,
            ident,
            params,
            body,
        })
}

fn r#type(input: &mut &str) -> Result<Type> {
    token(alt(("void".value(Type::Void), "int".value(Type::Int)))).parse_next(input)
}

fn params<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Vec<'bump, Param<'input>>, ContextError> {
    move |input: &mut &'input str| {
        let init = opt(param).parse_next(input)?;
        if let Some(init) = init {
            repeat(.., preceded(token(','), param))
                .fold(
                    || vec![in bump; init.clone()],
                    |mut acc, param| {
                        acc.push(param);
                        acc
                    },
                )
                .parse_next(input)
        } else {
            Ok(Vec::new_in(bump))
        }
    }
}

fn param<'input>(input: &mut &'input str) -> Result<Param<'input>> {
    (r#type, opt(ident))
        .map(|(r#type, ident)| Param { r#type, ident })
        .parse_next(input)
}

fn stmt<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Stmt<'bump, 'input>, ContextError> {
    terminated(
        alt((
            preceded(token("return"), expr(bump)).map(Stmt::Return),
            expr(bump).map(Stmt::Expr),
        )),
        token(';'),
    )
}

fn expr<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ContextError> {
    ExprParser { bump }
}

/// Expr parser is a separate structure to eliminate recursive opaque return type.
struct ExprParser<'bump> {
    bump: &'bump Bump,
}

impl<'bump, 'input> Parser<&'input str, Expr<'bump, 'input>, ContextError> for ExprParser<'bump> {
    fn parse_next(&mut self, input: &mut &'input str) -> Result<Expr<'bump, 'input>, ContextError> {
        additive(self.bump)
            .context(StrContext::Label("expression"))
            .parse_next(input)
    }
}

fn additive<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ContextError> {
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
                '+' => Expr::Add(bump.alloc(lhs), bump.alloc(rhs)),
                '-' => Expr::Sub(bump.alloc(lhs), bump.alloc(rhs)),
                _ => unreachable!(),
            },
        )
        .parse_next(input)
    }
}

fn multiplicative<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ContextError> {
    move |input: &mut &'input str| {
        let init = primary(bump).parse_next(input)?;

        repeat(0.., (token(one_of(b"*/")), primary(bump)))
            .fold(
                || init.clone(),
                |lhs, (op, rhs)| match op {
                    '*' => Expr::Mul(bump.alloc(lhs), bump.alloc(rhs)),
                    '/' => Expr::Div(bump.alloc(lhs), bump.alloc(rhs)),
                    _ => unreachable!(),
                },
            )
            .parse_next(input)
    }
}

fn primary<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ContextError> {
    alt((
        group(bump),
        int.map(Expr::Int),
        ident.map(Expr::Ident),
        fail.context(StrContext::Label("primary expression")),
    ))
}

fn group<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ContextError> {
    delimited(
        token('(')
            .context(StrContext::Label("group delimiter"))
            .context(StrContext::Expected(StrContextValue::CharLiteral('('))),
        expr(bump),
        token(')')
            .context(StrContext::Label("group delimiter"))
            .context(StrContext::Expected(StrContextValue::CharLiteral(')'))),
    )
}

fn ident<'input>(input: &mut &'input str) -> Result<&'input str> {
    token((
        any.verify(|&c: &char| is_xid_start(c)),
        take_while(.., |c: char| is_xid_continue(c)),
    ))
    .take()
    .parse_next(input)
}

fn int<'input>(input: &mut &'input str) -> Result<Int<'input>> {
    token((int_radix, opt(int_suffix)))
        .map(|((radix, value), suffix)| Int {
            radix,
            value,
            suffix,
        })
        .parse_next(input)
}

fn int_radix<'input>(input: &mut &'input str) -> Result<(IntRadix, &'input str)> {
    alt((
        bin_int_radix.map(|v| (IntRadix::Bin, v)),
        hex_int_radix.map(|v| (IntRadix::Hex, v)),
        oct_int_radix.map(|v| (IntRadix::Oct, v)),
        dec_int_radix.map(|v| (IntRadix::Dec, v)),
    ))
    .parse_next(input)
}

fn bin_int_radix<'input>(input: &mut &'input str) -> Result<&'input str> {
    base_int_radix((alt(("0b", "0B")), one_of(b"01")), one_of(b"01"))
        .map(|v: &'input str| &v[2..])
        .parse_next(input)
}

fn oct_int_radix<'input>(input: &mut &'input str) -> Result<&'input str> {
    base_int_radix('0', oct_digit1).parse_next(input)
}

fn dec_int_radix<'input>(input: &mut &'input str) -> Result<&'input str> {
    base_int_radix(one_of(b"123456789"), digit1)
        .take()
        .parse_next(input)
}

fn hex_int_radix<'input>(input: &mut &'input str) -> Result<&'input str> {
    base_int_radix(
        (alt(("0x", "0X")), any.verify(|c: &char| c.is_hex_digit())),
        hex_digit1,
    )
    .map(|v: &'input str| &v[2..])
    .parse_next(input)
}

fn base_int_radix<I, E, PrefixOutput, DigitOutput>(
    prefix: impl Parser<I, PrefixOutput, E>,
    digit: impl Parser<I, DigitOutput, E>,
) -> impl Parser<I, I::Slice, E>
where
    I: Stream + StreamIsPartial + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    (prefix, repeat::<_, _, (), _, _>(.., (opt('\''), digit))).take()
}

fn int_suffix(input: &mut &str) -> Result<IntSuffix> {
    let inner = || {
        alt((
            alt(("ll", "LL")).value(IntSuffix::LongLong),
            one_of(b"lL").value(IntSuffix::Long),
            alt(("wb", "WB")).value(IntSuffix::BitPrecise),
        ))
    };

    alt((
        alt((
            preceded(one_of(b"uU"), inner()),
            terminated(inner(), one_of(b"uU")),
        ))
        .map(|suffix| suffix.to_unsigned()),
        one_of(b"uU").value(IntSuffix::Unsigned),
        inner(),
    ))
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

#[cfg(test)]
mod tests {
    mod int_radix {
        use winnow::Parser;

        use crate::{ast::IntRadix, parser::int_radix};

        #[test]
        fn parses_all_radixes() {
            assert_eq!(int_radix.parse("0b01"), Ok((IntRadix::Bin, "01")));
            assert_eq!(int_radix.parse("01234567"), Ok((IntRadix::Oct, "01234567")));
            assert_eq!(
                int_radix.parse("1234567890"),
                Ok((IntRadix::Dec, "1234567890"))
            );
            assert_eq!(
                int_radix.parse("0x0123456789abcdefABCDEF"),
                Ok((IntRadix::Hex, "0123456789abcdefABCDEF"))
            );
        }
    }

    mod bin_int {
        use winnow::Parser;

        use crate::parser::bin_int_radix;

        #[test]
        fn valid() {
            let inputs = ["0b0", "0B1", "0b100011110", "0b0'11'1"];
            for input in inputs {
                assert_eq!(bin_int_radix.parse(input), Ok(&input[2..]));
            }
        }

        #[test]
        fn invalid() {
            let inputs = ["2", "0", "0b", "'0b", "0b'", "0b'1", "0b1'"];
            for input in inputs {
                assert!(bin_int_radix.parse(input).is_err());
            }
        }
    }

    mod oct_int {
        use winnow::Parser;

        use crate::parser::oct_int_radix;

        #[test]
        fn valid() {
            let inputs = ["0", "07", "01234567", "0'123", "0'1'2'3", "0'000"];
            for input in inputs {
                assert_eq!(oct_int_radix.parse(input), Ok(input));
            }
        }

        #[test]
        fn invalid() {
            let inputs = ["9", "'123", "123'"];
            for input in inputs {
                assert!(oct_int_radix.parse(input).is_err());
            }
        }
    }

    mod dec_int {
        use winnow::Parser;

        use crate::parser::dec_int_radix;

        #[test]
        fn valid() {
            let inputs = ["1", "123", "1'000", "9'876'543", "1'2'3'4'5'6"];
            for input in inputs {
                assert_eq!(dec_int_radix.parse(input), Ok(input));
            }
        }

        #[test]
        fn invalid() {
            let inputs = ["0123", "'123", "123'"];
            for input in inputs {
                assert!(dec_int_radix.parse(input).is_err());
            }
        }
    }

    mod hex_int {
        use winnow::Parser;

        use crate::parser::hex_int_radix;

        #[test]
        fn valid() {
            let inputs = [
                "0x0",
                "0X0",
                "0x1'0c0",
                "0x9'8D6'543",
                "0x1'2'3'4'5'6'A'a'B'b'F'f",
            ];
            for input in inputs {
                assert_eq!(hex_int_radix.parse(input), Ok(&input[2..]));
            }
        }

        #[test]
        fn invalid() {
            let inputs = ["0x'", "0x'", "0x'3", "0x", "0A", "ABC", "0xg", "0xaa'"];
            for input in inputs {
                assert!(hex_int_radix.parse(input).is_err());
            }
        }
    }
}
