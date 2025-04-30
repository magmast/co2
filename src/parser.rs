use bumpalo::Bump;
use winnow::{
    Parser, Result,
    ascii::{digit1, hex_digit1, multispace0, oct_digit1},
    combinator::{alt, delimited, fail, opt, preceded, repeat, terminated},
    error::{ContextError, ParserError, StrContext, StrContextValue},
    stream::{AsChar, Compare, Stream, StreamIsPartial},
    token::{any, one_of},
};

use crate::ast::{Add, Div, Expr, Group, Int, IntRadix, IntSuffix, Mul, Sub};

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
    token((
        int_radix,
        opt(alt((
            preceded(one_of(b"uU"), int_suffix).map(|suffix| (true, Some(suffix))),
            terminated(int_suffix, one_of(b"uU")).map(|suffix| (true, Some(suffix))),
            int_suffix.map(|suffix| (false, Some(suffix))),
            one_of(b"uU").value((true, None)),
        )))
        .map(|opt| opt.unwrap_or((false, None))),
    ))
    .map(|(radix, (unsigned, suffix))| Int {
        radix,
        unsigned,
        suffix,
    })
    .parse_next(input)
}

fn int_radix<'input>(input: &mut &'input str) -> Result<IntRadix<'input>> {
    alt((
        bin_int_radix.map(IntRadix::Bin),
        hex_int_radix.map(IntRadix::Hex),
        oct_int_radix.map(IntRadix::Oct),
        dec_int_radix.map(IntRadix::Dec),
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
    alt((
        alt(("ll", "LL")).value(IntSuffix::LongLong),
        one_of(b"lL").value(IntSuffix::Long),
        alt(("wb", "WB")).value(IntSuffix::BitPrecise),
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
    mod int {
        use winnow::Parser;

        use crate::{
            ast::{Int, IntRadix, IntSuffix},
            parser::int,
        };

        #[test]
        fn parses_suffixes() {
            let cases = [IntSuffix::Long, IntSuffix::LongLong, IntSuffix::BitPrecise]
                .into_iter()
                .flat_map(|suffix| {
                    [
                        (
                            format!("1{suffix}",),
                            Int {
                                radix: IntRadix::Dec("1"),
                                suffix: Some(suffix.clone()),
                                unsigned: false,
                            },
                        ),
                        (
                            format!("1u{suffix}",),
                            Int {
                                radix: IntRadix::Dec("1"),
                                suffix: Some(suffix.clone()),
                                unsigned: true,
                            },
                        ),
                        (
                            format!("1{suffix}u",),
                            Int {
                                radix: IntRadix::Dec("1"),
                                suffix: Some(suffix.clone()),
                                unsigned: true,
                            },
                        ),
                    ]
                })
                .flat_map(|(input, int)| [(input.to_uppercase(), int.clone()), (input, int)]);

            for (input, value) in cases {
                assert_eq!(int.parse(input.as_str()), Ok(value));
            }
        }
    }

    mod int_radix {
        use winnow::Parser;

        use crate::{ast::IntRadix, parser::int_radix};

        #[test]
        fn parses_all_radixes() {
            assert_eq!(int_radix.parse("0b01"), Ok(IntRadix::Bin("01")));
            assert_eq!(int_radix.parse("01234567"), Ok(IntRadix::Oct("01234567")));
            assert_eq!(
                int_radix.parse("1234567890"),
                Ok(IntRadix::Dec("1234567890"))
            );
            assert_eq!(
                int_radix.parse("0x0123456789abcdefABCDEF"),
                Ok(IntRadix::Hex("0123456789abcdefABCDEF"))
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
