use bumpalo::{Bump, collections::Vec, vec};
use unicode_ident::{is_xid_continue, is_xid_start};
use winnow::{
    ModalResult, Parser, Result,
    ascii::{digit1, hex_digit1, multispace0, oct_digit1},
    combinator::{
        alt, cut_err, delimited, fail, opt, preceded, repeat, separated_pair, terminated,
    },
    error::{ContextError, ErrMode, ParserError, StrContext, StrContextValue},
    stream::{AsChar, Compare, Stream, StreamIsPartial},
    token::{any, one_of, take_while},
};

use crate::ast::{Assign, Decl, Expr, Func, If, Int, IntRadix, IntSuffix, Param, Stmt, Type};

use super::{Call, File};

pub fn file<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, File<'bump, 'input>, ErrMode<ContextError>> {
    repeat(.., func(bump))
        .fold(
            || Vec::new_in(bump),
            |mut acc, func| {
                acc.push(func);
                acc
            },
        )
        .map(|funcs| File { funcs })
        .context(StrContext::Label("file"))
}

fn func<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Func<'bump, 'input>, ErrMode<ContextError>> {
    (
        ty,
        ident,
        delimited(token('('), params(bump), token(')')),
        block(bump),
    )
        .map(|(r#type, ident, params, body)| Func {
            ty: r#type,
            ident,
            params,
            body,
        })
        .context(StrContext::Label("function"))
}

fn params<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Vec<'bump, Param<'input>>, ErrMode<ContextError>> {
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

fn param<'input>(input: &mut &'input str) -> ModalResult<Param<'input>> {
    (ty, opt(ident))
        .map(|(r#type, ident)| Param { ty: r#type, ident })
        .parse_next(input)
}

fn ty(input: &mut &str) -> ModalResult<Type> {
    token(alt(("void".value(Type::Void), "int".value(Type::Int))))
        .context(StrContext::Label("type"))
        .parse_next(input)
}

fn block<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Vec<'bump, Stmt<'bump, 'input>>, ErrMode<ContextError>> {
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
    )
}

fn stmt<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Stmt<'bump, 'input>, ErrMode<ContextError>> {
    StmtParser { bump }
}

struct StmtParser<'bump> {
    bump: &'bump Bump,
}

impl<'bump, 'input> Parser<&'input str, Stmt<'bump, 'input>, ErrMode<ContextError>>
    for StmtParser<'bump>
{
    fn parse_next(
        &mut self,
        input: &mut &'input str,
    ) -> Result<Stmt<'bump, 'input>, ErrMode<ContextError>> {
        alt((
            r#if(self.bump).map(Stmt::If),
            terminated(
                alt((
                    preceded(token("return"), expr(self.bump)).map(Stmt::Return),
                    decl(self.bump).map(Stmt::Decl),
                    expr(self.bump).map(Stmt::Expr),
                )),
                cut_err(token(';'))
                    .context(StrContext::Label("statement termination"))
                    .context(StrContext::Expected(StrContextValue::CharLiteral(';'))),
            ),
        ))
        .parse_next(input)
    }
}

fn decl<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Decl<'bump, 'input>, ErrMode<ContextError>> {
    (ty, ident, opt(preceded(token('='), expr(bump)))).map(|(ty, ident, init)| Decl {
        ty,
        ident,
        init,
    })
}

fn r#if<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, If<'bump, 'input>, ErrMode<ContextError>> {
    preceded(
        token("if"),
        cut_err((
            delimited(token('('), cut_err(expr(bump)), token(')')),
            block_or_stmt(bump).context(StrContext::Label("if body")),
            opt(preceded(
                token("else"),
                cut_err(block_or_stmt(bump)).context(StrContext::Label("else body")),
            )),
        )),
    )
    .map(|(cond, body, r#else)| If {
        cond,
        then: body,
        r#else,
    })
}

fn block_or_stmt<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Vec<'bump, Stmt<'bump, 'input>>, ErrMode<ContextError>> {
    alt((
        block(bump),
        stmt(bump).map(|stmt| vec![in bump; stmt]),
        fail.context(StrContext::Expected(StrContextValue::StringLiteral(
            "block",
        )))
        .context(StrContext::Expected(StrContextValue::StringLiteral(
            "statement",
        ))),
    ))
}

fn expr<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ErrMode<ContextError>> {
    ExprParser { bump }
}

/// Expr parser is a separate structure to eliminate recursive opaque return type.
struct ExprParser<'bump> {
    bump: &'bump Bump,
}

impl<'bump, 'input> Parser<&'input str, Expr<'bump, 'input>, ErrMode<ContextError>>
    for ExprParser<'bump>
{
    fn parse_next(
        &mut self,
        input: &mut &'input str,
    ) -> Result<Expr<'bump, 'input>, ErrMode<ContextError>> {
        alt((
            assign(self.bump).map(|assign| Expr::Assign(self.bump.alloc(assign))),
            equality(self.bump),
        ))
        .context(StrContext::Label("expression"))
        .parse_next(input)
    }
}

fn assign<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Assign<'bump, 'input>, ErrMode<ContextError>> {
    separated_pair(ident, token('='), expr(bump)).map(|(ident, value)| Assign { ident, value })
}

fn equality<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ErrMode<ContextError>> {
    move |input: &mut &'input str| {
        let init = additive(bump).parse_next(input)?;

        repeat(.., (token(alt(("==", "!="))), additive(bump)))
            .fold(
                || init.clone(),
                |lhs, (op, rhs)| match op {
                    "==" => Expr::Eq(bump.alloc(lhs), bump.alloc(rhs)),
                    "!=" => Expr::Ne(bump.alloc(lhs), bump.alloc(rhs)),
                    _ => unreachable!(),
                },
            )
            .parse_next(input)
    }
}

fn additive<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ErrMode<ContextError>> {
    move |input: &mut &'input str| {
        let init = multiplicative(bump).parse_next(input)?;

        repeat(
            ..,
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
) -> impl Parser<&'input str, Expr<'bump, 'input>, ErrMode<ContextError>> {
    move |input: &mut &'input str| {
        let init = unary(bump).parse_next(input)?;

        repeat(0.., (token(one_of(b"*/%")), unary(bump)))
            .fold(
                || init.clone(),
                |lhs, (op, rhs)| match op {
                    '*' => Expr::Mul(bump.alloc(lhs), bump.alloc(rhs)),
                    '/' => Expr::Div(bump.alloc(lhs), bump.alloc(rhs)),
                    '%' => Expr::Mod(bump.alloc(lhs), bump.alloc(rhs)),
                    _ => unreachable!(),
                },
            )
            .parse_next(input)
    }
}

fn unary<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ErrMode<ContextError>> {
    UnaryParser { bump }
}

struct UnaryParser<'bump> {
    bump: &'bump Bump,
}

impl<'bump, 'input> Parser<&'input str, Expr<'bump, 'input>, ErrMode<ContextError>>
    for UnaryParser<'bump>
{
    fn parse_next(
        &mut self,
        input: &mut &'input str,
    ) -> Result<Expr<'bump, 'input>, ErrMode<ContextError>> {
        alt((
            preceded(token('-'), unary(self.bump)).map(|expr| Expr::Neg(self.bump.alloc(expr))),
            preceded(token('+'), unary(self.bump)).map(|expr| Expr::Pos(self.bump.alloc(expr))),
            preceded(
                token('!'),
                unary(self.bump).map(|expr| Expr::Not(self.bump.alloc(expr))),
            ),
            primary(self.bump),
        ))
        .parse_next(input)
    }
}

fn primary<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ErrMode<ContextError>> {
    alt((
        group(bump),
        call(bump).map(Expr::Call),
        int.map(Expr::Int),
        ident.map(Expr::Ident),
        fail.context(StrContext::Label("primary expression")),
    ))
}

fn group<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Expr<'bump, 'input>, ErrMode<ContextError>> {
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

fn call<'bump, 'input: 'bump>(
    bump: &'bump Bump,
) -> impl Parser<&'input str, Call<'bump, 'input>, ErrMode<ContextError>> {
    (
        ident,
        delimited(
            token('('),
            separated(bump, expr(bump), token(',')),
            token(')'),
        ),
    )
        .map(|(ident, params)| Call { ident, params })
}

fn ident<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    token(
        (
            any.verify(|&c: &char| is_xid_start(c)),
            take_while(.., |c: char| is_xid_continue(c)),
        )
            .take(),
    )
    .parse_next(input)
}

fn int<'input>(input: &mut &'input str) -> ModalResult<Int<'input>> {
    token((int_radix, opt(int_suffix)))
        .map(|((radix, value), suffix)| Int {
            radix,
            value,
            suffix,
        })
        .parse_next(input)
}

fn int_radix<'input>(input: &mut &'input str) -> ModalResult<(IntRadix, &'input str)> {
    alt((
        bin_int_radix.map(|v| (IntRadix::Bin, v)),
        hex_int_radix.map(|v| (IntRadix::Hex, v)),
        oct_int_radix.map(|v| (IntRadix::Oct, v)),
        dec_int_radix.map(|v| (IntRadix::Dec, v)),
    ))
    .parse_next(input)
}

fn bin_int_radix<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    base_int_radix((alt(("0b", "0B")), one_of(b"01")), one_of(b"01"))
        .map(|v: &'input str| &v[2..])
        .parse_next(input)
}

fn oct_int_radix<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    base_int_radix('0', oct_digit1).parse_next(input)
}

fn dec_int_radix<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    base_int_radix(one_of(b"123456789"), digit1)
        .take()
        .parse_next(input)
}

fn hex_int_radix<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
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

fn int_suffix(input: &mut &str) -> ModalResult<IntSuffix> {
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

fn separated<I, O, E, So>(
    bump: &Bump,
    mut parser: impl Parser<I, O, E>,
    mut separator: impl Parser<I, So, E>,
) -> impl Parser<I, Vec<O>, E>
where
    I: Stream,
{
    move |input: &mut I| {
        let mut vec = Vec::new_in(bump);
        while let Ok(item) = parser.parse_next(input) {
            vec.push(item);
            if separator.parse_next(input).is_err() {
                break;
            }
        }
        Ok(vec)
    }
}

#[cfg(test)]
mod tests {
    mod int_radix {
        use winnow::Parser;

        use crate::ast::{IntRadix, parser::int_radix};

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

        use crate::ast::parser::bin_int_radix;

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

        use crate::ast::parser::oct_int_radix;

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

        use crate::ast::parser::dec_int_radix;

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

        use crate::ast::parser::hex_int_radix;

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
