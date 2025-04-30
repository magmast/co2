use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'arena, 'input> {
    Int(Int<'input>),
    Group(&'arena Group<'arena, 'input>),
    Add(&'arena Add<'arena, 'input>),
    Sub(&'arena Sub<'arena, 'input>),
    Mul(&'arena Mul<'arena, 'input>),
    Div(&'arena Div<'arena, 'input>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Int<'input> {
    pub radix: IntRadix<'input>,
    pub unsigned: bool,
    pub suffix: Option<IntSuffix>,
}

impl<'input> From<IntRadix<'input>> for Int<'input> {
    fn from(value: IntRadix<'input>) -> Self {
        Self {
            radix: value,
            unsigned: false,
            suffix: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntRadix<'input> {
    Bin(&'input str),
    Oct(&'input str),
    Dec(&'input str),
    Hex(&'input str),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntSuffix {
    BitPrecise,
    Long,
    LongLong,
}

impl Display for IntSuffix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::BitPrecise => write!(f, "wb"),
            Self::Long => write!(f, "l"),
            Self::LongLong => write!(f, "ll"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Group<'arena, 'input>(pub Expr<'arena, 'input>);

#[derive(Debug, PartialEq, Clone)]
pub struct Add<'arena, 'input>(pub Expr<'arena, 'input>, pub Expr<'arena, 'input>);

#[derive(Debug, PartialEq, Clone)]
pub struct Sub<'arena, 'input>(pub Expr<'arena, 'input>, pub Expr<'arena, 'input>);

#[derive(Debug, PartialEq, Clone)]
pub struct Mul<'arena, 'input>(pub Expr<'arena, 'input>, pub Expr<'arena, 'input>);

#[derive(Debug, PartialEq, Clone)]
pub struct Div<'arena, 'input>(pub Expr<'arena, 'input>, pub Expr<'arena, 'input>);
