use std::fmt::{self, Display, Formatter};

use bumpalo::collections::Vec;

#[derive(Debug, PartialEq, Clone)]
pub struct Func<'bump, 'input> {
    pub r#type: Type,
    pub ident: &'input str,
    pub params: Vec<'bump, Param<'input>>,
    pub body: Vec<'bump, Stmt<'bump, 'input>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Void,
    Int,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Int => write!(f, "int"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Param<'input> {
    pub r#type: Type,
    pub ident: Option<&'input str>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'bump, 'input> {
    Expr(Expr<'bump, 'input>),
    Return(Expr<'bump, 'input>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'bump, 'input> {
    Ident(&'input str),
    Int(Int<'input>),
    Add(&'bump Expr<'bump, 'input>, &'bump Expr<'bump, 'input>),
    Sub(&'bump Expr<'bump, 'input>, &'bump Expr<'bump, 'input>),
    Mul(&'bump Expr<'bump, 'input>, &'bump Expr<'bump, 'input>),
    Div(&'bump Expr<'bump, 'input>, &'bump Expr<'bump, 'input>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Int<'input> {
    pub radix: IntRadix,
    pub value: &'input str,
    pub suffix: Option<IntSuffix>,
}

impl Int<'_> {
    pub fn to_u64(&self) -> u64 {
        u64::from_str_radix(self.value, self.radix.into()).unwrap()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IntRadix {
    Bin,
    Oct,
    Dec,
    Hex,
}

impl From<IntRadix> for u32 {
    fn from(val: IntRadix) -> Self {
        match val {
            IntRadix::Bin => 2,
            IntRadix::Oct => 8,
            IntRadix::Dec => 10,
            IntRadix::Hex => 16,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntSuffix {
    Unsigned,
    BitPrecise,
    UnsignedBitPrecise,
    Long,
    UnsignedLong,
    LongLong,
    UnsignedLongLong,
}

impl IntSuffix {
    pub fn to_unsigned(&self) -> Self {
        match self {
            Self::BitPrecise => Self::UnsignedBitPrecise,
            Self::Long => Self::UnsignedLong,
            Self::LongLong => Self::UnsignedLongLong,
            _ => self.clone(),
        }
    }
}
