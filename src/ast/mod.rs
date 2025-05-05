use std::fmt::{self, Display, Formatter};

use bumpalo::{Bump, collections::Vec};
use winnow::{
    Parser,
    error::{ContextError, ParseError},
};

mod parser;

#[derive(Debug, PartialEq, Clone)]
pub struct File<'bump, 'input> {
    pub funcs: Vec<'bump, Func<'bump, 'input>>,
}

impl<'bump, 'input> File<'bump, 'input> {
    pub fn parse(
        bump: &'bump Bump,
        code: &'input str,
    ) -> Result<Self, ParseError<&'input str, ContextError>> {
        parser::file(bump).parse(code)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Func<'bump, 'input> {
    pub ty: Type,
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
    pub ty: Type,
    pub ident: Option<&'input str>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'bump, 'input> {
    Decl(Decl<'bump, 'input>),
    If(If<'bump, 'input>),
    Expr(Expr<'bump, 'input>),
    Return(Expr<'bump, 'input>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Decl<'bump, 'input> {
    pub ty: Type,
    pub ident: &'input str,
    pub init: Option<Expr<'bump, 'input>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct If<'bump, 'input> {
    pub cond: Expr<'bump, 'input>,
    pub then: Vec<'bump, Stmt<'bump, 'input>>,
    pub r#else: Option<Vec<'bump, Stmt<'bump, 'input>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'bump, 'input> {
    Ident(&'input str),
    Int(Int<'input>),
    Call(Call<'bump, 'input>),
    Neg(&'bump Expr<'bump, 'input>),
    Pos(&'bump Expr<'bump, 'input>),
    Mul(&'bump Expr<'bump, 'input>, &'bump Expr<'bump, 'input>),
    Div(&'bump Expr<'bump, 'input>, &'bump Expr<'bump, 'input>),
    Add(&'bump Expr<'bump, 'input>, &'bump Expr<'bump, 'input>),
    Sub(&'bump Expr<'bump, 'input>, &'bump Expr<'bump, 'input>),
    Assign(&'bump Assign<'bump, 'input>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Int<'input> {
    pub radix: IntRadix,
    pub value: &'input str,
    pub suffix: Option<IntSuffix>,
}

impl Int<'_> {
    pub fn to_i32(&self) -> i32 {
        i32::from_str_radix(self.value, self.radix.into()).unwrap()
    }

    pub fn to_i64(&self) -> i64 {
        i64::from_str_radix(self.value, self.radix.into()).unwrap()
    }

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

#[derive(Debug, PartialEq, Clone)]
pub struct Call<'bump, 'input> {
    pub ident: &'input str,
    pub params: Vec<'bump, Expr<'bump, 'input>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign<'bump, 'input> {
    pub ident: &'input str,
    pub value: Expr<'bump, 'input>,
}
