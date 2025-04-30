#[derive(Debug, Clone)]
pub enum Expr<'arena, 'input> {
    Int(Int<'input>),
    Group(&'arena Group<'arena, 'input>),
    Add(&'arena Add<'arena, 'input>),
    Sub(&'arena Sub<'arena, 'input>),
    Mul(&'arena Mul<'arena, 'input>),
    Div(&'arena Div<'arena, 'input>),
}

#[derive(Debug, Clone)]
pub struct Int<'input>(pub &'input str);

#[derive(Debug, Clone)]
pub struct Group<'arena, 'input>(pub Expr<'arena, 'input>);

#[derive(Debug, Clone)]
pub struct Add<'arena, 'input>(pub Expr<'arena, 'input>, pub Expr<'arena, 'input>);

#[derive(Debug, Clone)]
pub struct Sub<'arena, 'input>(pub Expr<'arena, 'input>, pub Expr<'arena, 'input>);

#[derive(Debug, Clone)]
pub struct Mul<'arena, 'input>(pub Expr<'arena, 'input>, pub Expr<'arena, 'input>);

#[derive(Debug, Clone)]
pub struct Div<'arena, 'input>(pub Expr<'arena, 'input>, pub Expr<'arena, 'input>);
