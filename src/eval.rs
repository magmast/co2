use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Neg, Sub},
};

use anyhow::{Context, Result, anyhow};
use bumpalo::{Bump, collections::Vec};

use crate::ast::{Assign, Expr, Func, If, Param, Stmt, Type};

#[derive(Default)]
pub struct Interpreter {
    bump: Bump,
}

impl Interpreter {
    pub fn eval<'ast, 'input>(
        &self,
        funcs: &'ast Vec<'ast, Func<'ast, 'input>>,
    ) -> Result<&Value<'ast, 'input>> {
        let scope = Scope::new_in(&self.bump);
        scope.eval(funcs)
    }
}

struct Scope<'bump, 'ast, 'input> {
    bump: &'bump Bump,
    parent: Option<&'bump Scope<'bump, 'ast, 'input>>,
    defs: RefCell<HashMap<&'input str, Option<&'bump Value<'ast, 'input>>>>,
}

impl<'bump, 'ast, 'input> Scope<'bump, 'ast, 'input> {
    fn new_in(bump: &'bump Bump) -> &'bump Self {
        bump.alloc(Self {
            bump,
            parent: None,
            defs: RefCell::new(HashMap::new()),
        })
    }

    fn eval(
        &'bump self,
        funcs: &'ast Vec<'ast, Func<'ast, 'input>>,
    ) -> Result<&'bump Value<'ast, 'input>> {
        for func in funcs {
            self.defs
                .borrow_mut()
                .insert(func.ident, Some(self.bump.alloc(Value::Func(func))));
        }

        if let Value::Func(main) = self.get("main")? {
            self.eval_main(main)
        } else {
            Err(anyhow!("Main is not a function"))
        }
    }

    fn eval_main(
        &'bump self,
        func: &'bump Func<'ast, 'input>,
    ) -> Result<&'bump Value<'ast, 'input>> {
        if !func.params.is_empty()
            && func.params
                != [Param {
                    ty: Type::Void,
                    ident: None,
                }]
        {
            return Err(anyhow!("Main function must not take any parameters"));
        }

        let child = self.fork();
        for stmt in &func.body {
            println!("Eval: {stmt:#?}");
            if let Some(ret) = child.eval_stmt(stmt)? {
                return Ok(ret);
            }
        }
        Err(anyhow!("main function did not returned any value"))
    }

    fn eval_stmt(
        &'bump self,
        stmt: &Stmt<'ast, 'input>,
    ) -> Result<Option<&'bump Value<'ast, 'input>>> {
        match stmt {
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(None)
            }
            Stmt::Return(expr) => Ok(Some(self.eval_expr(expr)?)),
            Stmt::Decl(decl) => {
                if let Some(init) = &decl.init {
                    self.defs
                        .borrow_mut()
                        .insert(decl.ident, Some(self.eval_expr(init)?));
                } else {
                    self.defs.borrow_mut().insert(decl.ident, None);
                }
                Ok(None)
            }
            Stmt::If(r#if) => {
                self.eval_if(r#if)?;
                Ok(None)
            }
        }
    }

    fn eval_if(&'bump self, r#if: &If<'ast, 'input>) -> Result<()> {
        let cond = self.eval_expr(&r#if.cond)?;
        if bool::from(cond) {
            let child = self.fork();
            for stmt in &r#if.then {
                child.eval_stmt(stmt)?;
            }
        }
        Ok(())
    }

    fn eval_expr(&'bump self, expr: &Expr<'ast, 'input>) -> Result<&'bump Value<'ast, 'input>> {
        match expr {
            Expr::Ident(ident) => Ok(self
                .defs
                .borrow()
                .get(*ident)
                .with_context(|| format!("{ident} is not defined"))?
                .with_context(|| format!("{ident} is not initialized"))?),
            Expr::Int(int) => Ok(self.bump.alloc(Value::Number(i64::from_str_radix(
                int.value,
                int.radix.into(),
            )?))),
            Expr::Assign(assign) => self.eval_assign(assign),
            Expr::Add(lhs, rhs) => {
                (self.eval_expr(lhs)? + self.eval_expr(rhs)?).map(|value| &*self.bump.alloc(value))
            }
            Expr::Sub(lhs, rhs) => {
                (self.eval_expr(lhs)? - self.eval_expr(rhs)?).map(|value| &*self.bump.alloc(value))
            }
            Expr::Mul(lhs, rhs) => {
                (self.eval_expr(lhs)? * self.eval_expr(rhs)?).map(|value| &*self.bump.alloc(value))
            }
            Expr::Div(lhs, rhs) => {
                (self.eval_expr(lhs)? / self.eval_expr(rhs)?).map(|value| &*self.bump.alloc(value))
            }
            Expr::Neg(expr) => self
                .eval_expr(expr)
                .and_then(|result| -result)
                .map(|value| &*self.bump.alloc(value)),
            Expr::Pos(expr) => self.eval_expr(expr),
        }
    }

    fn eval_assign(
        &'bump self,
        _assign: &Assign<'ast, 'input>,
    ) -> Result<&'bump Value<'ast, 'input>> {
        todo!()
    }

    fn fork(&'bump self) -> &'bump Self {
        self.bump.alloc(Self {
            bump: self.bump,
            defs: RefCell::new(HashMap::new()),
            parent: Some(self),
        })
    }

    fn get(&'bump self, ident: &'input str) -> Result<&'bump Value<'ast, 'input>> {
        if let Some(v) = self.defs.borrow().get(ident).copied() {
            v.with_context(|| format!("{ident} is not initialized"))
        } else {
            self.parent
                .with_context(|| format!("{ident} is not defined"))
                .and_then(|parent| parent.get(ident))
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value<'ast, 'input> {
    Number(i64),
    Func(&'ast Func<'ast, 'input>),
}

impl From<&Value<'_, '_>> for bool {
    fn from(value: &Value<'_, '_>) -> Self {
        match value {
            Value::Number(v) => *v != 0,
            Value::Func(_) => true,
        }
    }
}

impl<'ast, 'input> Add for &Value<'ast, 'input> {
    type Output = Result<Value<'ast, 'input>>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
            _ => Err(anyhow!("Only number addition is allowed")),
        }
    }
}

impl<'ast, 'input> Sub for &Value<'ast, 'input> {
    type Output = Result<Value<'ast, 'input>>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
            _ => Err(anyhow!("Only number subtraction is allowed")),
        }
    }
}

impl<'ast, 'input> Mul for &Value<'ast, 'input> {
    type Output = Result<Value<'ast, 'input>>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
            _ => Err(anyhow!("Only number multiplication is allowed")),
        }
    }
}

impl<'ast, 'input> Div for &Value<'ast, 'input> {
    type Output = Result<Value<'ast, 'input>>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
            _ => Err(anyhow!("Only number division is allowed")),
        }
    }
}

impl<'ast, 'input> Neg for &Value<'ast, 'input> {
    type Output = Result<Value<'ast, 'input>>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(v) => Ok(Value::Number(-v)),
            _ => Err(anyhow!("Only number negation is allowed")),
        }
    }
}

impl Display for Value<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(v) => write!(f, "{v}"),
            Self::Func(v) => {
                write!(f, "{} {}(", v.ty, v.ident)?;
                let mut param_wrote = false;
                for param in &v.params {
                    if param_wrote {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param.ty)?;
                    if let Some(ident) = param.ident {
                        write!(f, " {}", ident)?;
                    }
                    param_wrote = true;
                }
                write!(f, ")")
            }
        }
    }
}
