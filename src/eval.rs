use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Sub},
};

use anyhow::{Context, Result, anyhow};
use bumpalo::{Bump, collections::Vec};

use crate::ast::{Expr, Func, Param, Stmt, Type};

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
    defs: RefCell<HashMap<&'input str, &'bump Value<'ast, 'input>>>,
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
                .insert(func.ident, self.bump.alloc(Value::Func(func)));
        }

        let main = self.get("main");
        match main {
            Some(Value::Func(main)) => self.eval_main(main),
            Some(_) => Err(anyhow!("Main is not a function")),
            None => Err(anyhow!("No main function")),
        }
    }

    fn eval_main(
        &'bump self,
        func: &'bump Func<'ast, 'input>,
    ) -> Result<&'bump Value<'ast, 'input>> {
        if !func.params.is_empty()
            && func.params
                != [Param {
                    r#type: Type::Void,
                    ident: None,
                }]
        {
            return Err(anyhow!("Main function must not take any parameters"));
        }

        let child = self.fork();
        for stmt in &func.body {
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
        }
    }

    fn eval_expr(&'bump self, expr: &Expr<'ast, 'input>) -> Result<&'bump Value<'ast, 'input>> {
        match expr {
            Expr::Ident(ident) => Ok(self
                .defs
                .borrow()
                .get(*ident)
                .with_context(|| format!("{ident} is not defined"))?),
            Expr::Int(int) => Ok(self.bump.alloc(Value::Number(i64::from_str_radix(
                int.value,
                int.radix.into(),
            )?))),
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
        }
    }

    fn fork(&'bump self) -> &'bump Self {
        self.bump.alloc(Self {
            bump: self.bump,
            defs: RefCell::new(HashMap::new()),
            parent: Some(self),
        })
    }

    fn get(&'bump self, ident: &'input str) -> Option<&'bump Value<'ast, 'input>> {
        self.defs
            .borrow()
            .get(ident)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.get(ident)))
    }
}

#[derive(Debug, Clone)]
pub enum Value<'ast, 'input> {
    Number(i64),
    Func(&'ast Func<'ast, 'input>),
}

impl<'ast, 'input> Add for &Value<'ast, 'input> {
    type Output = Result<Value<'ast, 'input>>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
            _ => Err(anyhow!("Cannot add functions")),
        }
    }
}

impl<'ast, 'input> Sub for &Value<'ast, 'input> {
    type Output = Result<Value<'ast, 'input>>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
            _ => Err(anyhow!("Cannot sub functions")),
        }
    }
}

impl<'ast, 'input> Mul for &Value<'ast, 'input> {
    type Output = Result<Value<'ast, 'input>>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
            _ => Err(anyhow!("Cannot mul functions")),
        }
    }
}

impl<'ast, 'input> Div for &Value<'ast, 'input> {
    type Output = Result<Value<'ast, 'input>>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
            _ => Err(anyhow!("Cannot div functions")),
        }
    }
}

impl Display for Value<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(v) => write!(f, "{v}"),
            Self::Func(v) => {
                write!(f, "{} {}(", v.r#type, v.ident)?;
                let mut param_wrote = false;
                for param in &v.params {
                    if param_wrote {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param.r#type)?;
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
