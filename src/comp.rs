use std::{
    collections::HashMap,
    io::{self, Write},
    mem,
    ops::{Deref, DerefMut},
};

use crate::{
    ast::{Assign, Decl, Expr, Func, If, Int, Stmt},
    x64::{Encoder, M32, M64, Reg},
};

pub struct Compiler<'input, W: Write> {
    enc: Encoder<W>,
    scopes: Vec<HashMap<&'input str, i32>>,
    offset: i32,
}

impl<'input, W: Write> Compiler<'input, W> {
    pub fn compile(&mut self, func: &Func<'_, 'input>) -> Result<(), Error> {
        self.func(func)?;
        self.enc.movsxd::<M64>(Reg::Di, Reg::Ax)?;
        self.enc.mov::<M64>(Reg::Ax, 60i32)?;
        self.enc.sys_call()?;
        Ok(())
    }

    fn func(&mut self, func: &Func<'_, 'input>) -> Result<(), Error> {
        self.offset = 0;
        let body_bytes = self.buf(|c| c.block(&func.body))?;
        let frame_size = -self.offset;

        self.enc.push::<M64>(Reg::Bp)?;
        self.enc.mov::<M64>(Reg::Bp, Reg::Sp)?;
        self.enc.sub::<M64>(Reg::Sp, frame_size)?;
        self.enc.write_all(&body_bytes)?;
        self.enc.mov::<M64>(Reg::Sp, Reg::Bp)?;
        self.enc.pop::<M64>(Reg::Bp)?;

        Ok(())
    }

    fn block(&mut self, block: &[Stmt<'_, 'input>]) -> Result<(), Error> {
        let mut scope = self.enter();
        for stmt in block {
            scope.stmt(stmt)?;
        }
        Ok(())
    }

    fn stmt(&mut self, stmt: &Stmt<'_, 'input>) -> Result<(), Error> {
        match stmt {
            Stmt::Decl(decl) => self.decl(decl),
            Stmt::If(r#if) => self.r#if(r#if),
            Stmt::Expr(expr) => self.expr(expr),
            Stmt::Return(expr) => self.expr(expr),
        }
    }

    fn decl(&mut self, decl: &Decl<'_, 'input>) -> Result<(), Error> {
        if let Some(init) = &decl.init {
            self.expr(init)?;
            self.enc.mov::<M32>((Reg::Bp, self.offset), Reg::Ax)?;
            self.scopes
                .last_mut()
                .unwrap()
                .insert(decl.ident, self.offset);
            self.offset -= 4;
        }
        Ok(())
    }

    fn r#if(&mut self, r#if: &If<'_, 'input>) -> Result<(), Error> {
        let else_bytes = r#if
            .r#else
            .as_ref()
            .map(|block| self.buf(|c| c.block(block)))
            .transpose()?;

        let then_bytes = self.buf(|c| {
            c.block(&r#if.then)?;
            if let Some(else_bytes) = &else_bytes {
                c.enc.jmp(else_bytes.len() as i32)?;
            }
            Ok(())
        })?;

        self.expr(&r#if.cond)?;
        self.enc.cmp(0)?;
        self.enc.jz(then_bytes.len() as i32)?;
        self.enc.write_all(&then_bytes)?;
        if let Some(else_bytes) = else_bytes {
            self.enc.write_all(&else_bytes)?;
        }
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> Result<(), Error> {
        match expr {
            Expr::Int(int) => self.int(int),
            Expr::Ident(ident) => self.ident(ident),
            Expr::Call(_) => todo!(),
            Expr::Assign(assign) => self.assign(assign),
            Expr::Add(lhs, rhs) => self.add(lhs, rhs),
            Expr::Sub(lhs, rhs) => self.sub(lhs, rhs),
            Expr::Mul(lhs, rhs) => self.mul(lhs, rhs),
            Expr::Div(lhs, rhs) => self.div(lhs, rhs),
            Expr::Pos(expr) => self.expr(expr),
            Expr::Neg(expr) => self.neg(expr),
        }
    }

    fn int(&mut self, int: &Int) -> Result<(), Error> {
        self.enc.mov::<M32>(Reg::Ax, int.to_i32())?;
        Ok(())
    }

    fn ident(&mut self, ident: &str) -> Result<(), Error> {
        let offset = self.get(ident)?;
        self.enc.mov::<M32>(Reg::Ax, (Reg::Bp, offset))?;
        Ok(())
    }

    fn assign(&mut self, assign: &Assign) -> Result<(), Error> {
        self.expr(&assign.value)?;
        let offset = self.get(assign.ident)?;
        self.enc.mov::<M32>((Reg::Bp, offset), Reg::Ax)?;
        Ok(())
    }

    fn add(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.enc.push::<M32>(Reg::Ax)?;
        self.expr(rhs)?;
        self.enc.pop::<M32>(Reg::Bx)?;
        self.enc.add::<M32>(Reg::Ax, Reg::Bx)?;
        Ok(())
    }

    fn sub(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.enc.push::<M32>(Reg::Ax)?;
        self.expr(rhs)?;
        self.enc.mov::<M32>(Reg::Bx, Reg::Ax)?;
        self.enc.pop::<M32>(Reg::Ax)?;
        self.enc.sub::<M32>(Reg::Ax, Reg::Bx)?;
        Ok(())
    }

    fn mul(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.enc.push::<M32>(Reg::Ax)?;
        self.expr(rhs)?;
        self.enc.pop::<M32>(Reg::Bx)?;
        self.enc.imul::<M32>(Reg::Bx)?;
        Ok(())
    }

    fn div(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.enc.push::<M32>(Reg::Ax)?;
        self.expr(rhs)?;
        self.enc.mov::<M32>(Reg::Bx, Reg::Ax)?;
        self.enc.pop::<M32>(Reg::Ax)?;
        self.enc.mov::<M32>(Reg::Dx, 0)?;
        self.enc.idiv::<M32>(Reg::Bx)?;
        Ok(())
    }

    fn neg(&mut self, expr: &Expr) -> Result<(), Error> {
        self.expr(expr)?;
        self.enc.neg::<M32>(Reg::Ax)?;
        Ok(())
    }

    fn buf(
        &mut self,
        f: impl FnOnce(&mut Compiler<'input, &mut Vec<u8>>) -> Result<(), Error>,
    ) -> Result<Vec<u8>, Error> {
        let mut buf = vec![];
        let mut child = Compiler::from(&mut buf);
        child.scopes = mem::take(&mut self.scopes);
        child.offset = self.offset;
        let result = f(&mut child);
        self.scopes = child.scopes;
        self.offset = child.offset;
        result.and(Ok(buf))
    }

    fn enter(&mut self) -> ScopeGuard<'_, 'input, W> {
        self.scopes.push(HashMap::new());
        ScopeGuard(self)
    }

    fn get(&self, ident: &str) -> Result<i32, Error> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.get(ident))
            .copied()
            .ok_or(Error::Undeclared)
    }
}

impl<W: Write> From<W> for Compiler<'_, W> {
    fn from(value: W) -> Self {
        Self {
            enc: Encoder::from(value),
            offset: 0,
            scopes: vec![HashMap::new()],
        }
    }
}

struct ScopeGuard<'comp, 'input, W: Write>(&'comp mut Compiler<'input, W>);

impl<'input, W: Write> Deref for ScopeGuard<'_, 'input, W> {
    type Target = Compiler<'input, W>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<W: Write> DerefMut for ScopeGuard<'_, '_, W> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl<W: Write> Drop for ScopeGuard<'_, '_, W> {
    fn drop(&mut self) {
        self.0.scopes.pop();
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("Use of undeclared variable")]
    Undeclared,
}
