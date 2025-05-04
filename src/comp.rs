use std::{
    collections::HashMap,
    io::{self, Write},
};

use crate::{
    ast::{Decl, Expr, Func, Int, Stmt},
    x64::{Encoder, M32, M64, Reg},
};

pub struct Compiler<'input, W: Write> {
    enc: Encoder<W>,
    frames: Vec<Frame<'input>>,
}

impl<'input, W: Write> Compiler<'input, W> {
    pub fn compile(&mut self, func: &Func<'_, 'input>) -> Result<(), Error> {
        self.func(func)?;
        self.enc.movsxd::<M64>(Reg::Ax, Reg::Di)?;
        self.enc.mov::<M64>(60i32, Reg::Ax)?;
        self.enc.sys_call()?;
        Ok(())
    }

    fn func(&mut self, func: &Func<'_, 'input>) -> Result<(), Error> {
        self.frames.push(Frame::new(func));

        self.enc.push::<M64>(Reg::Bp)?;
        self.enc.mov::<M64>(Reg::Sp, Reg::Bp)?;
        self.enc.sub::<M64>(Reg::Sp, self.frame().size() as i32)?;

        for stmt in &func.body {
            self.stmt(stmt)?;
        }

        self.enc.mov::<M64>(Reg::Bp, Reg::Sp)?;
        self.enc.pop::<M64>(Reg::Bp)?;

        self.frames.pop();

        Ok(())
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Decl(decl) => self.decl(decl),
            Stmt::Expr(expr) => self.expr(expr),
            Stmt::Return(expr) => self.expr(expr),
        }
    }

    fn decl(&mut self, decl: &Decl) -> Result<(), Error> {
        if let Some(init) = &decl.init {
            self.expr(init)?;
            let offset = self.frame().get(decl.ident).unwrap();
            self.enc.mov::<M32>(Reg::Ax, (Reg::Bp, offset))?;
        }
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> Result<(), Error> {
        match expr {
            Expr::Int(int) => self.int(int),
            Expr::Ident(ident) => self.ident(ident),
            Expr::Add(lhs, rhs) => self.add(lhs, rhs),
            Expr::Sub(lhs, rhs) => self.sub(lhs, rhs),
            Expr::Mul(lhs, rhs) => self.mul(lhs, rhs),
            Expr::Div(lhs, rhs) => self.div(lhs, rhs),
            Expr::Pos(expr) => self.expr(expr),
            Expr::Neg(expr) => self.neg(expr),
        }
    }

    fn int(&mut self, int: &Int) -> Result<(), Error> {
        self.enc.mov::<M32>(int.to_i32(), Reg::Ax)?;
        Ok(())
    }

    fn ident(&mut self, ident: &str) -> Result<(), Error> {
        let offset = self.frame().get(ident).ok_or(Error::Undeclared)?;
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
        self.enc.mov::<M32>(Reg::Ax, Reg::Bx)?;
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
        self.enc.mov::<M32>(Reg::Ax, Reg::Bx)?;
        self.enc.pop::<M32>(Reg::Ax)?;
        self.enc.mov::<M32>(0, Reg::Dx)?;
        self.enc.idiv::<M32>(Reg::Bx)?;
        Ok(())
    }

    fn neg(&mut self, expr: &Expr) -> Result<(), Error> {
        self.expr(expr)?;
        self.enc.neg::<M32>(Reg::Ax)?;
        Ok(())
    }

    fn frame(&self) -> &Frame<'input> {
        self.frames.last().unwrap()
    }
}

impl<W: Write> From<W> for Compiler<'_, W> {
    fn from(value: W) -> Self {
        Self {
            enc: Encoder::from(value),
            frames: vec![],
        }
    }
}

#[derive(Default)]
struct Frame<'input> {
    locals: HashMap<&'input str, i32>,
}

impl<'input> Frame<'input> {
    fn new(func: &Func<'_, 'input>) -> Self {
        Self {
            locals: func
                .body
                .iter()
                .filter_map(|stmt| {
                    if let Stmt::Decl(decl) = stmt {
                        Some(decl.ident)
                    } else {
                        None
                    }
                })
                .scan(0, |offset, name| {
                    let local = (name, *offset);
                    *offset -= 8;
                    Some(local)
                })
                .collect(),
        }
    }

    fn size(&self) -> usize {
        self.locals.len() * 4
    }

    fn get(&self, name: &str) -> Option<i32> {
        self.locals.get(name).copied()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("Use of undeclared variable")]
    Undeclared,
}
