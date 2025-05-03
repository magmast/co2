use std::{
    collections::HashMap,
    io::{self, Write},
};

use crate::{
    ast::{Decl, Expr, Func, Int, Stmt},
    x64::{Encoder, Reg},
};

pub struct Compiler<'input, W: Write> {
    enc: Encoder<W>,
    frames: Vec<Frame<'input>>,
}

impl<'input, W: Write> Compiler<'input, W> {
    pub fn compile(&mut self, func: &Func<'_, 'input>) -> Result<(), Error> {
        self.func(func)?;
        self.enc.mov(Reg::Rax, Reg::Rdi)?;
        self.enc.mov(60, Reg::Rax)?;
        self.enc.sys_call()?;
        Ok(())
    }

    fn func(&mut self, func: &Func<'_, 'input>) -> Result<(), Error> {
        self.frames.push(Frame::new(func));

        self.enc.push(Reg::Rbp)?;
        self.enc.mov(Reg::Rsp, Reg::Rbp)?;
        self.enc.sub(Reg::Rsp, self.frame().size() as i32)?;

        for stmt in &func.body {
            self.stmt(stmt)?;
        }

        self.enc.mov(Reg::Rbp, Reg::Rsp)?;
        self.enc.pop(Reg::Rbp)?;

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
            self.enc.mov(Reg::Rax, (Reg::Rbp, offset))?;
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
        }
    }

    fn int(&mut self, int: &Int) -> Result<(), Error> {
        self.enc.mov(int.to_i32(), Reg::Rax)?;
        Ok(())
    }

    fn ident(&mut self, ident: &str) -> Result<(), Error> {
        let offset = self.frame().get(ident).ok_or(Error::Undeclared)?;
        self.enc.mov((Reg::Rbp, offset), Reg::Rax)?;
        Ok(())
    }

    fn add(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.enc.push(Reg::Rax)?;
        self.expr(rhs)?;
        self.enc.pop(Reg::Rbx)?;
        self.enc.add(Reg::Rax, Reg::Rbx)?;
        Ok(())
    }

    fn sub(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.enc.push(Reg::Rax)?;
        self.expr(rhs)?;
        self.enc.mov(Reg::Rax, Reg::Rbx)?;
        self.enc.pop(Reg::Rax)?;
        self.enc.sub(Reg::Rax, Reg::Rbx)?;
        Ok(())
    }

    fn mul(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.enc.push(Reg::Rax)?;
        self.expr(rhs)?;
        self.enc.pop(Reg::Rbx)?;
        self.enc.imul(Reg::Rbx)?;
        Ok(())
    }

    fn div(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.enc.push(Reg::Rax)?;
        self.expr(rhs)?;
        self.enc.mov(Reg::Rax, Reg::Rbx)?;
        self.enc.push(Reg::Rax)?;
        self.enc.mov(0, Reg::Rdx)?;
        self.enc.idiv(Reg::Rbx)?;
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
        self.locals.len() * 8
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
