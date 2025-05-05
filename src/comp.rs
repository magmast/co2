use std::{
    collections::HashMap,
    io, mem,
    ops::{Deref, DerefMut},
};

use iced_x86::{
    IcedError, Instruction,
    code_asm::{CodeAssembler, rax, rbp, rbx, rdi, rdx, rsp},
};

use crate::ast::{Assign, Decl, Expr, Func, If, Int, Stmt};

pub struct Compiler<'input> {
    asm: CodeAssembler,
    scopes: Vec<HashMap<&'input str, i32>>,
    offset: i32,
}

impl<'input> Compiler<'input> {
    pub fn compile(&mut self, base_addr: u64, func: &Func<'_, 'input>) -> Result<Vec<u8>, Error> {
        self.func(func)?;
        self.asm.mov(rdi, rax)?;
        self.asm.mov(rax, 60i64)?;
        self.asm.syscall()?;
        Ok(self.asm.assemble(base_addr)?)
    }

    fn func(&mut self, func: &Func<'_, 'input>) -> Result<(), Error> {
        self.offset = 0;

        let body_instrs = self.buf(|c| c.block(&func.body))?;

        self.asm.push(rbp)?;
        self.asm.mov(rbp, rsp)?;

        let frame_size = -self.offset;
        self.asm.sub(rsp, frame_size)?;
        for instr in body_instrs {
            self.asm.add_instruction(instr)?;
        }

        self.asm.mov(rsp, rbp)?;
        self.asm.pop(rbp)?;

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
            self.asm.mov(rbp + self.offset, rax)?;
            self.scopes
                .last_mut()
                .unwrap()
                .insert(decl.ident, self.offset);
            self.offset -= 8;
        }
        Ok(())
    }

    fn r#if(&mut self, r#if: &If<'_, 'input>) -> Result<(), Error> {
        let mut end_label = self.asm.create_label();

        self.expr(&r#if.cond)?;
        self.asm.cmp(rax, 0)?;

        if let Some(else_body) = r#if.r#else.as_ref() {
            let mut else_label = self.asm.create_label();
            self.asm.jz(else_label)?;

            self.block(&r#if.then)?;
            self.asm.jmp(end_label)?;

            self.asm.set_label(&mut else_label)?;
            self.block(else_body)?;
        } else {
            self.asm.jz(end_label)?;
            self.block(&r#if.then)?;
        }

        self.asm.set_label(&mut end_label)?;
        self.asm.nop()?;

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
        self.asm.mov(rax, int.to_i64())?;
        Ok(())
    }

    fn ident(&mut self, ident: &str) -> Result<(), Error> {
        let offset = self.get(ident)?;
        self.asm.mov(rax, rbp + offset)?;
        Ok(())
    }

    fn assign(&mut self, assign: &Assign) -> Result<(), Error> {
        self.expr(&assign.value)?;
        let offset = self.get(assign.ident)?;
        self.asm.mov(rbp + offset, rax)?;
        Ok(())
    }

    fn add(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.asm.push(rax)?;
        self.expr(rhs)?;
        self.asm.pop(rbx)?;
        self.asm.add(rax, rbx)?;
        Ok(())
    }

    fn sub(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.asm.push(rax)?;
        self.expr(rhs)?;
        self.asm.mov(rbx, rax)?;
        self.asm.pop(rax)?;
        self.asm.sub(rax, rbx)?;
        Ok(())
    }

    fn mul(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.asm.push(rax)?;
        self.expr(rhs)?;
        self.asm.pop(rbx)?;
        self.asm.imul(rbx)?;
        Ok(())
    }

    fn div(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.asm.push(rax)?;
        self.expr(rhs)?;
        self.asm.mov(rbx, rax)?;
        self.asm.pop(rax)?;
        self.asm.mov(rdx, 0i64)?;
        self.asm.idiv(rbx)?;
        Ok(())
    }

    fn neg(&mut self, expr: &Expr) -> Result<(), Error> {
        self.expr(expr)?;
        self.asm.neg(rax)?;
        Ok(())
    }

    fn buf(
        &mut self,
        f: impl FnOnce(&mut Compiler<'input>) -> Result<(), Error>,
    ) -> Result<Vec<Instruction>, Error> {
        let mut child = Compiler {
            asm: CodeAssembler::new(64)?,
            offset: self.offset,
            scopes: mem::take(&mut self.scopes),
        };
        let result = f(&mut child);
        self.scopes = child.scopes;
        self.offset = child.offset;
        result.and(Ok(child.asm.take_instructions()))
    }

    fn enter(&mut self) -> ScopeGuard<'_, 'input> {
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

impl Default for Compiler<'_> {
    fn default() -> Self {
        Self {
            asm: CodeAssembler::new(64).unwrap(),
            offset: 0,
            scopes: vec![HashMap::new()],
        }
    }
}

struct ScopeGuard<'comp, 'input>(&'comp mut Compiler<'input>);

impl<'input> Deref for ScopeGuard<'_, 'input> {
    type Target = Compiler<'input>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl DerefMut for ScopeGuard<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl Drop for ScopeGuard<'_, '_> {
    fn drop(&mut self) {
        self.0.scopes.pop();
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Iced(#[from] IcedError),
    #[error("Use of undeclared variable")]
    Undeclared,
}
