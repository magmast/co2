use std::{
    collections::HashMap,
    io,
    ops::{Deref, DerefMut},
};

use iced_x86::{
    IcedError,
    code_asm::{CodeAssembler, CodeLabel, al, rax, rbp, rbx, rdi, rdx, rsp},
};

use crate::ast::{Assign, Call, Decl, Expr, File, Func, If, Int, Stmt, Type};

pub struct Compiler<'input> {
    asm: CodeAssembler,
    scopes: Vec<HashMap<&'input str, i32>>,
    offset: i32,
    funcs: HashMap<&'input str, CodeLabel>,
    epilogue: CodeLabel,
    main: CodeLabel,
}

impl<'input> Compiler<'input> {
    pub fn compile(&mut self, base_addr: u64, file: &File<'_, 'input>) -> Result<Vec<u8>, Error> {
        self.asm.call(self.main)?;
        self.asm.mov(rdi, rax)?;
        self.asm.mov(rax, 60i64)?;
        self.asm.syscall()?;
        for func in &file.funcs {
            self.func(func)?;
        }
        Ok(self.asm.assemble(base_addr)?)
    }

    fn func(&mut self, func: &Func<'_, 'input>) -> Result<(), Error> {
        if func.ty != Type::Int {
            todo!();
        }

        self.epilogue = self.asm.create_label();
        self.prepare_scopes(func);

        let prologue = if func.ident == "main" {
            self.asm.set_label(&mut self.main)?;
            self.main
        } else {
            let mut label = self.asm.create_label();
            self.asm.set_label(&mut label)?;
            label
        };

        self.funcs.insert(func.ident, prologue);

        self.asm.push(rbp)?;
        self.asm.mov(rbp, rsp)?;
        let frame_size = Self::compute_frame_size(&func.body) as i32;
        self.asm.sub(rsp, frame_size)?;

        self.block(&func.body)?;

        self.asm.set_label(&mut self.epilogue)?;
        self.asm.mov(rsp, rbp)?;
        self.asm.pop(rbp)?;
        self.asm.ret()?;

        Ok(())
    }

    fn prepare_scopes(&mut self, func: &Func<'_, 'input>) {
        self.offset = 0;
        self.scopes.clear();
        let mut params_scope = HashMap::new();
        for (i, param) in func.params.iter().enumerate() {
            if param.ty != Type::Int {
                todo!();
            }
            if let Some(ident) = param.ident {
                params_scope.insert(ident, (i as i32 + 2) * 8);
            }
        }
        self.scopes.push(params_scope);
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
            Stmt::Return(expr) => self.ret(expr),
            Stmt::Decl(decl) => self.decl(decl),
            Stmt::If(r#if) => self.r#if(r#if),
            Stmt::Expr(expr) => self.expr(expr),
        }
    }

    fn ret(&mut self, expr: &Expr) -> Result<(), Error> {
        self.expr(expr)?;
        self.asm.jmp(self.epilogue)?;
        Ok(())
    }

    fn decl(&mut self, decl: &Decl<'_, 'input>) -> Result<(), Error> {
        if let Some(init) = &decl.init {
            self.expr(init)?;
            self.offset -= 8;
            self.asm.mov(rbp + self.offset, rax)?;
            self.scopes
                .last_mut()
                .unwrap()
                .insert(decl.ident, self.offset);
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
            Expr::Call(call) => self.call(call),
            Expr::Assign(assign) => self.assign(assign),
            Expr::Add(lhs, rhs) => self.add(lhs, rhs),
            Expr::Sub(lhs, rhs) => self.sub(lhs, rhs),
            Expr::Mul(lhs, rhs) => self.mul(lhs, rhs),
            Expr::Div(lhs, rhs) => self.div(lhs, rhs),
            Expr::Eq(lhs, rhs) => self.eq(lhs, rhs),
            Expr::Ne(lhs, rhs) => self.ne(lhs, rhs),
            Expr::Not(expr) => self.not(expr),
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

    fn call(&mut self, call: &Call) -> Result<(), Error> {
        let label = self
            .funcs
            .get(call.ident)
            .ok_or(Error::Undeclared)
            .copied()?;

        let params_size = call.params.len() as i32 * 8;
        for param in call.params.iter().rev() {
            self.expr(param)?;
            self.asm.push(rax)?;
        }
        self.asm.call(label)?;
        self.asm.add(rsp, params_size)?;

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

    fn eq(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.asm.push(rax)?;
        self.expr(rhs)?;
        self.asm.pop(rbx)?;
        self.asm.cmp(rax, rbx)?;
        self.asm.sete(al)?;
        self.asm.movzx(rax, al)?;
        Ok(())
    }

    fn ne(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expr(lhs)?;
        self.asm.push(rax)?;
        self.expr(rhs)?;
        self.asm.pop(rbx)?;
        self.asm.cmp(rax, rbx)?;
        self.asm.setne(al)?;
        self.asm.movzx(rax, al)?;
        Ok(())
    }

    fn not(&mut self, expr: &Expr) -> Result<(), Error> {
        self.expr(expr)?;
        self.asm.cmp(rax, 0)?;
        self.asm.sete(al)?;
        self.asm.movzx(rax, al)?;
        Ok(())
    }

    fn neg(&mut self, expr: &Expr) -> Result<(), Error> {
        self.expr(expr)?;
        self.asm.neg(rax)?;
        Ok(())
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

    fn compute_frame_size(block: &[Stmt]) -> usize {
        block
            .iter()
            .map(|stmt| match stmt {
                Stmt::Decl(_) => 8,
                Stmt::If(r#if) => {
                    let then_size = Self::compute_frame_size(&r#if.then);
                    r#if.r#else
                        .as_ref()
                        .map(|r#else| then_size + Self::compute_frame_size(r#else))
                        .unwrap_or(then_size)
                }
                _ => 0,
            })
            .sum()
    }
}

impl Default for Compiler<'_> {
    fn default() -> Self {
        let mut asm = CodeAssembler::new(64).unwrap();
        Self {
            offset: 0,
            scopes: vec![HashMap::new()],
            funcs: HashMap::new(),
            epilogue: asm.create_label(),
            main: asm.create_label(),
            asm,
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
