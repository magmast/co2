use std::{collections::HashMap, io};

use iced_x86::{
    IcedError,
    code_asm::{
        AsmRegister8, AsmRegister64, CodeAssembler, CodeLabel, al, bl, cl, dl, r8, r8b, r9, r9b,
        r10, r10b, r11, r11b, r12, r12b, r13, r13b, r14, r14b, r15, r15b, rax, rbp, rbx, rcx, rdi,
        rdx, rsp,
    },
};

use crate::ast::{Assign, Call, Decl, Expr, File, Func, If, Int, Stmt, Type};

const RETURN_REG: AsmRegister64 = rax;

pub struct Compiler<'input> {
    asm: CodeAssembler,

    /// Labels for defined functions.
    funcs: HashMap<&'input str, CodeLabel>,

    /// Scopes visible in the current code block.
    scopes: Vec<HashMap<&'input str, i32>>,
    /// An offset from the RBP register of a previous variable.
    offset: i32,
    /// Label to the current function epilogue.
    epilogue: CodeLabel,

    /// Toggles zf flag meaning.
    invert_zf: bool,

    lru: [AsmRegister64; 12],
    used_regs: Vec<AsmRegister64>,
}

impl<'input> Compiler<'input> {
    pub fn compile(&mut self, base_addr: u64, file: &File<'_, 'input>) -> Result<Vec<u8>> {
        let main = self
            .funcs
            .get("main")
            .expect("Label for main function must be always added when creating a compiler");
        self.asm.call(*main)?;
        self.asm.mov(rdi, RETURN_REG)?;
        self.asm.mov(rax, 60i64)?;
        self.asm.syscall()?;
        for func in &file.funcs {
            self.func(func)?;
        }
        Ok(self.asm.assemble(base_addr)?)
    }

    fn func(&mut self, func: &Func<'_, 'input>) -> Result<()> {
        self.epilogue = self.asm.create_label();
        self.prepare_scopes(func)?;

        let prologue = self
            .funcs
            .entry(func.ident)
            .or_insert_with(|| self.asm.create_label());

        self.asm.set_label(prologue)?;

        {
            self.asm.push(rbp)?;
            self.asm.mov(rbp, rsp)?;

            let frame_size = Self::compute_frame_size(&func.body) as i32;
            if frame_size != 0 {
                self.asm.sub(rsp, frame_size)?;
            }

            self.block(&func.body)?;

            self.asm.set_label(&mut self.epilogue)?;
            self.asm.mov(rsp, rbp)?;
            self.asm.pop(rbp)?;
        }

        self.asm.ret()?;

        Ok(())
    }

    fn prepare_scopes(&mut self, func: &Func<'_, 'input>) -> Result<()> {
        self.offset = 0;
        self.scopes.clear();
        let mut params_scope = HashMap::new();
        for (i, param) in func.params.iter().enumerate() {
            match param.ty {
                Type::Void => {
                    if i > 0 || param.ident.is_some() {
                        return Err(Error::VoidType);
                    }
                }
                Type::Int => {
                    if let Some(ident) = param.ident {
                        params_scope.insert(ident, (i as i32 + 2) * 8);
                    }
                }
            }
        }
        self.scopes.push(params_scope);
        Ok(())
    }

    fn block(&mut self, block: &[Stmt<'_, 'input>]) -> Result<()> {
        self.scope(|c| {
            for stmt in block {
                c.stmt(stmt)?;
            }
            Ok(())
        })
    }

    fn stmt(&mut self, stmt: &Stmt<'_, 'input>) -> Result<()> {
        match stmt {
            Stmt::Return(expr) => self.ret(expr),
            Stmt::Decl(decl) => self.decl(decl),
            Stmt::If(r#if) => self.r#if(r#if),
            Stmt::Expr(expr) => self.reg(None, |c, reg| c.expr(reg, expr)),
        }
    }

    fn ret(&mut self, expr: &Expr<'_, 'input>) -> Result<()> {
        self.expr(RETURN_REG, expr)?;
        self.asm.jmp(self.epilogue)?;
        Ok(())
    }

    fn decl(&mut self, decl: &Decl<'_, 'input>) -> Result<()> {
        if let Some(init) = &decl.init {
            self.reg(None, |c, reg| {
                c.expr(reg, init)?;
                c.offset -= 8;
                c.asm.mov(rbp + c.offset, reg)?;
                c.scopes.last_mut().unwrap().insert(decl.ident, c.offset);
                Ok(())
            })?;
        }
        Ok(())
    }

    fn r#if(&mut self, r#if: &If<'_, 'input>) -> Result<()> {
        let mut end_label = self.asm.create_label();

        self.expr(Zf, &r#if.cond)?;

        if let Some(else_body) = r#if.r#else.as_ref() {
            let mut else_label = self.asm.create_label();
            self.jmp(else_label)?;

            self.block(&r#if.then)?;
            self.asm.jmp(end_label)?;

            self.asm.set_label(&mut else_label)?;
            self.block(else_body)?;
        } else {
            self.jmp(end_label)?;
            self.block(&r#if.then)?;
        }

        self.asm.set_label(&mut end_label)?;
        self.asm.nop()?;

        Ok(())
    }

    fn int(&mut self, dst: AsmRegister64, int: &Int<'input>) -> Result<()> {
        self.asm.mov(dst, int.to_i64())?;
        Ok(())
    }

    fn ident(&mut self, dst: AsmRegister64, ident: &str) -> Result<()> {
        let offset = self.offset_of(ident)?;
        self.asm.mov(dst, rbp + offset)?;
        Ok(())
    }

    fn call(&mut self, dst: AsmRegister64, call: &Call<'_, 'input>) -> Result<()> {
        let label = self
            .funcs
            .get(call.ident)
            .ok_or(Error::Undeclared)
            .copied()?;

        let params_size = call.params.len() as i32 * 8;
        self.reg(None, |c, reg| {
            for param in call.params.iter().rev() {
                c.expr(reg, param)?;
                c.asm.push(reg)?;
            }

            if dst == RETURN_REG {
                c.asm.call(label)?;
            } else {
                c.reg(Some(RETURN_REG), |c, reg| {
                    c.asm.call(label)?;
                    c.asm.mov(dst, reg)?;
                    Ok(())
                })?;
            }

            c.asm.add(rsp, params_size)?;

            Ok(())
        })
    }

    fn assign(&mut self, dst: AsmRegister64, assign: &Assign<'_, 'input>) -> Result<()> {
        self.expr(dst, &assign.value)?;
        let offset = self.offset_of(assign.ident)?;
        self.asm.mov(rbp + offset, dst)?;
        Ok(())
    }

    fn add(
        &mut self,
        dst: AsmRegister64,
        lhs: &Expr<'_, 'input>,
        rhs: &Expr<'_, 'input>,
    ) -> Result<()> {
        self.expr(dst, lhs)?;
        self.reg(None, |c, src| {
            c.expr(src, rhs)?;
            c.asm.add(dst, src)?;
            Ok(())
        })
    }

    fn sub(
        &mut self,
        dst: AsmRegister64,
        lhs: &Expr<'_, 'input>,
        rhs: &Expr<'_, 'input>,
    ) -> Result<()> {
        self.expr(dst, lhs)?;
        self.reg(None, |c, src| {
            c.expr(src, rhs)?;
            c.asm.sub(dst, src)?;
            Ok(())
        })
    }

    fn mul(
        &mut self,
        dst: AsmRegister64,
        lhs: &Expr<'_, 'input>,
        rhs: &Expr<'_, 'input>,
    ) -> Result<()> {
        self.expr(dst, lhs)?;
        self.reg(None, |c, src| {
            c.expr(src, rhs)?;
            c.asm.imul_2(dst, src)?;
            Ok(())
        })
    }

    fn div(
        &mut self,
        dst: AsmRegister64,
        lhs: &Expr<'_, 'input>,
        rhs: &Expr<'_, 'input>,
    ) -> Result<()> {
        let push_rax = dst != rax && self.used_regs.contains(&rax);
        if push_rax {
            self.asm.push(rax)?;
        }

        let push_rdx = dst != rdx && self.used_regs.contains(&rdx);
        if push_rdx {
            self.asm.push(rdx)?;
        }

        self.reg(None, |c, src| {
            c.expr(rax, lhs)?;
            c.asm.cqo()?;
            c.expr(src, rhs)?;
            c.asm.idiv(src)?;
            Ok(())
        })?;

        if push_rax {
            self.asm.pop(rax)?;
        }

        if push_rdx {
            self.asm.pop(rdx)?;
        }

        if dst != rax {
            self.asm.mov(dst, rax)?;
        }

        Ok(())
    }

    fn r#mod(
        &mut self,
        dst: AsmRegister64,
        lhs: &Expr<'_, 'input>,
        rhs: &Expr<'_, 'input>,
    ) -> Result<()> {
        let push_rax = dst != rax && self.used_regs.contains(&rax);
        if push_rax {
            self.asm.push(rax)?;
        }

        let push_rdx = dst != rdx && self.used_regs.contains(&rdx);
        if push_rdx {
            self.asm.push(rdx)?;
        }

        self.reg(None, |c, src| {
            c.expr(rax, lhs)?;
            c.asm.cqo()?;
            c.expr(src, rhs)?;
            c.asm.idiv(src)?;
            Ok(())
        })?;

        if push_rax {
            self.asm.pop(rax)?;
        }

        if push_rdx {
            self.asm.pop(rdx)?;
        }

        if dst != rdx {
            self.asm.mov(dst, rdx)?;
        }

        Ok(())
    }

    fn eq(&mut self, lhs: &Expr<'_, 'input>, rhs: &Expr<'_, 'input>) -> Result<()> {
        self.reg((None, None), |c, (r0, r1)| {
            c.expr(r0, lhs)?;
            c.expr(r1, rhs)?;
            c.asm.cmp(r0, r1)?;
            c.invert_zf = false;
            Ok(())
        })
    }

    fn ne(&mut self, lhs: &Expr<'_, 'input>, rhs: &Expr<'_, 'input>) -> Result<()> {
        self.eq(lhs, rhs)?;
        self.invert_zf = !self.invert_zf;
        Ok(())
    }

    fn not(&mut self, expr: &Expr<'_, 'input>) -> Result<()> {
        self.expr(Zf, expr)?;
        self.invert_zf = !self.invert_zf;
        Ok(())
    }

    fn neg(&mut self, dst: AsmRegister64, expr: &Expr<'_, 'input>) -> Result<()> {
        self.expr(dst, expr)?;
        self.asm.neg(dst)?;
        Ok(())
    }

    fn jmp(&mut self, label: CodeLabel) -> Result<()> {
        if self.invert_zf {
            self.asm.jz(label)?;
        } else {
            self.asm.jnz(label)?;
        }
        Ok(())
    }

    fn zf_to_reg(&mut self, reg: AsmRegister64) -> Result<()> {
        let lb = reg.to_low_byte();
        if self.invert_zf {
            self.asm.setne(lb)?;
        } else {
            self.asm.sete(lb)?;
        }
        self.asm.movzx(reg, lb)?;
        Ok(())
    }

    fn reg_to_zf(&mut self, reg: AsmRegister64) -> Result<()> {
        self.asm.cmp(reg, 0)?;
        self.invert_zf = true;
        Ok(())
    }

    fn scope<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
        self.scopes.push(HashMap::new());
        let result = f(self);
        self.scopes.pop();
        result
    }

    fn offset_of(&self, ident: &str) -> Result<i32> {
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

        let mut funcs = HashMap::new();
        funcs.insert("main", asm.create_label());

        let epilogue = asm.create_label();

        Self {
            asm,

            funcs,

            offset: 0,
            scopes: vec![HashMap::new()],
            epilogue,

            invert_zf: true,

            lru: [rax, rbx, rcx, rdx, r8, r9, r10, r11, r12, r13, r14, r15],
            used_regs: vec![],
        }
    }
}

trait ExprCompiler<'ident, Dst> {
    fn expr(&mut self, dst: Dst, expr: &Expr<'_, 'ident>) -> Result<()>;
}

impl<'ident> ExprCompiler<'ident, AsmRegister64> for Compiler<'ident> {
    fn expr(&mut self, dst: AsmRegister64, expr: &Expr<'_, 'ident>) -> Result<()> {
        match expr {
            Expr::Int(int) => self.int(dst, int),
            Expr::Ident(ident) => self.ident(dst, ident),
            Expr::Call(call) => self.call(dst, call),
            Expr::Assign(assign) => self.assign(dst, assign),
            Expr::Add(lhs, rhs) => self.add(dst, lhs, rhs),
            Expr::Sub(lhs, rhs) => self.sub(dst, lhs, rhs),
            Expr::Mul(lhs, rhs) => self.mul(dst, lhs, rhs),
            Expr::Div(lhs, rhs) => self.div(dst, lhs, rhs),
            Expr::Mod(lhs, rhs) => self.r#mod(dst, lhs, rhs),
            Expr::Eq(lhs, rhs) => {
                self.eq(lhs, rhs)?;
                self.zf_to_reg(dst)?;
                Ok(())
            }
            Expr::Ne(lhs, rhs) => {
                self.ne(lhs, rhs)?;
                self.zf_to_reg(dst)?;
                Ok(())
            }
            Expr::Not(expr) => {
                self.not(expr)?;
                self.zf_to_reg(dst)?;
                Ok(())
            }
            Expr::Pos(expr) => self.expr(dst, expr),
            Expr::Neg(expr) => self.neg(dst, expr),
        }
    }
}

impl<'ident> ExprCompiler<'ident, Zf> for Compiler<'ident> {
    fn expr(&mut self, _: Zf, expr: &Expr<'_, 'ident>) -> Result<()> {
        self.reg(None, |c, dst| match expr {
            Expr::Int(int) => {
                c.int(dst, int)?;
                c.reg_to_zf(dst)
            }
            Expr::Ident(ident) => {
                c.ident(dst, ident)?;
                c.reg_to_zf(dst)
            }
            Expr::Call(call) => {
                c.call(dst, call)?;
                c.reg_to_zf(dst)
            }
            Expr::Assign(assign) => {
                c.assign(dst, assign)?;
                c.reg_to_zf(dst)
            }
            Expr::Add(lhs, rhs) => {
                c.add(dst, lhs, rhs)?;
                c.reg_to_zf(dst)
            }
            Expr::Sub(lhs, rhs) => {
                c.sub(dst, lhs, rhs)?;
                c.reg_to_zf(dst)
            }
            Expr::Mul(lhs, rhs) => {
                c.mul(dst, lhs, rhs)?;
                c.reg_to_zf(dst)
            }
            Expr::Div(lhs, rhs) => {
                c.div(dst, lhs, rhs)?;
                c.reg_to_zf(dst)
            }
            Expr::Mod(lhs, rhs) => {
                c.r#mod(dst, lhs, rhs)?;
                c.reg_to_zf(dst)
            }
            Expr::Eq(lhs, rhs) => c.eq(lhs, rhs),
            Expr::Ne(lhs, rhs) => c.ne(lhs, rhs),
            Expr::Not(expr) => c.not(expr),
            Expr::Pos(expr) => {
                c.expr(dst, expr)?;
                c.reg_to_zf(dst)
            }
            Expr::Neg(expr) => {
                c.neg(dst, expr)?;
                c.reg_to_zf(dst)
            }
        })
    }
}

/// Represents an x86 zero flag.
///
/// Used as expression destination for expressions that are used in conditions.
struct Zf;

trait LockReg<I> {
    type Reg;

    fn reg<T>(&mut self, reg: I, f: impl FnOnce(&mut Self, Self::Reg) -> Result<T>) -> Result<T>;
}

impl LockReg<AsmRegister64> for Compiler<'_> {
    type Reg = AsmRegister64;

    fn reg<T>(
        &mut self,
        reg: AsmRegister64,
        f: impl FnOnce(&mut Self, Self::Reg) -> Result<T>,
    ) -> Result<T> {
        let idx = self.lru.iter().position(|&r| r == reg).unwrap();
        self.lru[idx..].rotate_left(1);

        let already_used = self.used_regs.contains(&reg);
        if already_used {
            self.asm.push(reg)?;
        } else {
            self.used_regs.push(reg);
        }

        let result = f(self, reg);

        if already_used {
            self.asm.pop(reg)?;
        } else if let Some(idx) = self.used_regs.iter().position(|&r| r == reg) {
            self.used_regs.remove(idx);
        }

        result
    }
}

impl LockReg<Option<AsmRegister64>> for Compiler<'_> {
    type Reg = AsmRegister64;

    fn reg<TOutput>(
        &mut self,
        reg: Option<AsmRegister64>,
        f: impl FnOnce(&mut Self, Self::Reg) -> Result<TOutput>,
    ) -> Result<TOutput> {
        LockReg::<AsmRegister64>::reg(self, reg.unwrap_or_else(|| self.lru[0]), f)
    }
}

impl LockReg<(Option<AsmRegister64>, Option<AsmRegister64>)> for Compiler<'_> {
    type Reg = (AsmRegister64, AsmRegister64);

    fn reg<TOutput>(
        &mut self,
        (r0, r1): (Option<AsmRegister64>, Option<AsmRegister64>),
        f: impl FnOnce(&mut Self, Self::Reg) -> Result<TOutput>,
    ) -> Result<TOutput> {
        LockReg::<Option<AsmRegister64>>::reg(self, r0, |c, r0| c.reg(r1, |c, r1| f(c, (r0, r1))))
    }
}

impl LockReg<(AsmRegister64, Option<AsmRegister64>)> for Compiler<'_> {
    type Reg = (AsmRegister64, AsmRegister64);

    fn reg<T>(
        &mut self,
        (r0, r1): (AsmRegister64, Option<AsmRegister64>),
        f: impl FnOnce(&mut Self, Self::Reg) -> Result<T>,
    ) -> Result<T> {
        LockReg::<(Option<AsmRegister64>, Option<AsmRegister64>)>::reg(self, (Some(r0), r1), f)
    }
}

impl LockReg<(AsmRegister64, AsmRegister64, Option<AsmRegister64>)> for Compiler<'_> {
    type Reg = (AsmRegister64, AsmRegister64, AsmRegister64);

    fn reg<T>(
        &mut self,
        (r0, r1, r2): (AsmRegister64, AsmRegister64, Option<AsmRegister64>),
        f: impl FnOnce(&mut Self, Self::Reg) -> Result<T>,
    ) -> Result<T> {
        LockReg::<(AsmRegister64, Option<AsmRegister64>)>::reg(
            self,
            (r0, Some(r1)),
            |c, (r0, r1)| c.reg(r2, |c, r2| f(c, (r0, r1, r2))),
        )
    }
}

trait ToLowByte {
    fn to_low_byte(self) -> AsmRegister8;
}

impl ToLowByte for AsmRegister64 {
    fn to_low_byte(self) -> AsmRegister8 {
        #[allow(non_upper_case_globals)]
        match self {
            rax => al,
            rbx => bl,
            rcx => cl,
            rdx => dl,
            r8 => r8b,
            r9 => r9b,
            r10 => r10b,
            r11 => r11b,
            r12 => r12b,
            r13 => r13b,
            r14 => r14b,
            r15 => r15b,
            _ => panic!("Unsupported register {self:?}"),
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Iced(#[from] IcedError),
    #[error("Use of undeclared variable")]
    Undeclared,
    #[error("Void cannot be used as a param or variable type")]
    VoidType,
}
