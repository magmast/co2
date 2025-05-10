use std::{
    collections::HashMap,
    io,
    ops::{Deref, DerefMut},
};

use bon::bon;
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
    /// Scopes visible in the current code block.
    scopes: Vec<HashMap<&'input str, i32>>,
    /// An offset from the RBP register of a previous variable.
    offset: i32,
    /// Labels for defined functions.
    funcs: HashMap<&'input str, CodeLabel>,
    /// Label to the current function epilogue.
    epilogue: CodeLabel,
    /// Currently used registers.
    regs: Vec<AsmRegister64>,
    /// Toggles zf flag meaning.
    invert_zf: bool,
}

#[bon]
impl<'input> Compiler<'input> {
    pub fn compile(&mut self, base_addr: u64, file: &File<'_, 'input>) -> Result<Vec<u8>, Error> {
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

    fn func(&mut self, func: &Func<'_, 'input>) -> Result<(), Error> {
        self.epilogue = self.asm.create_label();
        self.prepare_scopes(func)?;

        let prologue = self
            .funcs
            .entry(func.ident)
            .or_insert_with(|| self.asm.create_label());

        self.asm.set_label(prologue)?;

        {
            let mut lock = self.lock(rbp)?;
            lock.asm.mov(rbp, rsp)?;

            let frame_size = Self::compute_frame_size(&func.body) as i32;
            if frame_size != 0 {
                lock.asm.sub(rsp, frame_size)?;
            }

            lock.block(&func.body)?;

            lock.set_epilogue()?;
            lock.asm.mov(rsp, rbp)?;
        }

        self.asm.ret()?;

        Ok(())
    }

    fn prepare_scopes(&mut self, func: &Func<'_, 'input>) -> Result<(), Error> {
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

    fn set_epilogue(&mut self) -> Result<(), Error> {
        self.asm.set_label(&mut self.epilogue)?;
        Ok(())
    }

    fn block(&mut self, block: &[Stmt<'_, 'input>]) -> Result<(), Error> {
        self.scope(|c| {
            for stmt in block {
                c.stmt(stmt)?;
            }
            Ok(())
        })
    }

    fn stmt(&mut self, stmt: &Stmt<'_, 'input>) -> Result<(), Error> {
        match stmt {
            Stmt::Return(expr) => self.ret(expr),
            Stmt::Decl(decl) => self.decl(decl),
            Stmt::If(r#if) => self.r#if(r#if),
            Stmt::Expr(expr) => self.expr(self.any_reg(), expr),
        }
    }

    fn ret(&mut self, expr: &Expr<'_, 'input>) -> Result<(), Error> {
        self.expr(RETURN_REG, expr)?;
        self.asm.jmp(self.epilogue)?;
        Ok(())
    }

    fn decl(&mut self, decl: &Decl<'_, 'input>) -> Result<(), Error> {
        if let Some(init) = &decl.init {
            let reg = self.any_reg();
            self.expr(reg, init)?;
            self.offset -= 8;
            self.asm.mov(rbp + self.offset, reg)?;
            self.scopes
                .last_mut()
                .unwrap()
                .insert(decl.ident, self.offset);
        }
        Ok(())
    }

    fn r#if(&mut self, r#if: &If<'_, 'input>) -> Result<(), Error> {
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

    #[builder]
    fn int(
        &mut self,
        #[builder(finish_fn)] int: &Int<'input>,
        to: AsmRegister64,
    ) -> Result<(), Error> {
        self.asm.mov(to, int.to_i64())?;
        Ok(())
    }

    #[builder]
    fn ident(&mut self, #[builder(finish_fn)] ident: &str, to: AsmRegister64) -> Result<(), Error> {
        let offset = self.get(ident)?;
        self.asm.mov(to, rbp + offset)?;
        Ok(())
    }

    #[builder]
    fn call(
        &mut self,
        #[builder(finish_fn)] call: &Call<'_, 'input>,
        to: AsmRegister64,
    ) -> Result<(), Error> {
        let label = self
            .funcs
            .get(call.ident)
            .ok_or(Error::Undeclared)
            .copied()?;

        let param_reg = self.any_reg();
        let params_size = call.params.len() as i32 * 8;
        for param in call.params.iter().rev() {
            self.expr(param_reg, param)?;
            self.asm.push(param_reg)?;
        }

        if to == RETURN_REG {
            self.asm.call(label)?;
        } else {
            let mut lock = self.lock(RETURN_REG)?;
            lock.asm.call(label)?;
            lock.asm.mov(to, RETURN_REG)?;
        }

        self.asm.add(rsp, params_size)?;

        Ok(())
    }

    #[builder]
    fn assign(
        &mut self,
        #[builder(finish_fn)] assign: &Assign<'_, 'input>,
        to: AsmRegister64,
    ) -> Result<(), Error> {
        self.expr(to, &assign.value)?;
        let offset = self.get(assign.ident)?;
        self.asm.mov(rbp + offset, to)?;
        Ok(())
    }

    #[builder]
    fn add(
        &mut self,
        #[builder(finish_fn)] lhs: &Expr<'_, 'input>,
        #[builder(finish_fn)] rhs: &Expr<'_, 'input>,
        to: AsmRegister64,
    ) -> Result<(), Error> {
        self.expr(to, lhs)?;
        let rhs = {
            let mut lock = self.lock(to)?;
            let rhs_reg = lock.any_reg();
            lock.expr(rhs_reg, rhs)?;
            rhs_reg
        };
        self.asm.add(to, rhs)?;
        Ok(())
    }

    #[builder]
    fn sub(
        &mut self,
        #[builder(finish_fn)] lhs: &Expr<'_, 'input>,
        #[builder(finish_fn)] rhs: &Expr<'_, 'input>,
        to: AsmRegister64,
    ) -> Result<(), Error> {
        self.expr(to, lhs)?;
        let rhs = {
            let mut lock = self.lock(to)?;
            let rhs_reg = lock.any_reg();
            lock.expr(rhs_reg, rhs)?;
            rhs_reg
        };
        self.asm.sub(to, rhs)?;
        Ok(())
    }

    #[builder]
    fn mul(
        &mut self,
        #[builder(finish_fn)] lhs: &Expr<'_, 'input>,
        #[builder(finish_fn)] rhs: &Expr<'_, 'input>,
        to: AsmRegister64,
    ) -> Result<(), Error> {
        self.expr(to, lhs)?;
        let rhs = {
            let mut lock = self.lock(to)?;
            let rhs_reg = lock.any_reg();
            lock.expr(rhs_reg, rhs)?;
            rhs_reg
        };
        self.asm.imul_2(to, rhs)?;
        Ok(())
    }

    #[builder]
    fn div(
        &mut self,
        #[builder(finish_fn)] lhs: &Expr<'_, 'input>,
        #[builder(finish_fn)] rhs: &Expr<'_, 'input>,
        to: AsmRegister64,
    ) -> Result<(), Error> {
        let mut lock = self.lock(rax)?;
        let mut lock = lock.lock(rdx)?;
        lock.expr(rax, lhs)?;
        let rhs_reg = lock.any_reg();
        lock.expr(rhs_reg, rhs)?;
        lock.asm.idiv(rhs_reg)?;
        if to != rax {
            lock.asm.mov(to, rax)?;
        }
        Ok(())
    }

    #[builder]
    fn r#mod(
        &mut self,
        #[builder(finish_fn)] lhs: &Expr<'_, 'input>,
        #[builder(finish_fn)] rhs: &Expr<'_, 'input>,
        to: AsmRegister64,
    ) -> Result<(), Error> {
        let mut lock = self.lock(rax)?;
        let mut lock = lock.lock(rdx)?;
        lock.expr(rax, lhs)?;
        lock.asm.cqo()?;
        let rhs_reg = lock.any_reg();
        lock.expr(rhs_reg, rhs)?;
        lock.asm.idiv(rhs_reg)?;
        if to != rdx {
            lock.asm.mov(to, rdx)?;
        }
        Ok(())
    }

    fn eq(&mut self, lhs: &Expr<'_, 'input>, rhs: &Expr<'_, 'input>) -> Result<(), Error> {
        let lhs_reg = self.any_reg();
        self.expr(lhs_reg, lhs)?;
        let mut lock = self.lock(lhs_reg)?;
        let rhs_reg = lock.any_reg();
        lock.expr(rhs_reg, rhs)?;
        lock.asm.cmp(lhs_reg, rhs_reg)?;
        lock.invert_zf = false;
        Ok(())
    }

    fn ne(&mut self, lhs: &Expr<'_, 'input>, rhs: &Expr<'_, 'input>) -> Result<(), Error> {
        self.eq(lhs, rhs)?;
        self.invert_zf = !self.invert_zf;
        Ok(())
    }

    fn not(&mut self, expr: &Expr<'_, 'input>) -> Result<(), Error> {
        self.expr(Zf, expr)?;
        self.invert_zf = !self.invert_zf;
        Ok(())
    }

    #[builder]
    fn neg(
        &mut self,
        #[builder(finish_fn)] expr: &Expr<'_, 'input>,
        to: AsmRegister64,
    ) -> Result<(), Error> {
        self.expr(to, expr)?;
        self.asm.neg(to)?;
        Ok(())
    }

    fn jmp(&mut self, label: CodeLabel) -> Result<(), Error> {
        if self.invert_zf {
            self.asm.jz(label)?;
        } else {
            self.asm.jnz(label)?;
        }
        Ok(())
    }

    fn zf_to_reg(&mut self, reg: AsmRegister64) -> Result<(), Error> {
        let lb = reg.to_low_byte();
        if self.invert_zf {
            self.asm.setne(lb)?;
        } else {
            self.asm.sete(lb)?;
        }
        self.asm.movzx(reg, lb)?;
        Ok(())
    }

    fn reg_to_zf(&mut self, reg: AsmRegister64) -> Result<(), Error> {
        self.asm.cmp(reg, 0)?;
        self.invert_zf = true;
        Ok(())
    }

    fn scope<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T, Error>) -> Result<T, Error> {
        self.scopes.push(HashMap::new());
        let result = f(self);
        self.scopes.pop();
        result
    }

    fn get(&self, ident: &str) -> Result<i32, Error> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.get(ident))
            .copied()
            .ok_or(Error::Undeclared)
    }

    fn lock(&mut self, reg: AsmRegister64) -> Result<RegGuard<'_, 'input>, Error> {
        RegGuard::new(self, reg)
    }

    fn any_reg(&self) -> AsmRegister64 {
        [rax, rbx, rcx, rdx, r8, r9, r10, r11, r12, r13, r14, r15]
            .into_iter()
            .find(|reg| !self.regs.contains(reg))
            .expect("All registers are locked")
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
        Self {
            offset: 0,
            scopes: vec![HashMap::new()],
            funcs,
            epilogue: asm.create_label(),
            asm,
            regs: vec![rbp],
            invert_zf: true,
        }
    }
}

struct RegGuard<'comp, 'input> {
    comp: &'comp mut Compiler<'input>,
    reg: AsmRegister64,
    pop: bool,
}

impl<'comp, 'input> RegGuard<'comp, 'input> {
    fn new(comp: &'comp mut Compiler<'input>, reg: AsmRegister64) -> Result<Self, Error> {
        let pop = if comp.regs.contains(&reg) {
            comp.asm.push(reg)?;
            true
        } else {
            comp.regs.push(reg);
            false
        };

        Ok(Self { comp, reg, pop })
    }
}

impl<'input> Deref for RegGuard<'_, 'input> {
    type Target = Compiler<'input>;

    fn deref(&self) -> &Self::Target {
        self.comp
    }
}

impl DerefMut for RegGuard<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.comp
    }
}

impl Drop for RegGuard<'_, '_> {
    fn drop(&mut self) {
        if self.pop {
            self.comp.asm.pop(self.reg).unwrap();
        } else if let Some((idx, _)) = self
            .comp
            .regs
            .iter()
            .enumerate()
            .find(|(_, reg)| self.reg == **reg)
        {
            self.comp.regs.remove(idx);
        }
    }
}

trait CompilerExpr<'ident, Dst> {
    fn expr(&mut self, dst: Dst, expr: &Expr<'_, 'ident>) -> Result<(), Error>;
}

impl<'ident> CompilerExpr<'ident, AsmRegister64> for Compiler<'ident> {
    fn expr(&mut self, to: AsmRegister64, expr: &Expr<'_, 'ident>) -> Result<(), Error> {
        match expr {
            Expr::Int(int) => self.int().to(to).call(int),
            Expr::Ident(ident) => self.ident().to(to).call(ident),
            Expr::Call(call) => self.call().to(to).call(call),
            Expr::Assign(assign) => self.assign().to(to).call(assign),
            Expr::Add(lhs, rhs) => self.add().to(to).call(lhs, rhs),
            Expr::Sub(lhs, rhs) => self.sub().to(to).call(lhs, rhs),
            Expr::Mul(lhs, rhs) => self.mul().to(to).call(lhs, rhs),
            Expr::Div(lhs, rhs) => self.div().to(to).call(lhs, rhs),
            Expr::Mod(lhs, rhs) => self.r#mod().to(to).call(lhs, rhs),
            Expr::Eq(lhs, rhs) => {
                self.eq(lhs, rhs)?;
                self.zf_to_reg(to)?;
                Ok(())
            }
            Expr::Ne(lhs, rhs) => {
                self.ne(lhs, rhs)?;
                self.zf_to_reg(to)?;
                Ok(())
            }
            Expr::Not(expr) => {
                self.not(expr)?;
                self.zf_to_reg(to)?;
                Ok(())
            }
            Expr::Pos(expr) => self.expr(to, expr),
            Expr::Neg(expr) => self.neg().to(to).call(expr),
        }
    }
}

struct Zf;

impl<'ident> CompilerExpr<'ident, Zf> for Compiler<'ident> {
    fn expr(&mut self, _: Zf, expr: &Expr<'_, 'ident>) -> Result<(), Error> {
        let reg = self.any_reg();
        match expr {
            Expr::Int(int) => {
                self.int().to(reg).call(int)?;
                self.reg_to_zf(reg)?;
                Ok(())
            }
            Expr::Ident(ident) => {
                self.ident().to(reg).call(ident)?;
                self.reg_to_zf(reg)?;
                Ok(())
            }
            Expr::Call(call) => {
                self.call().to(reg).call(call)?;
                self.reg_to_zf(reg)?;
                Ok(())
            }
            Expr::Assign(assign) => {
                self.assign().to(reg).call(assign)?;
                self.reg_to_zf(reg)?;
                Ok(())
            }
            Expr::Add(lhs, rhs) => {
                self.add().to(reg).call(lhs, rhs)?;
                self.reg_to_zf(reg)?;
                Ok(())
            }
            Expr::Sub(lhs, rhs) => {
                self.sub().to(reg).call(lhs, rhs)?;
                self.reg_to_zf(reg)?;
                Ok(())
            }
            Expr::Mul(lhs, rhs) => {
                self.mul().to(reg).call(lhs, rhs)?;
                self.reg_to_zf(reg)?;
                Ok(())
            }
            Expr::Div(lhs, rhs) => {
                self.div().to(reg).call(lhs, rhs)?;
                self.reg_to_zf(reg)?;
                Ok(())
            }
            Expr::Mod(lhs, rhs) => {
                self.r#mod().to(reg).call(lhs, rhs)?;
                self.reg_to_zf(reg)?;
                Ok(())
            }
            Expr::Eq(lhs, rhs) => {
                self.eq(lhs, rhs)?;
                Ok(())
            }
            Expr::Ne(lhs, rhs) => {
                self.ne(lhs, rhs)?;
                Ok(())
            }
            Expr::Not(expr) => self.not(expr),
            Expr::Pos(expr) => {
                self.expr(reg, expr)?;
                self.asm.cmp(reg, 0)?;
                Ok(())
            }
            Expr::Neg(expr) => {
                self.neg().to(reg).call(expr)?;
                self.asm.cmp(reg, 0)?;
                Ok(())
            }
        }
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
