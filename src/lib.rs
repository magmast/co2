use std::io::{self, Write};

use ast::{Expr, Func, Stmt};
use x64::{Instruction, Reg, RegOrImm};

pub mod ast;
pub mod elf;
pub mod eval;
pub mod parser;
pub mod x64;

pub trait Writable {
    fn byte_len(&self) -> usize;

    fn write(&self, out: &mut dyn Write) -> io::Result<()>;
}

impl<T> Writable for Vec<T>
where
    T: Writable,
{
    fn byte_len(&self) -> usize {
        self.iter().map(|w| w.byte_len()).sum()
    }

    fn write(&self, out: &mut dyn Write) -> io::Result<()> {
        for w in self.iter() {
            w.write(out)?;
        }
        Ok(())
    }
}

pub fn compile(func: &Func, instrs: &mut Vec<Instruction>) {
    for stmt in &func.body {
        match stmt {
            Stmt::Return(expr) => {
                compile_expr(expr, instrs);
            }
            Stmt::Expr(expr) => {
                compile_expr(expr, instrs);
            }
        }
    }

    instrs.push(Instruction::Mov(Reg::Rdi, RegOrImm::Reg(Reg::Rax)));
    instrs.push(Instruction::Mov(Reg::Rax, RegOrImm::Imm(60)));
    instrs.push(Instruction::SysCall);
}

fn compile_expr(expr: &Expr, instrs: &mut Vec<Instruction>) {
    match expr {
        Expr::Int(val) => instrs.push(Instruction::Mov(Reg::Rax, RegOrImm::Imm(val.to_u64()))),
        Expr::Add(lhs, rhs) => {
            compile_expr(lhs, instrs);
            instrs.push(Instruction::Push(Reg::Rax));
            compile_expr(rhs, instrs);
            instrs.push(Instruction::Pop(Reg::Rbx));
            instrs.push(Instruction::Add(Reg::Rax, Reg::Rbx));
        }
        Expr::Sub(lhs, rhs) => {
            compile_expr(lhs, instrs);
            instrs.push(Instruction::Push(Reg::Rax));
            compile_expr(rhs, instrs);
            instrs.push(Instruction::Mov(Reg::Rbx, RegOrImm::Reg(Reg::Rax)));
            instrs.push(Instruction::Pop(Reg::Rax));
            instrs.push(Instruction::Sub(Reg::Rax, Reg::Rbx));
        }
        Expr::Mul(lhs, rhs) => {
            compile_expr(lhs, instrs);
            instrs.push(Instruction::Push(Reg::Rax));
            compile_expr(rhs, instrs);
            instrs.push(Instruction::Pop(Reg::Rbx));
            instrs.push(Instruction::Mul(Reg::Rbx));
        }
        Expr::Div(lhs, rhs) => {
            compile_expr(lhs, instrs);
            instrs.push(Instruction::Push(Reg::Rax));
            compile_expr(rhs, instrs);
            instrs.push(Instruction::Mov(Reg::Rbx, RegOrImm::Reg(Reg::Rax)));
            instrs.push(Instruction::Pop(Reg::Rax));
            instrs.push(Instruction::Mov(Reg::Rdx, RegOrImm::Imm(0)));
            instrs.push(Instruction::Div(Reg::Rbx));
        }
        _ => todo!(),
    }
}
