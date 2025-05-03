use std::io::{self, Write};

use ast::{Expr, Func, Stmt};
use x64::{Instruction, Reg};

pub mod ast;
pub mod elf;
pub mod eval;
pub mod parser;
pub mod x64;

pub fn compile(func: &Func) -> Vec<Instruction> {
    let mut instructions: Vec<_> = func
        .body
        .iter()
        .map(|stmt| match stmt {
            Stmt::Return(Expr::Int(int)) => Instruction::MovImm(Reg::Rax, int.to_u64()),
            _ => todo!(),
        })
        .collect();

    instructions.push(Instruction::MovReg(Reg::Rdi, Reg::Rax));
    instructions.push(Instruction::MovImm(Reg::Rax, 60));
    instructions.push(Instruction::SysCall);

    instructions
}

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
