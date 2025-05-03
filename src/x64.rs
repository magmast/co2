use std::io::Write;

use crate::Writable;

const REX_W: u8 = 0x48;

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    Rax,
    Rdx,
    Rbx,
    Rdi,
}

impl Reg {
    fn mod_rm_reg(&self, src: &Reg) -> u8 {
        (0b11 << 6) | (u8::from(*src) << 3) | u8::from(*self)
    }

    fn mod_rm_op(&self, op: u8) -> u8 {
        (0b11 << 6) | (op << 3) | u8::from(*self)
    }
}

impl From<Reg> for u8 {
    fn from(value: Reg) -> Self {
        match value {
            Reg::Rax => 0,
            Reg::Rdx => 2,
            Reg::Rbx => 3,
            Reg::Rdi => 7,
        }
    }
}

#[derive(Debug)]
pub enum RegOrImm {
    Reg(Reg),
    Imm(u64),
}

#[derive(Debug)]
pub enum Instruction {
    Push(Reg),
    Pop(Reg),
    Mov(Reg, RegOrImm),
    Add(Reg, Reg),
    Sub(Reg, Reg),
    Mul(Reg),
    Div(Reg),
    SysCall,
}

impl Writable for Instruction {
    fn byte_len(&self) -> usize {
        match self {
            Self::Push(_) => 1,
            Self::Pop(_) => 1,
            Self::Mov(_, RegOrImm::Reg(_)) => 3,
            Self::Mov(_, RegOrImm::Imm(_)) => 10,
            Self::Add(_, _) => 3,
            Self::Sub(_, _) => 3,
            Self::Mul(_) => 3,
            Self::Div(_) => 3,
            Self::SysCall => 2,
        }
    }

    fn write(&self, out: &mut dyn Write) -> std::io::Result<()> {
        match self {
            Self::Push(reg) => out.write_all(&[0x50 + u8::from(*reg)])?,
            Self::Pop(reg) => out.write_all(&[0x58 + u8::from(*reg)])?,
            Self::Mov(dst, RegOrImm::Imm(src)) => {
                out.write_all(&[REX_W, 0xB8 + u8::from(*dst)])?;
                out.write_all(&src.to_le_bytes())?;
            }
            Self::Mov(dst, RegOrImm::Reg(src)) => {
                out.write_all(&[REX_W, 0x89, dst.mod_rm_reg(src)])?;
            }
            Self::Add(dst, src) => {
                out.write_all(&[REX_W, 0x01, dst.mod_rm_reg(src)])?;
            }
            Self::Sub(dst, src) => {
                out.write_all(&[REX_W, 0x29, dst.mod_rm_reg(src)])?;
            }
            Self::Mul(reg) => {
                out.write_all(&[REX_W, 0xF7, reg.mod_rm_op(4)])?;
            }
            Self::Div(reg) => {
                out.write_all(&[REX_W, 0xF7, reg.mod_rm_op(6)])?;
            }
            Self::SysCall => {
                out.write_all(&[0x0F, 0x05])?;
            }
        }
        Ok(())
    }
}
