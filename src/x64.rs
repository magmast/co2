use std::io::Write;

use crate::Writable;

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    Rax,
    Rdi,
}

#[derive(Debug)]
pub enum Instruction {
    MovImm(Reg, u64),
    MovReg(Reg, Reg),
    SysCall,
}

impl Writable for Instruction {
    fn byte_len(&self) -> usize {
        match self {
            Self::MovImm(_, _) => 4,
            Self::MovReg(_, _) => 3,
            Self::SysCall => 2,
        }
    }

    fn write(&self, out: &mut dyn Write) -> std::io::Result<()> {
        match self {
            Self::MovImm(Reg::Rax, val) => {
                out.write_all(&[0x48, 0xB8])?;
                out.write_all(&val.to_le_bytes())?;
            }
            Self::MovReg(Reg::Rdi, Reg::Rax) => {
                out.write_all(&[0x48, 0x89, 0xC7])?;
            }
            Self::SysCall => {
                out.write_all(&[0x0F, 0x05])?;
            }
            _ => todo!(),
        }
        Ok(())
    }
}
