use std::{
    io::{self, Write},
    ops::{Deref, DerefMut},
};

mod raw;

use raw::REX_W;
pub use raw::Reg;

pub struct Encoder<W: Write>(W);

impl<W: Write> Encoder<W> {
    pub fn push(&mut self, reg: Reg) -> io::Result<()> {
        self.write_byte(0x50 + u8::from(reg))
    }

    pub fn pop(&mut self, reg: Reg) -> io::Result<()> {
        self.write_byte(0x58 + u8::from(reg))
    }

    pub fn mov(&mut self, src: impl Into<Src>, dst: impl Into<MovDst>) -> io::Result<()> {
        let src = src.into();
        let dst = dst.into();

        match (src, dst) {
            (Src::Reg(src), MovDst::Reg(dst)) => {
                self.write_all(&[REX_W, 0x89])?;
                self.mod_rm(src, dst)
            }
            (Src::Reg(src), MovDst::Mem32(reg, disp)) => {
                self.write_all(&[REX_W, 0x89])?;
                self.mod_rm(src, (reg, disp))
            }
            (Src::Imm(src), MovDst::Reg(dst)) => {
                self.write_all(&[REX_W, 0xC7])?;
                self.mod_rm(0, dst)?;
                self.write_all(&src.to_le_bytes())
            }
            (Src::Imm(src), MovDst::Mem32(reg, disp)) => {
                self.write_all(&[REX_W, 0xC7])?;
                self.mod_rm(0, (reg, disp))?;
                self.write_all(&src.to_le_bytes())
            }
            (Src::Mem32(src_reg, src_disp), MovDst::Reg(dst)) => {
                self.write_all(&[REX_W, 0x8B])?;
                self.mod_rm(dst, (src_reg, src_disp))
            }
            (Src::Mem32(_, _), MovDst::Mem32(_, _)) => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Memory-to-memory moves are not supported",
            )),
        }
    }

    pub fn add(&mut self, lhs: Reg, rhs: Reg) -> io::Result<()> {
        self.write_all(&[REX_W, 0x01])?;
        self.mod_rm(rhs, lhs)
    }

    pub fn sub(&mut self, lhs: Reg, rhs: impl Into<Src>) -> io::Result<()> {
        match rhs.into() {
            Src::Imm(rhs) => {
                self.write_all(&[REX_W, 0x81])?;
                self.mod_rm(5, lhs)?;
                self.write_all(&rhs.to_le_bytes())
            }
            Src::Reg(rhs) => {
                self.write_all(&[REX_W, 0x29])?;
                self.mod_rm(rhs, lhs)
            }
            Src::Mem32(reg, disp) => {
                self.write_all(&[REX_W, 0x2B])?;
                self.mod_rm(lhs, (reg, disp))
            }
        }
    }

    pub fn mul(&mut self, rhs: Reg) -> io::Result<()> {
        self.write_all(&[REX_W, 0xF7])?;
        self.mod_rm(4, rhs)
    }

    pub fn imul(&mut self, rhs: Reg) -> io::Result<()> {
        self.write_all(&[REX_W, 0xF7])?;
        self.mod_rm(5, rhs)
    }

    pub fn div(&mut self, rhs: Reg) -> io::Result<()> {
        self.write_all(&[REX_W, 0xF7])?;
        self.mod_rm(6, rhs)
    }

    pub fn idiv(&mut self, rhs: Reg) -> io::Result<()> {
        self.write_all(&[REX_W, 0xF7])?;
        self.mod_rm(7, rhs)
    }

    pub fn sys_call(&mut self) -> io::Result<()> {
        self.write_all(&[0x0F, 0x05])
    }

    fn mod_rm(&mut self, reg: impl Into<ModReg>, rm: impl Into<ModRm>) -> io::Result<()> {
        let reg = reg.into();
        let rm = rm.into();

        let reg_bits = match reg {
            ModReg::Reg(r) => u8::from(r),
            ModReg::Op(op) => op & 0b111,
        };

        match rm {
            ModRm::Reg(rm_reg) => {
                let rm_bits = u8::from(rm_reg);
                self.write_byte((0b11 << 6) | (reg_bits << 3) | rm_bits)
            }
            ModRm::Mem32(base, disp) => {
                let rm_bits = u8::from(base);
                self.write_byte((0b10 << 6) | (reg_bits << 3) | rm_bits)?;
                self.write_all(&disp.to_le_bytes())
            }
        }
    }

    fn write_byte(&mut self, byte: u8) -> io::Result<()> {
        self.write_all(&[byte])
    }
}

impl<W: Write> Deref for Encoder<W> {
    type Target = W;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<W: Write> DerefMut for Encoder<W> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<W: Write> From<W> for Encoder<W> {
    fn from(value: W) -> Self {
        Self(value)
    }
}

pub enum Src {
    Reg(Reg),
    Imm(i32),
    Mem32(Reg, i32),
}

impl From<Reg> for Src {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<i32> for Src {
    fn from(value: i32) -> Self {
        Self::Imm(value)
    }
}

impl From<(Reg, i32)> for Src {
    fn from((reg, disp): (Reg, i32)) -> Self {
        Self::Mem32(reg, disp)
    }
}

pub enum MovDst {
    Reg(Reg),
    Mem32(Reg, i32),
}

impl From<Reg> for MovDst {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<(Reg, i32)> for MovDst {
    fn from((reg, disp): (Reg, i32)) -> Self {
        Self::Mem32(reg, disp)
    }
}

enum ModReg {
    Reg(Reg),
    Op(u8),
}

impl From<Reg> for ModReg {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<u8> for ModReg {
    fn from(value: u8) -> Self {
        Self::Op(value)
    }
}

enum ModRm {
    Reg(Reg),
    Mem32(Reg, i32),
}

impl From<Reg> for ModRm {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<(Reg, i32)> for ModRm {
    fn from((reg, disp): (Reg, i32)) -> Self {
        Self::Mem32(reg, disp)
    }
}
