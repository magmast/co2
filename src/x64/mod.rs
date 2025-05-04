use std::{
    io::{self, Write},
    ops::{Deref, DerefMut},
};

mod raw;

use raw::REX_W;
pub use raw::Reg;

pub struct Encoder<W: Write>(W);

impl<W: Write> Encoder<W> {
    pub fn push<M: Mode>(&mut self, reg: Reg) -> io::Result<()> {
        self.write_prefix::<M>()?;
        self.write_byte(0x50 + u8::from(reg))
    }

    pub fn pop<M: Mode>(&mut self, reg: Reg) -> io::Result<()> {
        self.write_prefix::<M>()?;
        self.write_byte(0x58 + u8::from(reg))
    }

    pub fn mov<M: Mode>(&mut self, dst: impl Into<Dst>, src: impl Into<Src>) -> io::Result<()> {
        self.write_prefix::<M>()?;
        match (src.into(), dst.into()) {
            (Src::Reg(src), Dst::Reg(dst)) => {
                self.write_byte(0x89)?;
                self.write_mod_rm(src, dst)
            }
            (Src::Reg(src), Dst::Mem32(reg, disp)) => {
                self.write_byte(0x89)?;
                self.write_mod_rm(src, (reg, disp))
            }
            (Src::Imm32(src), Dst::Reg(dst)) => {
                self.write_byte(0xC7)?;
                self.write_mod_rm(0, dst)?;
                self.write_all(&src.to_le_bytes())
            }
            (Src::Imm32(src), Dst::Mem32(reg, disp)) => {
                self.write_byte(0xC7)?;
                self.write_mod_rm(0, (reg, disp))?;
                self.write_all(&src.to_le_bytes())
            }
            (Src::Mem32(src_reg, src_disp), Dst::Reg(dst)) => {
                self.write_byte(0x8B)?;
                self.write_mod_rm(dst, (src_reg, src_disp))
            }
            (Src::Mem32(_, _), Dst::Mem32(_, _)) => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Memory-to-memory moves are not supported",
            )),
        }
    }

    pub fn movsxd<M: Mode>(&mut self, dst: Reg, src: Reg) -> io::Result<()> {
        self.write_prefix::<M>()?;
        self.write_all(&[REX_W, 0x63])?;
        self.write_mod_rm(dst, src)
    }

    pub fn jmp(&mut self, disp: i32) -> io::Result<()> {
        self.write_byte(0xE9)?;
        self.write_all(&disp.to_le_bytes())
    }

    pub fn jz(&mut self, disp: i32) -> io::Result<()> {
        self.write_all(&[0x0F, 0x84])?;
        self.write_all(&disp.to_le_bytes())
    }

    pub fn cmp(&mut self, imm: i32) -> io::Result<()> {
        self.write_all(&[0x3D])?;
        self.write_all(&imm.to_le_bytes())
    }

    pub fn add<M: Mode>(&mut self, dst: Reg, src: Reg) -> io::Result<()> {
        self.write_prefix::<M>()?;
        self.write_byte(0x01)?;
        self.write_mod_rm(src, dst)
    }

    pub fn sub<M: Mode>(&mut self, dst: Reg, src: impl Into<Src>) -> io::Result<()> {
        self.write_prefix::<M>()?;
        match src.into() {
            Src::Imm32(src) => {
                self.write_byte(0x81)?;
                self.write_mod_rm(5, dst)?;
                self.write_all(&src.to_le_bytes())
            }
            Src::Reg(src) => {
                self.write_byte(0x29)?;
                self.write_mod_rm(src, dst)
            }
            Src::Mem32(reg, disp) => {
                self.write_byte(0x2B)?;
                self.write_mod_rm(dst, (reg, disp))
            }
        }
    }

    pub fn mul<M: Mode>(&mut self, src: Reg) -> io::Result<()> {
        self.write_prefix::<M>()?;
        self.write_byte(0xF7)?;
        self.write_mod_rm(4, src)
    }

    pub fn imul<M: Mode>(&mut self, src: Reg) -> io::Result<()> {
        self.write_prefix::<M>()?;
        self.write_byte(0xF7)?;
        self.write_mod_rm(5, src)
    }

    pub fn div<M: Mode>(&mut self, src: Reg) -> io::Result<()> {
        self.write_prefix::<M>()?;
        self.write_byte(0xF7)?;
        self.write_mod_rm(6, src)
    }

    pub fn idiv<M: Mode>(&mut self, src: Reg) -> io::Result<()> {
        self.write_prefix::<M>()?;
        self.write_byte(0xF7)?;
        self.write_mod_rm(7, src)
    }

    pub fn neg<M: Mode>(&mut self, reg: Reg) -> io::Result<()> {
        self.write_prefix::<M>()?;
        self.write_byte(0xF7)?;
        self.write_mod_rm(3, reg)
    }

    pub fn sys_call(&mut self) -> io::Result<()> {
        self.write_all(&[0x0F, 0x05])
    }

    fn write_mod_rm(&mut self, reg: impl Into<ModReg>, rm: impl Into<ModRm>) -> io::Result<()> {
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

    fn write_prefix<M: Mode>(&mut self) -> io::Result<()> {
        if let Some(prefix) = M::PREFIX {
            self.write_byte(prefix)
        } else {
            Ok(())
        }
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

pub trait Mode {
    const PREFIX: Option<u8>;
}

pub struct M32;

impl Mode for M32 {
    const PREFIX: Option<u8> = None;
}

pub struct M64;

impl Mode for M64 {
    const PREFIX: Option<u8> = Some(REX_W);
}

pub enum Src {
    Reg(Reg),
    Imm32(i32),
    Mem32(Reg, i32),
}

impl From<Reg> for Src {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<i32> for Src {
    fn from(value: i32) -> Self {
        Self::Imm32(value)
    }
}

impl From<(Reg, i32)> for Src {
    fn from((reg, disp): (Reg, i32)) -> Self {
        Self::Mem32(reg, disp)
    }
}

pub enum Dst {
    Reg(Reg),
    Mem32(Reg, i32),
}

impl From<Reg> for Dst {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<(Reg, i32)> for Dst {
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
