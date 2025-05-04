use bytemuck::Contiguous;

pub const REX_W: u8 = 0x48;

#[derive(Debug, Clone, Copy, Contiguous)]
#[repr(u8)]
pub enum Reg {
    Ax,
    Cx,
    Dx,
    Bx,
    Sp,
    Bp,
    Si,
    Di,
}

impl From<Reg> for u8 {
    fn from(value: Reg) -> Self {
        value.into_integer()
    }
}
