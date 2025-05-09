use std::{
    fmt::Debug,
    io::{self, Write},
    mem,
};

use bitflags::bitflags;
use bon::bon;
use bytemuck::Zeroable;
use itertools::Itertools;
use raw::{Header, Ident, ProgramHeader, SectionFlags, SectionHeader};

pub mod raw;

pub const MAGIC: [u8; 4] = [0x7F, b'E', b'L', b'F'];

const SHSTRTAB_NAME: &str = ".shstrtab";

pub const BASE_ADDR: u64 = 0x0040_0000;

fn align_up(addr: u64, align: u64) -> u64 {
    (addr + align - 1) & !(align - 1)
}

pub struct Elf<'data> {
    ty: Type,
    machine: u16,
    segments: Vec<Segment<'data>>,
}

#[bon]
impl<'data> Elf<'data> {
    #[builder]
    pub fn new(
        ty: Option<Type>,
        #[builder(default = 0x3E)] machine: u16,
        segments: Vec<Segment<'data>>,
    ) -> Result<Self, Error> {
        Self::verify_segments(&segments)?;

        Ok(Self {
            ty: ty.unwrap_or_else(|| {
                if segments.iter().any(|s| s.entry) {
                    Type::Executable
                } else if segments.iter().any(|s| s.ty == SegmentType::Interp) {
                    Type::Dynamic
                } else {
                    Type::Relocatable
                }
            }),
            machine,
            segments,
        })
    }

    fn verify_segments(segments: &[Segment<'data>]) -> Result<(), Error> {
        Self::verify_interp_order(segments)?;

        if segments.is_empty() {
            Err(Error::MissingSegments)
        } else if let Some(name) = segments
            .iter()
            .filter_map(|s| s.name.as_ref())
            .counts()
            .into_iter()
            .find(|(_, count)| *count > 1)
            .map(|(name, _)| name.clone())
        {
            Err(Error::DuplicatedNames(name))
        } else if segments.len() > u16::MAX.into() {
            Err(Error::SegmentCount)
        } else if segments
            .iter()
            .filter(|s| s.ty == SegmentType::Interp)
            .count()
            > 1
        {
            Err(Error::DuplicatedInterp)
        } else if segments.iter().filter(|s| s.entry).count() > 1 {
            Err(Error::DuplicatedEntry)
        } else if segments
            .iter()
            .tuple_combinations()
            .any(|(s1, s2)| s1.overlaps(s2))
        {
            Err(Error::Overlap)
        } else {
            Ok(())
        }
    }

    fn verify_interp_order(segments: &[Segment<'data>]) -> Result<(), Error> {
        let first_interp_index = segments
            .iter()
            .enumerate()
            .find(|(_, s)| s.ty == SegmentType::Interp)
            .map(|(i, _)| i);

        let first_load_index = segments
            .iter()
            .enumerate()
            .find(|(_, s)| s.ty == SegmentType::Load)
            .map(|(i, _)| i);

        match (first_interp_index, first_load_index) {
            (Some(interp), Some(load)) if interp > load => Err(Error::InterpOrder),
            _ => Ok(()),
        }
    }

    pub fn write(&self, out: &mut dyn Write) -> io::Result<()> {
        let segments: Vec<_> = self.alloc_segments();

        let mut sections = vec![SectionHeader::zeroed()];
        let mut sh_name_offset = 1;
        for (s, ph) in segments.iter() {
            if let Some(name) = s.name.as_ref() {
                sections.push(SectionHeader {
                    sh_name: sh_name_offset as u32,
                    sh_type: if s.entry { 1 } else { todo!() },
                    sh_flags: if s.entry {
                        SectionFlags::SHF_ALLOC | SectionFlags::SHF_EXECINSTR
                    } else {
                        todo!()
                    },
                    sh_addr: ph.p_vaddr,
                    sh_offset: ph.p_offset,
                    sh_size: ph.p_filesz,
                    sh_link: 0,
                    sh_info: 0,
                    sh_addralign: ph.p_align,
                    sh_entsize: 0,
                });
                sh_name_offset += name.len() + 1;
            }
        }

        let shstrtab_off = segments
            .last()
            .map(|(_, ph)| ph.p_offset + ph.p_filesz)
            .unwrap();

        sections.push(SectionHeader {
            sh_name: sh_name_offset as u32,
            sh_type: 3,
            sh_flags: SectionFlags::empty(),
            sh_addr: 0,
            sh_offset: shstrtab_off,
            sh_size: (sh_name_offset + SHSTRTAB_NAME.len() + 1) as u64,
            sh_link: 0,
            sh_info: 0,
            sh_addralign: 1,
            sh_entsize: 0,
        });
        sh_name_offset += SHSTRTAB_NAME.len() + 1;

        let header = Header {
            e_ident: Ident {
                ei_mag: MAGIC,
                ei_class: 2,
                ei_data: 1,
                ei_version: 1,
                ei_osabi: 0,
                ei_abiversion: 0,
                ei_pad: [0; 7],
            },
            e_type: self.ty.into(),
            e_machine: self.machine,
            e_version: 1, // EV_CURRENT
            e_entry: segments
                .iter()
                .find(|(s, _)| s.entry)
                .map(|(_, h)| h.p_vaddr)
                .unwrap_or_default(),
            e_phoff: mem::size_of::<Header>() as u64,
            e_shoff: shstrtab_off + sh_name_offset as u64,
            e_flags: 0,
            e_ehsize: mem::size_of::<Header>() as u16,
            e_phentsize: mem::size_of::<ProgramHeader>() as u16,
            e_phnum: segments.len() as u16,
            e_shentsize: mem::size_of::<SectionHeader>() as u16,
            e_shnum: sections.len() as u16,
            e_shstrndx: (sections.len() - 1) as u16,
        };

        let mut pos = 0;

        out.write_all(bytemuck::bytes_of(&header))?;
        pos += mem::size_of::<Header>();

        for (_, ph) in &segments {
            out.write_all(bytemuck::bytes_of(ph))?;
            pos += mem::size_of::<ProgramHeader>();
        }

        for (s, ph) in &segments {
            let pad = vec![0; (ph.p_offset as usize).saturating_sub(pos)];
            out.write_all(&pad)?;
            pos += pad.len();

            out.write_all(s.data)?;
            pos += ph.p_filesz as usize;
        }

        // First null string of shstrtab section data
        out.write_all(&[0])?;

        for name in segments.iter().filter_map(|(s, _)| s.name.as_ref()) {
            out.write_all(name.as_bytes())?;
            out.write_all(&[0])?;
        }

        out.write_all(SHSTRTAB_NAME.as_bytes())?;
        out.write_all(&[0])?;

        for sh in sections {
            out.write_all(bytemuck::bytes_of(&sh))?;
        }

        Ok(())
    }

    fn alloc_segments(&self) -> Vec<(&Segment<'data>, ProgramHeader)> {
        self.segments
            .iter()
            .scan(
                (
                    (mem::size_of::<Header>()
                        + mem::size_of::<ProgramHeader>() * self.segments.len())
                        as u64,
                    BASE_ADDR,
                    BASE_ADDR,
                ),
                |(pfend, pvoff, ppoff), s| {
                    let filesz = s.data.len() as u64;
                    let memsz = s.mem_size.unwrap_or(filesz);

                    let offset = align_up(*pfend, s.alignment);
                    *pfend = offset + filesz;

                    let vaddr = s
                        .virtual_address
                        .unwrap_or_else(|| align_up(*pvoff, s.alignment));
                    *pvoff = vaddr + memsz;

                    let paddr = s
                        .physical_address
                        .unwrap_or_else(|| align_up(*ppoff, s.alignment));
                    *ppoff = paddr + memsz;

                    Some((
                        s,
                        ProgramHeader {
                            p_type: s.ty.into(),
                            p_flags: s.flags.bits(),
                            p_offset: offset,
                            p_vaddr: vaddr,
                            p_paddr: paddr,
                            p_filesz: filesz,
                            p_memsz: memsz,
                            p_align: s.alignment,
                        },
                    ))
                },
            )
            .collect()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    None,
    Relocatable,
    Executable,
    Dynamic,
    Core,
    Os(OsType),
    Proc(ProcType),
}

impl From<Type> for u16 {
    fn from(value: Type) -> Self {
        match value {
            Type::None => 0x00,
            Type::Relocatable => 0x01,
            Type::Executable => 0x02,
            Type::Dynamic => 0x03,
            Type::Core => 0x04,
            Type::Os(OsType(v)) => v,
            Type::Proc(ProcType(v)) => v,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OsType(u16);

impl TryFrom<u16> for OsType {
    type Error = RangeError<u16>;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        let start = 0xFE00;
        let end = 0xFEFF;
        if value >= start && value <= end {
            Ok(Self(value))
        } else {
            Err(RangeError { start, end, value })
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ProcType(u16);

impl TryFrom<u16> for ProcType {
    type Error = RangeError<u16>;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        let start = 0xFF00;
        let end = 0xFFFF;
        if value >= start && value <= end {
            Ok(Self(value))
        } else {
            Err(RangeError { start, end, value })
        }
    }
}

pub struct Segment<'data> {
    name: Option<String>,
    ty: SegmentType,
    flags: SegmentFlags,
    virtual_address: Option<u64>,
    physical_address: Option<u64>,
    mem_size: Option<u64>,
    alignment: u64,
    data: &'data [u8],
    entry: bool,
}

#[bon]
impl<'data> Segment<'data> {
    #[builder]
    pub fn new(
        /// Name of a section header for this segment.
        ///
        /// If it's `None` then:
        ///
        /// - If the segment is marked as entry: it's set to ".text"
        /// - Otherwise: No section header is generated for the section
        name: Option<String>,
        #[builder(default = SegmentType::Load)] ty: SegmentType,
        flags: SegmentFlags,
        virtual_address: Option<u64>,
        physical_address: Option<u64>,
        mem_size: Option<u64>,
        #[builder(default = 0x1000)] alignment: u64,
        data: &'data [u8],
        #[builder(default)] entry: bool,
    ) -> Result<Self, Error> {
        if name
            .as_ref()
            .map(|n| n == SHSTRTAB_NAME)
            .unwrap_or_default()
        {
            Err(Error::ReservedName(name.unwrap().to_string()))
        } else if mem_size.map(|s| s < data.len() as u64).unwrap_or_default() {
            Err(Error::InsufficientMemSize)
        } else if !alignment.is_power_of_two() {
            Err(Error::Alignment)
        } else if entry && !flags.contains(SegmentFlags::EXEC) {
            Err(Error::NonExecutableEntry)
        } else {
            Ok(Self {
                name: name.or_else(|| entry.then(|| ".text".into())),
                ty,
                flags,
                virtual_address,
                physical_address,
                mem_size,
                alignment,
                data,
                entry,
            })
        }
    }

    fn overlaps(&self, other: &Segment) -> bool {
        self.overlaps_using(other, |s| s.virtual_address)
            || self.overlaps_using(other, |s| s.physical_address)
    }

    fn overlaps_using(
        &self,
        other: &Segment,
        mut get_addr: impl FnMut(&Segment) -> Option<u64>,
    ) -> bool {
        let s1 = get_addr(self);
        let s2 = get_addr(self);
        match (s1, s2) {
            (Some(s1), Some(s2)) => {
                let e1 = self.mem_size.unwrap_or(self.data.len() as u64);
                let e2 = other.mem_size.unwrap_or(other.data.len() as u64);
                e1 > s2 && s1 < e2
            }
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SegmentType {
    Null,
    Load,
    Dynamic,
    Interp,
    Note,
    Shlib,
    Phdr,
    Tls,
    Os(OsSegmentType),
    Proc(ProcSegmentType),
}

impl From<SegmentType> for u32 {
    fn from(value: SegmentType) -> Self {
        match value {
            SegmentType::Null => 0x00,
            SegmentType::Load => 0x01,
            SegmentType::Dynamic => 0x02,
            SegmentType::Interp => 0x03,
            SegmentType::Note => 0x04,
            SegmentType::Shlib => 0x05,
            SegmentType::Phdr => 0x06,
            SegmentType::Tls => 0x07,
            SegmentType::Os(OsSegmentType(v)) => v,
            SegmentType::Proc(ProcSegmentType(v)) => v,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct OsSegmentType(u32);

impl TryFrom<u32> for OsSegmentType {
    type Error = RangeError<u32>;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let start = 0x6000_0000;
        let end = 0x6FFFFFFF;
        if value >= start && value <= end {
            Ok(OsSegmentType(value))
        } else {
            Err(RangeError { start, end, value })
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ProcSegmentType(u32);

impl TryFrom<u32> for ProcSegmentType {
    type Error = RangeError<u32>;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let start = 0x70000000;
        let end = 0x7FFFFFFF;
        if value >= start && value <= end {
            Ok(Self(value))
        } else {
            Err(RangeError { start, end, value })
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Value {value} is not within {start}..={end} range")]
pub struct RangeError<T> {
    start: T,
    end: T,
    value: T,
}

bitflags! {
    #[derive(Debug)]
    pub struct SegmentFlags: u32 {
        const EXEC = 0x1;
        const WRITE = 0x2;
        const READ = 0x4;
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Segments overlap")]
    Overlap,
    #[error("Mem size is smaller than file size")]
    InsufficientMemSize,
    #[error("Only one interp segment is allowed")]
    DuplicatedInterp,
    #[error("Interp segment must appear before first load segment")]
    InterpOrder,
    #[error("Alignment must be a power of two")]
    Alignment,
    #[error("Only one entry segment is allowed")]
    DuplicatedEntry,
    #[error("Entry segment must be executable")]
    NonExecutableEntry,
    #[error("Number of segments must fit within u16")]
    SegmentCount,
    #[error("Name {0} is used by multiple segments")]
    DuplicatedNames(String),
    #[error("Name {0} is reserved and cannot be used as a segment name")]
    ReservedName(String),
    #[error("At least one segment is required")]
    MissingSegments,
}
