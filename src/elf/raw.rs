use bitflags::bitflags;
use bytemuck::{Pod, Zeroable};

#[derive(Debug, Clone, Copy, Pod, Zeroable)]
#[repr(C)]
pub struct Header {
    pub e_ident: Ident,
    pub e_type: u16,
    pub e_machine: u16,
    pub e_version: u32,
    pub e_entry: u64,
    pub e_phoff: u64,
    pub e_shoff: u64,
    pub e_flags: u32,
    pub e_ehsize: u16,
    pub e_phentsize: u16,
    pub e_phnum: u16,
    pub e_shentsize: u16,
    pub e_shnum: u16,
    pub e_shstrndx: u16,
}

#[derive(Debug, Clone, Copy, Pod, Zeroable)]
#[repr(C)]
pub struct Ident {
    pub ei_mag: [u8; 4],
    pub ei_class: u8,
    pub ei_data: u8,
    pub ei_version: u8,
    pub ei_osabi: u8,
    pub ei_abiversion: u8,
    pub ei_pad: [u8; 7],
}

#[derive(Debug, Clone, Copy, Pod, Zeroable)]
#[repr(C)]
pub struct ProgramHeader {
    pub p_type: u32,
    pub p_flags: u32,
    pub p_offset: u64,
    pub p_vaddr: u64,
    pub p_paddr: u64,
    pub p_filesz: u64,
    pub p_memsz: u64,
    pub p_align: u64,
}

#[derive(Debug, Clone, Copy, Pod, Zeroable)]
#[repr(C)]
pub struct SectionHeader {
    pub sh_name: u32,
    pub sh_type: u32,
    pub sh_flags: SectionFlags,
    pub sh_addr: u64,
    pub sh_offset: u64,
    pub sh_size: u64,
    pub sh_link: u32,
    pub sh_info: u32,
    pub sh_addralign: u64,
    pub sh_entsize: u64,
}

bitflags! {
    #[derive(Debug, Pod, Zeroable, Clone, Copy)]
    #[repr(C)]
    pub struct SectionFlags: u64 {
        const SHF_WRITE = 0x01;
        const SHF_ALLOC = 0x02;
        const SHF_EXECINSTR = 0x04;
        const SHF_MERGE = 0x10;
        const SHF_STRINGS = 0x20;
        const SHF_INFO_LINK = 0x40;
        const SHF_LINK_ORDER = 0x80;
        const SHF_OS_NONCONFORMING = 0x100;
        const SHF_GROUP = 0x200;
        const SHF_TLS = 0x400;
        const SHF_MASKOS = 0x0FF0_0000;
        const SHF_MASKPROC =0xF000_0000;
        const SHF_ORDERED = 0x0400_0000;
        const SHF_EXCLUDE = 0x0800_0000;
    }
}
