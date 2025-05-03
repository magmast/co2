use core::str;
use std::{fs::File, io::BufWriter};

use anyhow::{Context, Result, anyhow};
use bumpalo::Bump;
use co2::{
    Writable,
    elf::{Elf, Segment, SegmentFlags},
    parser,
};
use winnow::Parser;

const CODE: &str = "
int main() {
    return 8;
}
";

fn main() -> Result<()> {
    let bump = Bump::new();

    let func = parser::file(&bump)
        .parse(CODE)
        .map_err(|err| anyhow!("{err}"))
        .context("Parsing failed")?
        .into_iter()
        .next()
        .context("No functions are defined")?;

    let instructions = co2::compile(&func);

    let elf = Elf::builder()
        .segments(vec![
            Segment::builder()
                .flags(SegmentFlags::READ | SegmentFlags::EXEC)
                .data(&instructions)
                .entry(true)
                .build()?,
        ])
        .build()?;

    let mut f =
        BufWriter::new(File::create("a.out").context("Failed to create file for the program")?);
    elf.write(&mut f)?;

    Ok(())
}
