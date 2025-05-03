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
    4 * 8;
    return (2 + 2) * 2;
}
";

fn main() -> Result<()> {
    let instructions = {
        let bump = Bump::new();

        let func = parser::file(&bump)
            .parse(CODE)
            .map_err(|err| anyhow!("{err}"))
            .context("Parsing failed")?
            .into_iter()
            .next()
            .context("No functions are defined")?;

        let mut instructions = Vec::new();
        co2::compile(&func, &mut instructions);

        instructions
    };

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
