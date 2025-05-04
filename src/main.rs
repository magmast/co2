use core::str;
use std::{fs::File, io::BufWriter, os::unix::fs::PermissionsExt};

use anyhow::{Context, Result, anyhow};
use bumpalo::Bump;
use co2::{
    comp::Compiler,
    elf::{Elf, Segment, SegmentFlags},
    parser,
};
use winnow::Parser;

const CODE: &str = "
int main() {
    int ret = 0;
    if (0) ret = 1;
    else if (0) {
        int a = 2;
        ret = a;
    }
    else ret = 3;
    return ret;
}
";

fn main() -> Result<()> {
    let mut code = vec![];

    {
        let bump = Bump::new();

        let func = parser::file(&bump)
            .parse(CODE)
            .map_err(|err| anyhow!("{err}"))
            .context("Parsing failed")?
            .into_iter()
            .next()
            .context("No functions are defined")?;

        Compiler::from(&mut code)
            .compile(&func)
            .context("Compilation failed")?;
    };

    let elf = Elf::builder()
        .segments(vec![
            Segment::builder()
                .flags(SegmentFlags::READ | SegmentFlags::EXEC)
                .data(&code)
                .entry(true)
                .build()?,
        ])
        .build()?;

    let f = File::create("a.out").context("Failed to create file for the program")?;
    f.metadata()?.permissions().set_mode(0o755);
    let mut f = BufWriter::new(f);
    elf.write(&mut f)?;

    Ok(())
}
