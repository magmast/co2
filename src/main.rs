use core::str;
use std::{
    fs::{self},
    io::BufWriter,
    os::unix::fs::PermissionsExt,
};

use anyhow::{Context, Result, anyhow};
use bumpalo::Bump;
use co2::{
    ast::File,
    comp::Compiler,
    elf::{self, Elf, Segment, SegmentFlags},
};

const CODE: &str = "
int main() {
    int ret = 0;
    if (0) ret = 1;
    else if (1) {
        int a = 2;
        ret = a * 4 / 2;
    }
    else ret = 3;
    return ret;
}
";

fn main() -> Result<()> {
    let code = {
        let bump = Bump::new();

        let func = File::parse(&bump, CODE)
            .map_err(|err| anyhow!("{err}"))
            .context("Parsing failed")?
            .funcs
            .into_iter()
            .next()
            .context("No functions are defined")?;

        Compiler::default()
            .compile(elf::BASE_ADDR, &func)
            .context("Compilation failed")?
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

    let f = fs::File::create("a.out").context("Failed to create file for the program")?;
    f.metadata()?.permissions().set_mode(0o755);
    let mut f = BufWriter::new(f);
    elf.write(&mut f)?;

    Ok(())
}
