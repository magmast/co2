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
int fib(int i) {
    if (i == 0) {
        return 1;
    }
    if (i == 1) {
        return 1;
    }
    int left = fib(i - 2);
    int right = fib(i - 1);
    return left + right;
}

int main() {
    return fib(10);
}
";

fn main() -> Result<()> {
    let code = {
        let bump = Bump::new();

        let file = File::parse(&bump, CODE)
            .map_err(|err| anyhow!("{err}"))
            .context("Parsing failed")?;

        Compiler::default()
            .compile(elf::BASE_ADDR, &file)
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
