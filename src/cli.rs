use std::{fs, io::BufWriter, os::unix::fs::PermissionsExt, path::PathBuf};

use anyhow::{Context, Result, anyhow};
use bumpalo::Bump;
use clap::Parser;

use crate::{
    ast::File,
    comp::Compiler,
    elf::{self, Elf, Segment, SegmentFlags},
};

#[derive(Parser)]
pub struct Cli {
    #[clap(help = "Path to the source file")]
    file: PathBuf,
}

impl Cli {
    pub fn run() -> Result<()> {
        let cli = Self::parse();

        let code = fs::read_to_string(&cli.file)?;

        let asm = {
            let bump = Bump::new();

            let file = File::parse(&bump, &code)
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
                    .data(&asm)
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
}
