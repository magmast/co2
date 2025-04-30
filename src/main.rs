use anyhow::{Result, anyhow};
use bumpalo::Bump;
use co2::parser;
use winnow::Parser;

fn main() -> Result<()> {
    let bump = Bump::new();
    let ast = parser::expr(&bump)
        .parse("(2 + 2) * 2")
        .map_err(|err| anyhow!("{err}"))?;
    println!("{:#?}", ast);
    Ok(())
}
