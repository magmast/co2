use anyhow::{Result, anyhow};
use bumpalo::Bump;
use co2::{eval::Interpreter, parser};
use winnow::Parser;

const CODE: &str = "
int magic_number() {
    return 7 * 5 + 2;
}

void side_effect(void) {}

int main(void) {
    return 2 + 2 * 2;
}
";

fn main() -> Result<()> {
    let bump = Bump::new();
    let file = parser::file(&bump)
        .parse(CODE)
        .map_err(|err| anyhow!("{err}"))?;
    let interpreter = Interpreter::default();
    let result = interpreter.eval(&file)?;
    println!("{}", result);
    Ok(())
}
