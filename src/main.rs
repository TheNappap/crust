mod codegen;
mod error;
mod lexer;
mod parser;

fn build() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    let filename = args
        .get(1)
        .ok_or(format!(r#"Error: "{}" expected argument"#, args[0]))?;
    let contents = std::fs::read_to_string(filename)
        .map_err(|_| format!("Error: could not read file: {}", filename))?;

    let syntax_tree = parser::parse(&contents).map_err(|err| err.to_string())?;
    codegen::build(syntax_tree).map_err(|err| err.to_string())?;
    Ok(())
}

fn main() {
    if let Err(error) = build() {
        println!("{}", error);
    }
}
