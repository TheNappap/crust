mod codegen;
mod lexer;
mod parser;
mod type_check;
mod std_lib;
mod utils;

fn build() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    let filename = args
        .get(1)
        .ok_or(format!(r#"Error: "{}" expected argument"#, args[0]))?;
    let contents = std::fs::read_to_string(filename)
        .map_err(|_| format!("Error: could not read file: {}", filename))?;

    let mut syntax_tree = parser::parse(&contents).map_err(|err| err.to_string())?;
    syntax_tree.add_lib(&std_lib::StdLib::new());
    type_check::type_check(&mut syntax_tree).map_err(|err| err.to_string())?;
    codegen::build(syntax_tree).map_err(|err| err.to_string())?;
    Ok(())
}

fn main() {
    if let Err(error) = build() {
        println!("{}", error);
    }
}
