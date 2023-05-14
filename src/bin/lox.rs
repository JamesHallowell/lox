use lox::Interpreter;

fn main() {
    let std_in = std::io::stdin();
    let mut input = String::new();

    let mut interpreter = Interpreter::default();
    loop {
        std_in.read_line(&mut input).unwrap();

        let result = interpreter.interpret(&input);
        match result {
            Ok(value) => println!("{value}"),
            Err(err) => println!("{err:?}"),
        }

        input.clear();
    }
}
