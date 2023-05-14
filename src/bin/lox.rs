use lox::Interpreter;

fn main() {
    std::env::args()
        .nth(1)
        .map(run_program)
        .unwrap_or_else(repl);
}

fn run_program(filename: String) {
    let program = std::fs::read_to_string(filename).expect("invalid filename");
    let mut interpreter = Interpreter::default();
    let result = interpreter.interpret(&program);

    if let Err(err) = result {
        eprintln!("{err:?}");
    }
}

fn repl() {
    let std_in = std::io::stdin();
    let mut input = String::new();

    let mut interpreter = Interpreter::default();
    loop {
        std_in.read_line(&mut input).unwrap();

        if let Err(err) = interpreter.interpret(&input) {
            eprintln!("{err:?}");
        }

        input.clear();
    }
}
