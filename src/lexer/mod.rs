#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Literal(Literal),
    Keyword(Keyword),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Identifier(String),
    String(String),
    Number(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut chars = input.chars().peekable();
    let mut tokens = vec![];

    while let Some(char) = chars.next() {
        let token = match char {
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            '{' => Some(Token::LeftBrace),
            '}' => Some(Token::RightBrace),
            ',' => Some(Token::Comma),
            '.' => Some(Token::Dot),
            '-' => Some(Token::Minus),
            '+' => Some(Token::Plus),
            ';' => Some(Token::Semicolon),
            '*' => Some(Token::Star),
            '!' => match chars.next_if_eq(&'=') {
                Some(_) => Some(Token::BangEqual),
                None => Some(Token::Bang),
            },
            '=' => match chars.next_if_eq(&'=') {
                Some(_) => Some(Token::EqualEqual),
                None => Some(Token::Equal),
            },
            '<' => match chars.next_if_eq(&'=') {
                Some(_) => Some(Token::LessEqual),
                None => Some(Token::Less),
            },
            '>' => match chars.next_if_eq(&'=') {
                Some(_) => Some(Token::GreaterEqual),
                None => Some(Token::Greater),
            },
            '/' => match chars.next_if_eq(&'/') {
                Some(_) => {
                    while chars.next_if(|&char| char != '\n').is_some() {}
                    None
                }
                None => Some(Token::Slash),
            },
            '"' => {
                let mut value = String::new();
                loop {
                    match chars.next() {
                        Some('"') => break Some(Token::Literal(Literal::String(value))),
                        Some('\n') => {
                            // multi-line string
                        }
                        Some(char) => {
                            value.push(char);
                        }
                        None => {
                            break None;
                        }
                    }
                }
            }
            char if char.is_numeric() => {
                let mut value = String::from(char);

                while let Some(char) = chars.next_if(|char| char.is_numeric()) {
                    value.push(char);
                }

                if let Some('.') = chars.peek() {
                    let is_dot_followed_by_more_numbers = chars
                        .clone()
                        .nth(1)
                        .map(|char| char.is_numeric())
                        .unwrap_or(false);

                    if is_dot_followed_by_more_numbers {
                        value.push(chars.next().unwrap());
                        while let Some(char) = chars.next_if(|char| char.is_numeric()) {
                            value.push(char);
                        }
                    }
                }

                value
                    .parse::<f64>()
                    .map(Literal::Number)
                    .map(Token::Literal)
                    .ok()
            }
            char if char.is_ascii_alphabetic() || char == '_' => {
                let mut value = String::from(char);
                loop {
                    match chars.next() {
                        Some(char) if char.is_alphanumeric() => {
                            value.push(char);
                        }
                        _ => {
                            break Some(match value.as_str() {
                                "and" => Token::Keyword(Keyword::And),
                                "class" => Token::Keyword(Keyword::Class),
                                "else" => Token::Keyword(Keyword::Else),
                                "false" => Token::Keyword(Keyword::False),
                                "fun" => Token::Keyword(Keyword::Fun),
                                "for" => Token::Keyword(Keyword::For),
                                "if" => Token::Keyword(Keyword::If),
                                "nil" => Token::Keyword(Keyword::Nil),
                                "or" => Token::Keyword(Keyword::Or),
                                "print" => Token::Keyword(Keyword::Print),
                                "return" => Token::Keyword(Keyword::Return),
                                "super" => Token::Keyword(Keyword::Super),
                                "this" => Token::Keyword(Keyword::This),
                                "true" => Token::Keyword(Keyword::True),
                                "var" => Token::Keyword(Keyword::Var),
                                "while" => Token::Keyword(Keyword::While),
                                _ => Token::Literal(Literal::Identifier(value)),
                            })
                        }
                    }
                }
            }
            char if char.is_whitespace() => None,
            _ => None,
        };

        if let Some(token) = token {
            tokens.push(token);
        }
    }

    tokens
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn can_scan_single_character_tokens() {
        let tokens = lex("(){},.-+;*");

        assert_eq!(
            tokens,
            vec![
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBrace,
                Token::RightBrace,
                Token::Comma,
                Token::Dot,
                Token::Minus,
                Token::Plus,
                Token::Semicolon,
                Token::Star
            ]
        );
    }

    #[test]
    fn can_distinguish_between_single_and_multi_character_operators() {
        let tokens = lex("! != = == < <= > >= / //");

        assert_eq!(
            tokens,
            vec![
                Token::Bang,
                Token::BangEqual,
                Token::Equal,
                Token::EqualEqual,
                Token::Less,
                Token::LessEqual,
                Token::Greater,
                Token::GreaterEqual,
                Token::Slash
            ]
        );
    }

    #[test]
    fn all_tokens_following_a_comment_are_ignored() {
        let tokens = lex("// () {}\n+");

        assert_eq!(tokens, vec![Token::Plus]);
    }

    #[test]
    fn can_scan_string_literals() {
        let tokens = lex("\"Hello, world!\"");

        assert_eq!(
            tokens,
            vec![Token::Literal(Literal::String("Hello, world!".to_string()))]
        );
    }

    #[test]
    fn can_scan_empty_string_literals() {
        let tokens = lex("\"\"");

        assert_eq!(
            tokens,
            vec![Token::Literal(Literal::String("".to_string()))]
        );
    }

    #[test]
    fn can_scan_unterminated_string_literals() {
        let tokens = lex("\"Hello, wor");

        assert!(tokens.is_empty());
    }

    #[test]
    fn can_scan_multi_line_string_literals() {
        let tokens = lex("\"Hello, wor\nld!\"");

        assert_eq!(
            tokens,
            vec![Token::Literal(Literal::String("Hello, world!".to_string()))]
        );
    }

    #[test]
    fn can_scan_number_literals() {
        let tokens = lex("1 1234 12.34 .1234 1234.");

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::Number(1.0)),
                Token::Literal(Literal::Number(1234.0)),
                Token::Literal(Literal::Number(12.34)),
                Token::Dot,
                Token::Literal(Literal::Number(1234.0)),
                Token::Literal(Literal::Number(1234.0)),
                Token::Dot
            ]
        );
    }

    #[test]
    fn can_scan_identifiers_and_keywords() {
        let tokens = lex("hello and class while value");

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::Identifier("hello".to_string())),
                Token::Keyword(Keyword::And),
                Token::Keyword(Keyword::Class),
                Token::Keyword(Keyword::While),
                Token::Literal(Literal::Identifier("value".to_string()))
            ]
        );
    }
}
