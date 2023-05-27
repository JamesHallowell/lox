#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token<'input> {
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
    Literal(Literal<'input>),
    Keyword(Keyword),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Literal<'input> {
    Identifier(&'input str),
    String(&'input str),
    Number(f64),
}

impl<'a> From<&'a str> for Token<'a> {
    fn from(value: &'a str) -> Self {
        Self::Literal(Literal::String(value))
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
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
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl From<Keyword> for Token<'_> {
    fn from(keyword: Keyword) -> Self {
        Self::Keyword(keyword)
    }
}

fn char_equals(needle: char) -> impl FnOnce(&(usize, char)) -> bool {
    move |&(_, found)| found == needle
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut chars = input.char_indices().peekable();
    let mut tokens = vec![];

    while let Some((i, char)) = chars.next() {
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
            '!' => match chars.next_if(char_equals('=')) {
                Some(_) => Some(Token::BangEqual),
                None => Some(Token::Bang),
            },
            '=' => match chars.next_if(char_equals('=')) {
                Some(_) => Some(Token::EqualEqual),
                None => Some(Token::Equal),
            },
            '<' => match chars.next_if(char_equals('=')) {
                Some(_) => Some(Token::LessEqual),
                None => Some(Token::Less),
            },
            '>' => match chars.next_if(char_equals('=')) {
                Some(_) => Some(Token::GreaterEqual),
                None => Some(Token::Greater),
            },
            '/' => match chars.next_if(char_equals('/')) {
                Some(_) => {
                    while chars.next_if(|&(_, char)| char != '\n').is_some() {}
                    None
                }
                None => Some(Token::Slash),
            },
            '"' => {
                let start = i + char.len_utf8();
                loop {
                    match chars.next() {
                        Some((end, '"')) => {
                            let string = &input[start..end];
                            break Some(Token::Literal(Literal::String(string)));
                        }
                        Some(_) => {}
                        None => {
                            break None;
                        }
                    }
                }
            }
            char if char.is_numeric() => {
                let mut value = String::from(char);

                while let Some((_, char)) = chars.next_if(|(_, char)| char.is_numeric()) {
                    value.push(char);
                }

                if let Some((_, '.')) = chars.peek() {
                    let is_dot_followed_by_more_numbers = chars
                        .clone()
                        .nth(1)
                        .map(|(_, char)| char.is_numeric())
                        .unwrap_or(false);

                    if is_dot_followed_by_more_numbers {
                        value.push(chars.next().map(|(_, char)| char).unwrap());
                        while let Some((_, char)) = chars.next_if(|(_, char)| char.is_numeric()) {
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
                let start = i;
                let mut end = start + char.len_utf8();
                loop {
                    match chars.next_if(|(_, char)| char.is_alphanumeric()) {
                        Some((_, char)) => {
                            end += char.len_utf8();
                        }
                        _ => {
                            let string = &input[start..end];
                            break Some(match string {
                                "and" => Token::Keyword(Keyword::And),
                                "class" => Token::Keyword(Keyword::Class),
                                "else" => Token::Keyword(Keyword::Else),
                                "false" => Token::Keyword(Keyword::False),
                                "fun" => Token::Keyword(Keyword::Fun),
                                "for" => Token::Keyword(Keyword::For),
                                "if" => Token::Keyword(Keyword::If),
                                "nil" => Token::Keyword(Keyword::Nil),
                                "or" => Token::Keyword(Keyword::Or),
                                "return" => Token::Keyword(Keyword::Return),
                                "super" => Token::Keyword(Keyword::Super),
                                "this" => Token::Keyword(Keyword::This),
                                "true" => Token::Keyword(Keyword::True),
                                "var" => Token::Keyword(Keyword::Var),
                                "while" => Token::Keyword(Keyword::While),
                                _ => Token::Literal(Literal::Identifier(string)),
                            });
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
    fn can_lex_single_character_tokens() {
        assert_eq!(
            lex("(){},.-+;*"),
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
        assert_eq!(
            lex("! != = == < <= > >= / //"),
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
        assert_eq!(lex("// () {}\n+"), vec![Token::Plus]);
    }

    #[test]
    fn can_lex_string_literals() {
        assert_eq!(
            lex("\"Hello, world!\""),
            vec![Token::Literal(Literal::String("Hello, world!"))]
        );
    }

    #[test]
    fn can_lex_empty_string_literals() {
        assert_eq!(lex("\"\""), vec![Token::Literal(Literal::String(""))]);
    }

    #[test]
    fn can_lex_unterminated_string_literals() {
        let tokens = lex("\"Hello, wor");
        assert!(tokens.is_empty());
    }

    #[test]
    fn can_lex_multi_line_string_literals() {
        assert_eq!(
            lex("\"Hello, wor\nld!\""),
            vec![Token::Literal(Literal::String("Hello, wor\nld!"))]
        );
    }

    #[test]
    fn can_lex_number_literals() {
        assert_eq!(
            lex("1 1234 12.34 .1234 1234."),
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
    fn can_lex_identifiers_and_keywords() {
        assert_eq!(
            lex("hello and class while value"),
            vec![
                Token::Literal(Literal::Identifier("hello")),
                Token::Keyword(Keyword::And),
                Token::Keyword(Keyword::Class),
                Token::Keyword(Keyword::While),
                Token::Literal(Literal::Identifier("value"))
            ]
        );
    }

    #[test]
    fn can_lex_unary_expression_statement() {
        assert_eq!(
            lex("!true;"),
            vec![Token::Bang, Token::Keyword(Keyword::True), Token::Semicolon]
        );
    }
}
