use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    True,
    False,
    And,
    Or,
    Gt,
    Lt,
    Newline,
    LParen,
    RParen,
    Id(String),
    Num(String),
    Error(LexErrorKind),
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexErrorKind {
    MissingDotsForKeyword { keyword: String, suggested: String },
    MissingTrailingDotForKeyword { keyword: String, suggested: String },
    UnknownDottedWord { word: String },
    ExtraDotBeforeIdentifier,
    UnexpectedChar { ch: char },
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ValidationMessage {
    pub kind: String,
    pub message: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Serialize)]
pub struct ValidationResult {
    pub ok: bool,
    pub messages: Vec<ValidationMessage>,
}

fn line_col(input: &str, byte_index: usize) -> (usize, usize) {
    let mut line = 1usize;
    let mut col = 1usize;

    for (idx, ch) in input.char_indices() {
        if idx >= byte_index {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    (line, col)
}

fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

fn lex(input: &str) -> Vec<Token> {
    let bytes = input.as_bytes();
    let mut i = 0usize;
    let mut tokens = Vec::new();

    let push = |tokens: &mut Vec<Token>, kind: TokenKind, start: usize, end: usize| {
        tokens.push(Token {
            kind,
            span: Span { start, end },
        });
    };

    while i < bytes.len() {
        let ch = input[i..].chars().next().unwrap();

        if ch == '\n' {
            let start = i;
            i += 1;
            push(&mut tokens, TokenKind::Newline, start, i);
            continue;
        }
        if ch == '\r' {
            let start = i;
            if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                i += 2;
            } else {
                i += 1;
            }
            push(&mut tokens, TokenKind::Newline, start, i);
            continue;
        }

        if ch.is_whitespace() {
            i += ch.len_utf8();
            continue;
        }

        let start = i;

        match ch {
            '(' => {
                i += 1;
                push(&mut tokens, TokenKind::LParen, start, i);
            }
            ')' => {
                i += 1;
                push(&mut tokens, TokenKind::RParen, start, i);
            }
            '.' => {
                let rest = &input[start..];
                let kw = [
                    (".TRUE.", TokenKind::True),
                    (".FALSE.", TokenKind::False),
                    (".AND.", TokenKind::And),
                    (".OR.", TokenKind::Or),
                    (".GT.", TokenKind::Gt),
                    (".LT.", TokenKind::Lt),
                ];
                let mut matched = None;
                for (lit, kind) in kw {
                    if rest.starts_with(lit) {
                        matched = Some((lit.len(), kind));
                        break;
                    }
                }

                if let Some((len, kind)) = matched {
                    i += len;
                    push(&mut tokens, kind, start, i);
                    continue;
                }

                let mut j = start + 1;
                if j < bytes.len() {
                    let next_ch = input[j..].chars().next().unwrap();
                    if is_ident_start(next_ch) {
                        j += next_ch.len_utf8();
                        while j < bytes.len() {
                            let c = input[j..].chars().next().unwrap();
                            if is_ident_continue(c) {
                                j += c.len_utf8();
                            } else {
                                break;
                            }
                        }

                        let word_upper = input[(start + 1)..j].to_string();
                        let suggested = match word_upper.as_str() {
                            "TRUE" => Some((".TRUE.", TokenKind::True)),
                            "FALSE" => Some((".FALSE.", TokenKind::False)),
                            "AND" => Some((".AND.", TokenKind::And)),
                            "OR" => Some((".OR.", TokenKind::Or)),
                            "GT" => Some((".GT.", TokenKind::Gt)),
                            "LT" => Some((".LT.", TokenKind::Lt)),
                            _ => None,
                        };

                        if j >= bytes.len() || !input[j..].starts_with('.') {
                            if let Some((suggested_lit, kind)) = suggested {
                                i = j;
                                push(
                                    &mut tokens,
                                    TokenKind::Error(LexErrorKind::MissingTrailingDotForKeyword {
                                        keyword: word_upper,
                                        suggested: suggested_lit.to_string(),
                                    }),
                                    start,
                                    i,
                                );
                                push(&mut tokens, kind, start, i);
                                continue;
                            }
                        }

                        if j < bytes.len() && input[j..].starts_with('.') {
                            let rest_from_trailing_dot = &input[j..];
                            let trailing_dot_starts_known = [
                                ".TRUE.",
                                ".FALSE.",
                                ".AND.",
                                ".OR.",
                                ".GT.",
                                ".LT.",
                            ]
                            .iter()
                            .any(|lit| rest_from_trailing_dot.starts_with(lit));

                            if trailing_dot_starts_known {
                                i += 1;
                                push(
                                    &mut tokens,
                                    TokenKind::Error(LexErrorKind::ExtraDotBeforeIdentifier),
                                    start,
                                    i,
                                );
                                continue;
                            }

                            let word = input[(start + 1)..j].to_string();
                            j += 1;
                            i = j;
                            push(
                                &mut tokens,
                                TokenKind::Error(LexErrorKind::UnknownDottedWord { word }),
                                start,
                                i,
                            );
                            continue;
                        }

                        i += 1;
                        push(
                            &mut tokens,
                            TokenKind::Error(LexErrorKind::ExtraDotBeforeIdentifier),
                            start,
                            i,
                        );
                        continue;
                    }
                }

                i += 1;
                push(
                    &mut tokens,
                    TokenKind::Error(LexErrorKind::UnexpectedChar { ch: '.' }),
                    start,
                    i,
                );
            }
            c if is_ident_start(c) => {
                let mut j = start + c.len_utf8();
                while j < bytes.len() {
                    let cc = input[j..].chars().next().unwrap();
                    if is_ident_continue(cc) {
                        j += cc.len_utf8();
                    } else {
                        break;
                    }
                }
                let word = input[start..j].to_string();

                let upper = word.as_str();
                let dotted = match upper {
                    "TRUE" => Some(".TRUE."),
                    "FALSE" => Some(".FALSE."),
                    "AND" => Some(".AND."),
                    "OR" => Some(".OR."),
                    "GT" => Some(".GT."),
                    "LT" => Some(".LT."),
                    _ => None,
                };

                if let Some(suggested) = dotted {
                    i = j;
                    push(
                        &mut tokens,
                        TokenKind::Error(LexErrorKind::MissingDotsForKeyword {
                            keyword: word,
                            suggested: suggested.to_string(),
                        }),
                        start,
                        i,
                    );
                } else {
                    i = j;
                    push(&mut tokens, TokenKind::Id(word), start, i);
                }
            }
            c if c.is_ascii_digit() => {
                let mut j = start + 1;
                while j < bytes.len() {
                    let cc = input[j..].chars().next().unwrap();
                    if cc.is_ascii_digit() {
                        j += 1;
                    } else {
                        break;
                    }
                }
                let num = input[start..j].to_string();
                i = j;
                push(&mut tokens, TokenKind::Num(num), start, i);
            }
            other => {
                i += other.len_utf8();
                push(
                    &mut tokens,
                    TokenKind::Error(LexErrorKind::UnexpectedChar { ch: other }),
                    start,
                    i,
                );
            }
        }
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        span: Span {
            start: input.len(),
            end: input.len(),
        },
    });

    tokens
}

struct Parser<'a> {
    input: &'a str,
    tokens: Vec<Token>,
    pos: usize,
    messages: Vec<ValidationMessage>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str, tokens: Vec<Token>) -> Self {
        Self {
            input,
            tokens,
            pos: 0,
            messages: Vec::new(),
        }
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or_else(|| self.tokens.last().unwrap())
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len().saturating_sub(1) {
            self.pos += 1;
        }
    }

    fn at_eof(&self) -> bool {
        matches!(self.current().kind, TokenKind::Eof)
    }

    fn emit_error_at(&mut self, span_start: usize, message: String) {
        let (line, column) = line_col(self.input, span_start);
        self.messages.push(ValidationMessage {
            kind: "error".to_string(),
            message: format!("Ошибка выполнения: {}", message),
            line,
            column,
        });
    }

    fn emit_error_token(&mut self, tok: &Token, message: String) {
        self.emit_error_at(tok.span.start, message);
    }

    fn consume_errors(&mut self) {
        loop {
            let tok = self.current().clone();
            match &tok.kind {
                TokenKind::Error(kind) => {
                    match kind {
                        LexErrorKind::MissingDotsForKeyword { keyword, suggested } => {
                            self.emit_error_token(
                                &tok,
                                format!("Необходимо писать {}, а не {}", suggested, keyword),
                            );
                        }
                        LexErrorKind::MissingTrailingDotForKeyword { keyword, suggested } => {
                            let prev_non_error = self.tokens[..self.pos]
                                .iter()
                                .rev()
                                .find(|t| !matches!(t.kind, TokenKind::Error(_)))
                                .map(|t| &t.kind);

                            let found = format!(".{}", keyword);
                            let msg = match prev_non_error {
                                Some(TokenKind::And) => format!(
                                    "После .AND. ожидался логический операнд; найдено {} без точки в конце (нужно {})",
                                    found, suggested
                                ),
                                Some(TokenKind::Or) => format!(
                                    "После .OR. ожидался логический операнд; найдено {} без точки в конце (нужно {})",
                                    found, suggested
                                ),
                                Some(TokenKind::Gt) => format!(
                                    "После .GT. ожидался второй операнд; найдено {} без точки в конце (нужно {})",
                                    found, suggested
                                ),
                                Some(TokenKind::Lt) => format!(
                                    "После .LT. ожидался второй операнд; найдено {} без точки в конце (нужно {})",
                                    found, suggested
                                ),
                                _ => format!(
                                    "Необходимо писать {} (не хватает точки в конце)",
                                    suggested
                                ),
                            };

                            self.emit_error_token(&tok, msg);
                        }
                        LexErrorKind::UnknownDottedWord { word } => {
                            self.emit_error_token(
                                &tok,
                                format!("Неизвестный операнд .{}.", word),
                            );
                        }
                        LexErrorKind::ExtraDotBeforeIdentifier => {
                            self.emit_error_token(&tok, "Лишняя точка перед переменной".to_string());
                        }
                        LexErrorKind::UnexpectedChar { ch } => {
                            self.emit_error_token(&tok, format!("Недопустимый символ '{}'", ch));
                        }
                    }
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn parse(&mut self) {
        self.consume_errors();

        while matches!(self.current().kind, TokenKind::Newline) {
            self.advance();
            self.consume_errors();
        }

        while !self.at_eof() {
            self.parse_lv();
            self.consume_errors();

            if matches!(self.current().kind, TokenKind::Newline) {
                while matches!(self.current().kind, TokenKind::Newline) {
                    self.advance();
                    self.consume_errors();
                }
                continue;
            }

            if self.at_eof() {
                break;
            }

            while !self.at_eof() && !matches!(self.current().kind, TokenKind::Newline) {
                let tok = self.current().clone();
                match tok.kind {
                    TokenKind::RParen => {
                        self.emit_error_token(&tok, "Лишняя закрывающая скобка".to_string());
                        self.advance();
                    }
                    _ => {
                        self.emit_error_token(&tok, "Лишние символы в конце выражения".to_string());
                        self.advance();
                    }
                }
                self.consume_errors();
            }

            while matches!(self.current().kind, TokenKind::Newline) {
                self.advance();
                self.consume_errors();
            }
        }
    }

    fn parse_lv(&mut self) {
        self.parse_lt();
        loop {
            self.consume_errors();
            if matches!(self.current().kind, TokenKind::Or) {
                self.advance();
                self.consume_errors();
                self.parse_lt();
                continue;
            }
            break;
        }
    }

    fn parse_lt(&mut self) {
        self.parse_lo();
        loop {
            self.consume_errors();
            if matches!(self.current().kind, TokenKind::And) {
                self.advance();
                self.consume_errors();
                self.parse_lo();
                continue;
            }
            break;
        }
    }

    fn parse_lo(&mut self) {
        self.consume_errors();
        let tok = self.current().clone();
        match tok.kind {
            TokenKind::True | TokenKind::False => {
                self.advance();
            }
            TokenKind::Newline => {
                self.emit_error_token(&tok, "Ожидался логический операнд".to_string());
            }
            TokenKind::LParen => {
                self.advance();
                self.consume_errors();
                self.parse_lv();
                self.consume_errors();
                if matches!(self.current().kind, TokenKind::RParen) {
                    self.advance();
                } else {
                    self.emit_error_at(tok.span.start, "Отсутствует закрывающая скобка".to_string());
                }
            }
            TokenKind::Id(_) | TokenKind::Num(_) => {
                self.parse_sv();
            }
            TokenKind::RParen => {
                self.emit_error_token(&tok, "Лишняя закрывающая скобка".to_string());
                self.advance();
            }
            TokenKind::Eof => {
                self.emit_error_token(&tok, "Ожидалось логическое выражение".to_string());
            }
            _ => {
                self.emit_error_token(&tok, "Ожидался логический операнд".to_string());
                self.advance();
            }
        }
    }

    fn parse_sv(&mut self) {
        self.consume_errors();
        self.parse_operand();
        self.consume_errors();

        let op_tok = self.current().clone();
        let op_str = match op_tok.kind {
            TokenKind::Gt => Some(".GT."),
            TokenKind::Lt => Some(".LT."),
            _ => None,
        };

        if let Some(_op) = op_str {
            self.advance();
            self.consume_errors();

            match self.current().kind.clone() {
                TokenKind::Id(_) | TokenKind::Num(_) => {
                    self.parse_operand();
                }
                TokenKind::Newline => {
                    let here = self.current().clone();
                    self.emit_error_token(&here, "Ожидался второй операнд сравнения".to_string());
                }
                TokenKind::And | TokenKind::Or => {
                    let next_op = match self.current().kind {
                        TokenKind::And => ".AND.",
                        TokenKind::Or => ".OR.",
                        _ => "",
                    };
                    let here = self.current().clone();
                    self.emit_error_token(
                        &here,
                        format!("Между {} и {} нет второго операнда", match op_tok.kind { TokenKind::Gt => ".GT.", TokenKind::Lt => ".LT.", _ => "" }, next_op),
                    );
                }
                TokenKind::RParen | TokenKind::Eof => {
                    let here = self.current().clone();
                    self.emit_error_token(
                        &here,
                        format!("После {} отсутствует второй операнд", match op_tok.kind { TokenKind::Gt => ".GT.", TokenKind::Lt => ".LT.", _ => "" }),
                    );
                }
                _ => {
                    let here = self.current().clone();
                    self.emit_error_token(&here, "Ожидался второй операнд сравнения".to_string());
                    self.advance();
                }
            }
        } else {
            self.emit_error_token(
                &op_tok,
                "Ожидалось сравнение с .GT. или .LT.".to_string(),
            );
        }
    }

    fn parse_operand(&mut self) {
        self.consume_errors();
        let tok = self.current().clone();
        match tok.kind {
            TokenKind::Id(_) | TokenKind::Num(_) => self.advance(),
            _ => {
                self.emit_error_token(&tok, "Ожидался операнд (id или num)".to_string());
                self.advance();
            }
        }
    }
}

pub fn validate_expression(input: &str) -> ValidationResult {
    let tokens = lex(input);
    let mut parser = Parser::new(input, tokens);
    parser.parse();

    ValidationResult {
        ok: parser.messages.is_empty(),
        messages: parser.messages,
    }
}

#[cfg(test)]
mod tests {
    use super::validate_expression;

    #[test]
    fn recovers_after_missing_operand_and_extra_dot() {
        let res = validate_expression("X.GT..AND..Y.LT.5");
        assert_eq!(res.messages.len(), 2, "messages: {:#?}", res.messages);
        let m0 = &res.messages[0].message;
        let m1 = &res.messages[1].message;

        assert!(
            m0.contains("Между .GT. и .AND. нет второго операнда")
                || m1.contains("Между .GT. и .AND. нет второго операнда"),
            "expected missing-operand message, got: {m0:?} / {m1:?}"
        );
        assert!(
            m0.contains("Лишняя точка перед переменной") || m1.contains("Лишняя точка перед переменной"),
            "expected extra-dot message, got: {m0:?} / {m1:?}"
        );
    }

    #[test]
    fn missing_trailing_dot_after_and_is_single_error() {
        let res = validate_expression(".TRUE..AND..FALSE");
        assert_eq!(res.messages.len(), 1, "messages: {:#?}", res.messages);
        assert!(
            res.messages[0].message.contains("После .AND.")
                && res.messages[0].message.contains(".FALSE")
                && res.messages[0].message.contains("нужно .FALSE."),
            "unexpected message: {:?}",
            res.messages[0].message
        );
    }

    #[test]
    fn supports_multiple_lines_of_expressions() {
        let res = validate_expression(".TRUE.\n.FALSE.");
        assert_eq!(res.messages.len(), 0, "messages: {:#?}", res.messages);
    }

    #[test]
    fn missing_rhs_before_newline_does_not_cascade_into_next_line() {
        let input = "X.GT.\nX.GT..TRUE.";
        let res = validate_expression(input);
        assert_eq!(res.messages.len(), 2, "messages: {:#?}", res.messages);
        assert_eq!((res.messages[0].line, res.messages[0].column), (1, 6));
        assert_eq!((res.messages[1].line, res.messages[1].column), (2, 6));
        assert!(
            res.messages[0].message.contains("Ожидался второй операнд сравнения"),
            "unexpected message[0]: {:?}",
            res.messages[0].message
        );
        assert!(
            res.messages[1].message.contains("Ожидался второй операнд сравнения"),
            "unexpected message[1]: {:?}",
            res.messages[1].message
        );
    }

    #[test]
    fn missing_logical_operand_before_newline_does_not_cascade_into_next_line() {
        let input = ".TRUE..AND.\n.TRUE..OR.\n";
        let res = validate_expression(input);
        assert_eq!(res.messages.len(), 2, "messages: {:#?}", res.messages);
        assert_eq!((res.messages[0].line, res.messages[0].column), (1, 12));
        assert_eq!((res.messages[1].line, res.messages[1].column), (2, 11));
        assert!(
            res.messages[0].message.contains("Ожидался логический операнд"),
            "unexpected message[0]: {:?}",
            res.messages[0].message
        );
        assert!(
            res.messages[1].message.contains("Ожидался логический операнд"),
            "unexpected message[1]: {:?}",
            res.messages[1].message
        );
    }

    #[test]
    fn valid_chains_should_have_no_errors() {
        let cases = [
            ".TRUE.",
            ".FALSE.",
            "X.GT.0",
            "X.LT.5",
            "A1.GT.123",
            "ABC123.LT.999",
            ".TRUE..AND..FALSE.",
            ".TRUE..OR..FALSE.",
            "(X.GT.0)",
            "(X.GT.0).AND.(Y.LT.5)",
            "(X.GT.0.OR.Y.LT.5)",
            "(X.GT.0).OR.(Y.LT.5).AND.(Z.GT.10)",
            "((X.GT.0).AND.(Y.LT.5))",
            "((X.GT.0).AND.(Y.LT.5)).OR.(.TRUE.)",
            ".TRUE.\n.FALSE.\n(X.GT.0).AND.(Y.LT.5)",
            "\n\n.TRUE.\n\n.FALSE.\n",
        ];

        for input in cases {
            let res = validate_expression(input);
            assert!(
                res.messages.is_empty(),
                "expected ok for input {input:?}, messages: {:#?}",
                res.messages
            );
        }
    }

    #[test]
    fn one_error_chains_should_have_exactly_one_error() {
        let cases: [&str; 20] = [
            "X",
            "X.GT.",
            "X.GT..TRUE.",
            "Y.LT.",
            "Y.LT..TRUE.",
            ".TRUE",
            ".FALSE",
            ".TRUE..AND..FALSE",
            ".TRUE..OR..FALSE",
            ".TRUE..AND.",
            ".TRUE..OR.",
            "X.GT.0.OR.Y",
            "X.GT.0)",
            "X.GT.0.AND.",
            "X.GT.0.AND.X",
            "(X.GT.0",
            ".TRUE.\n.FALSE",
            ".TRUE.\nX",
            "\nX",
            "X.GT.0\nY",
        ];

        let mut failures: Vec<(&str, Vec<_>)> = Vec::new();

        for input in cases {
            let res = validate_expression(input);
            if res.messages.len() != 1 {
                failures.push((input, res.messages));
            }
        }

        assert!(
            failures.is_empty(),
            "expected exactly 1 error for every case, failures: {:#?}",
            failures
        );
    }

    #[test]
    fn two_error_inputs_should_have_exactly_two_errors() {
        let cases: [&str; 15] = [
            "X.GT..AND..Y.LT.5",
            "X.GT..AND..Y.GT.5",
            "X.LT..AND..Y.LT.5",
            "X.LT..AND..Y.GT.5",
            "A1.GT..AND..B2.LT.3",
            "A1.LT..AND..B2.GT.3",
            "ABC.GT..AND..DEF.LT.10",
            "ABC.LT..AND..DEF.GT.10",
            "X.GT..OR..Y.LT.5",
            "X.GT..OR..Y.GT.5",
            "X.LT..OR..Y.LT.5",
            "X.LT..OR..Y.GT.5",
            "A1.GT..OR..B2.LT.3",
            "A1.LT..OR..B2.GT.3",
            "ABC.GT..OR..DEF.LT.10",
        ];

        let mut failures: Vec<(&str, Vec<_>)> = Vec::new();

        for input in cases {
            let res = validate_expression(input);
            if res.messages.len() != 2 {
                failures.push((input, res.messages));
            }
        }

        assert!(
            failures.is_empty(),
            "expected exactly 2 errors for every case, failures: {:#?}",
            failures
        );
    }

    #[test]
    fn three_error_inputs_should_have_exactly_three_errors() {
        let cases: [&str; 10] = [
            "X.GT..AND..Y.LT.5)",
            "X.GT..OR..Y.LT.5)",
            "X.LT..AND..Y.GT.5)",
            "X.LT..OR..Y.GT.5)",
            "A1.GT..AND..B2.LT.3)",
            "A1.GT..OR..B2.LT.3)",
            "A1.LT..AND..B2.GT.3)",
            "A1.LT..OR..B2.GT.3)",
            "ABC.GT..AND..DEF.LT.10)",
            "ABC.LT..OR..DEF.GT.10)",
        ];

        let mut failures: Vec<(&str, Vec<_>)> = Vec::new();

        for input in cases {
            let res = validate_expression(input);
            if res.messages.len() != 3 {
                failures.push((input, res.messages));
            }
        }

        assert!(
            failures.is_empty(),
            "expected exactly 3 errors for every case, failures: {:#?}",
            failures
        );
    }
}
