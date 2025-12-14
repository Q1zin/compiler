use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize, // byte index
    pub end: usize,   // byte index
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    True,
    False,
    And,
    Or,
    Gt,
    Lt,
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
    pub kind: String, // "error" | "warning" (we only emit errors for now)
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
    // 1-based line/column
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
                // Try known dotted keyword .XXX.
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

                // Unknown dotted word like .FASE.
                // Pattern: '.' IDENT '.'
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
                        if j < bytes.len() && input[j..].starts_with('.') {
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

                        // ".Y" style: extra dot before identifier
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

                // Lone dot or dot before something else
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

                // Bare keywords that must be dotted
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
        // Convert lexer error tokens into messages and skip them.
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
        self.parse_lv();
        self.consume_errors();

        // Extra tokens / extra closing parens
        while !self.at_eof() {
            let tok = self.current().clone();
            match tok.kind {
                TokenKind::RParen => {
                    self.emit_error_token(&tok, "Лишняя закрывающая скобка".to_string());
                    self.advance();
                }
                TokenKind::Eof => break,
                _ => {
                    self.emit_error_token(&tok, "Лишние символы в конце выражения".to_string());
                    self.advance();
                }
            }
            self.consume_errors();
        }
    }

    // ⟨ЛВ⟩ → ⟨ЛТ⟩ ( .OR. ⟨ЛТ⟩ )*
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

    // ⟨ЛТ⟩ → ⟨ЛО⟩ ( .AND. ⟨ЛО⟩ )*
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

    // ⟨ЛО⟩ → .TRUE. | .FALSE. | ⟨СВ⟩ | (⟨ЛВ⟩)
    fn parse_lo(&mut self) {
        self.consume_errors();
        let tok = self.current().clone();
        match tok.kind {
            TokenKind::True | TokenKind::False => {
                self.advance();
            }
            TokenKind::LParen => {
                self.advance();
                self.consume_errors();
                self.parse_lv();
                self.consume_errors();
                if matches!(self.current().kind, TokenKind::RParen) {
                    self.advance();
                } else {
                    // missing closing paren
                    self.emit_error_at(tok.span.start, "Отсутствует закрывающая скобка".to_string());
                }
            }
            TokenKind::Id(_) | TokenKind::Num(_) => {
                self.parse_sv();
            }
            TokenKind::RParen => {
                // likely extra ')'
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

    // ⟨СВ⟩ → ⟨Операнд⟩ ( .GT. ⟨Операнд⟩ | .LT. ⟨Операнд⟩ )
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

            // second operand
            match self.current().kind.clone() {
                TokenKind::Id(_) | TokenKind::Num(_) => {
                    self.parse_operand();
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
                    // do not consume AND/OR here; higher level will handle it
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
            // If it starts like a comparison but no .GT./.LT. follows
            self.emit_error_token(
                &op_tok,
                "Ожидалось сравнение с .GT. или .LT.".to_string(),
            );
        }
    }

    // ⟨Операнд⟩ → id | num
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
