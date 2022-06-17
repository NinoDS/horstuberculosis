use std::fmt::Display;
use std::ops::Range;
use crate::tokens::{Position, Token};
use crate::TokenType;

#[derive(Debug, Clone)]
pub(crate) struct ScanError {
	message: String,
	start: Position,
	end: Position,
}

impl Display for ScanError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", self.message)
	}
}

pub(crate) struct Scanner {
	source: String,
	pub(crate) tokens: Vec<Token>,
	start: usize,
	current: usize,
	line: usize,
	error: Option<ScanError>,
	column: usize,
}

impl Scanner {
	pub fn new<S: ToString>(source: S) -> Scanner {
		Scanner {
			source: source.to_string(),
			tokens: Vec::new(),
			start: 0,
			current: 0,
			line: 1,
			column: 1,
			error: None,
		}
	}

	pub fn scan_tokens(&mut self) -> Result<(), ScanError> {
		while !self.is_at_end() {
			if self.has_error() {
				return Err(self.error.clone().unwrap());
			}
			self.start = self.current;
			self.scan_token();
		}

		self.push_token(TokenType::Eof);

		if self.has_error() {
			return Err(self.error.clone().unwrap());
		}
		Ok(())
	}

	fn scan_token(&mut self) {
		let c = self.advance();
		match c {
			' ' | '\r' | '\t' => {

			}
			'\n' => {
			}
			'(' => self.push_token(TokenType::LeftParen),
			')' => self.push_token(TokenType::RightParen),
			'{' => self.push_token(TokenType::LeftBrace),
			'}' => self.push_token(TokenType::RightBrace),
			'[' => self.push_token(TokenType::LeftBracket),
			']' => self.push_token(TokenType::RightBracket),
			',' => self.push_token(TokenType::Comma),
			'.' => self.push_token(TokenType::Dot),
			';' => self.push_token(TokenType::Semicolon),

			'+' => self.match_token('=', TokenType::PlusEqual, TokenType::Plus),
			'-' => self.match_token('=', TokenType::MinusEqual, TokenType::Minus),
			'*' => self.match_token('=', TokenType::StarEqual, TokenType::Star),
			'!' => self.match_token('=', TokenType::BangEqual, TokenType::Bang),
			'=' => self.match_token('=', TokenType::EqualEqual, TokenType::Equal),
			'>' => self.match_token('=', TokenType::GreaterEqual, TokenType::Greater),
			'<' => self.match_token('=', TokenType::LessEqual, TokenType::Less),
			'^' => self.match_token('=', TokenType::CaretEqual, TokenType::Caret),
			'%' => self.match_token('=', TokenType::PercentEqual, TokenType::Percent),

			'&' => {
				// Only && is a valid token
				if self.match_char('&') {
					self.push_token(TokenType::AmpersandAmpersand);
				} else {
					self.error("Expected '&'");
				}
			},
			'|' => {
				// Only || is a valid token
				if self.match_char('|') {
					self.push_token(TokenType::PipePipe);
				} else {
					self.error("Expected '|'");
				}
			},

			'/' => {
				// Either comment, block comment, or division
				if self.match_char('/') {
					// Line comment
					self.skip_line_comment();
				} else if self.match_char('*') {
					// Block comment
					self.skip_block_comment();
				} else {
					self.push_token(TokenType::Slash);
				}
			}

			'"' => self.string(),
			'0'..='9' => self.number(),
			_ => {
				if Scanner::is_identifier_start(c) {
					self.identifier();
				} else {
					self.error(format!("Unexpected character: {}", c));
				}
			}
		}
	}

	fn skip_line_comment(&mut self) {
		while !self.is_at_end() && self.peek() != '\n' {
			self.advance();
		}
	}

	fn skip_block_comment(&mut self) {
		// Advance until "*/"
		while !self.is_at_end() {
			if self.match_char('*') && self.match_char('/') {
				break;
			} else {
				self.advance();
			}
		}

		// If we reached the end of the file, error
		if self.is_at_end() {
			self.error("Unterminated block comment");
		}
	}

	fn string(&mut self) {
		while !self.is_at_end() && self.peek() != '"' {
			if self.peek() == '\n' {
				self.line += 1;
			}
			self.advance();
		}

		// If we reached the end of the file, error
		if self.is_at_end() {
			self.error("Unterminated string");
		}

		// Advance past the closing "
		self.advance();

		// Trim the surrounding quotes
		let value = &self.source[self.start + 1..self.current - 1];
		self.push_token(TokenType::String(value.to_string()));
	}

	fn number(&mut self) {
		while self.is_digit(self.peek()) {
			self.advance();
		}

		// Look for a fractional part
		if self.peek() == '.' && self.is_digit(self.peek_next()) {
			// Consume the "."
			self.advance();

			while self.is_digit(self.peek()) {
				self.advance();
			}
		}

		let value = &self.source[self.start..self.current];
		let number = match value.parse::<f64>() {
			Ok(n) => n,
			Err(e) => {
				self.error(format!("{}", e));
				return;
			}
		};

		self.push_token(TokenType::Number(number));
	}

	fn identifier(&mut self) {
		while self.is_alphanumeric(self.peek()) {
			self.advance();
		}

		let value = &self.source[self.start..self.current];
		let token_type = match value {
			"as" => TokenType::As,
			"break" => TokenType::Break,
			"class" => TokenType::Class,
			"continue" => TokenType::Continue,
			"else" => TokenType::Else,
			"false" => TokenType::False,
			"fn" => TokenType::Fn,
			"for" => TokenType::For,
			"get" => TokenType::Get,
			"if" => TokenType::If,
			"index" => TokenType::Index,
			"init" => TokenType::Init,
			"let" => TokenType::Let,
			"loop" => TokenType::Loop,
			"nil" => TokenType::Nil,
			"print" => TokenType::Print,
			"return" => TokenType::Return,
			"set" => TokenType::Set,
			"super" => TokenType::Super,
			"this" => TokenType::This,
			"true" => TokenType::True,
			"typeof" => TokenType::Typeof,
			"while" => TokenType::While,
			"string" => TokenType::StringType,
			"number" => TokenType::NumberType,
			"boolean" => TokenType::BooleanType,
			"object" => TokenType::ObjectType,
			"array" => TokenType::ArrayType,
			"type" => TokenType::TypeType,
			_ => TokenType::Identifier(value.to_string()),
		};

		self.push_token(token_type);
	}

	fn is_identifier_start(c: char) -> bool {
		c.is_ascii_alphabetic() || c == '_'
	}

	fn match_token(
		&mut self,
		expected: char,
		if_equal: TokenType,
		if_not_equal: TokenType,
	) {
		if self.match_char(expected) {
			self.push_token(if_equal);
		} else {
			self.push_token(if_not_equal);
		}
	}

	fn match_char(&mut self, expected: char) -> bool {
		if self.is_at_end() || self.peek() != expected {
			return false;
		}

		self.advance();
		true
	}

	fn peek(&self) -> char {
		if self.is_at_end() {
			'\0'
		} else {
			self.source_at(self.current)
		}
	}

	fn peek_next(&self) -> char {
		if self.current + 1 >= self.source.len() {
			'\0'
		} else {
			self.source_at(self.current + 1)
		}
	}

	fn is_digit(&self, c: char) -> bool {
		c.is_ascii_digit()
	}

	fn is_alphanumeric(&self, c: char) -> bool {
		self.is_digit(c) || Scanner::is_identifier_start(c)
	}

	fn is_at_end(&self) -> bool {
		self.current >= self.source.len()
	}

	fn advance(&mut self) -> char {
		self.current += 1;
		self.column += 1;

		let c = self.source_at(self.current - 1);
		if c == '\n' {
			self.line += 1;
			self.column = 1;
		}
		c
	}

	fn push_token(&mut self, token_type: TokenType) {
		let mut start_line = 1;
		let mut start_column = 1;
		for (i, c) in self.source.chars().enumerate() {
			if i >= self.start {
				break;
			}
			if c == '\n' {
				start_line += 1;
				start_column = 1;
			} else {
				start_column += 1;
			}
		}

		let start = Position {
			line: start_line,
			column: start_column,
		};

		self.tokens.push(Token {
			token_type,
			start,
			end: Position {
				line: self.line,
				column: self.column,
			},
		});
	}

	fn error<S: ToString>(&mut self, message: S) {
		let end = Position {
			line: self.line,
			column: self.column,
		};

		let mut start_line = 1;
		let mut start_column = 1;
		for (i, c) in self.source.chars().enumerate() {
			if i >= self.start {
				break;
			}
			if c == '\n' {
				start_line += 1;
				start_column = 1;
			} else {
				start_column += 1;
			}
		}

		let start = Position {
			line: start_line,
			column: start_column,
		};

		self.error = Some(ScanError {
			start,
			end,
			message: message.to_string(),
		});
	}

	fn has_error(&self) -> bool {
		self.error.is_some()
	}

	fn source_at(&self, index: usize) -> char {
		if let Some(c) = self.source.chars().nth(index) {
			c
		} else {
			'\0'
		}
	}

}