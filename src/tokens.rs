use std::fmt::{Debug, Display};
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
	LeftParen,          // "("
	RightParen,         // ")"
	LeftBrace,          // "{"
	RightBrace,         // "}"
	LeftBracket,        // "["
	RightBracket,       // "]"
	Comma,              // ","
	Dot,                // "."
	Semicolon,          // ";"


	Slash,              // "/"
	SlashEqual,         // "/="
	Star,               // "*"
	StarEqual,          // "*="
	Minus,              // "-"
	MinusEqual,         // "-="
	Plus,               // "+"
	PlusEqual,          // "+="
	Bang,               // "!"
	BangEqual,          // "!="
	Equal,              // "="
	EqualEqual,         // "=="
	Greater,            // ">"
	GreaterEqual,       // ">="
	Less,               // "<"
	LessEqual,          // "<="
	AmpersandAmpersand, // "&&"
	PipePipe,           // "||"
	Caret,              // "^"
	CaretEqual,         // "^="
	Percent,            // "%"
	PercentEqual,       // "%="

	Identifier(String),
	String(String),
	Number(f64),

	// Keywords: as, break,class,continue,else,false,fn,for,get,if,index,init,let,loop,nil,print,return,set,super,this,true,typeof,while
	As,                 // "as"
	Break,              // "break"
	Class,              // "class"
	Continue,           // "continue"
	Else,               // "else"
	False,              // "false"
	Fn,                 // "fn"
	For,                // "for"
	Get,                // "get"
	If,                 // "if"
	Index,              // "index"
	Init,               // "init"
	Let,                // "let"
	Loop,               // "loop"
	Nil,                // "nil"
	Print,              // "print"
	Return,             // "return"
	Set,                // "set"
	Super,              // "super"
	This,               // "this"
	True,               // "true"
	Typeof,             // "typeof"
	While,              // "while"

	// Types
	StringType,         // "string"
	NumberType,         // "number"
	BooleanType,        // "boolean"
	ObjectType,         // "object"
	ArrayType,          // "array"
	TypeType,           // "type"
	// "fn" is a keyword, but it's also a type
	// "nil" is a keyword, but it's also its own type

	Eof,
}

impl Display for TokenType {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			TokenType::LeftParen => write!(f, "("),
			TokenType::RightParen => write!(f, ")"),
			TokenType::LeftBrace => write!(f, "{{"),
			TokenType::RightBrace => write!(f, "}}"),
			TokenType::LeftBracket => write!(f, "["),
			TokenType::RightBracket => write!(f, "]"),
			TokenType::Comma => write!(f, ","),
			TokenType::Dot => write!(f, "."),
			TokenType::Semicolon => write!(f, ";"),

			TokenType::Slash => write!(f, "/"),
			TokenType::SlashEqual => write!(f, "/="),
			TokenType::Star => write!(f, "*"),
			TokenType::StarEqual => write!(f, "*="),
			TokenType::Minus => write!(f, "-"),
			TokenType::MinusEqual => write!(f, "-="),
			TokenType::Plus => write!(f, "+"),
			TokenType::PlusEqual => write!(f, "+="),
			TokenType::Bang => write!(f, "!"),
			TokenType::BangEqual => write!(f, "!="),
			TokenType::Equal => write!(f, "="),
			TokenType::EqualEqual => write!(f, "=="),
			TokenType::Greater => write!(f, ">"),
			TokenType::GreaterEqual => write!(f, ">="),
			TokenType::Less => write!(f, "<"),
			TokenType::LessEqual => write!(f, "<="),
			TokenType::AmpersandAmpersand => write!(f, "&&"),
			TokenType::PipePipe => write!(f, "||"),
			TokenType::Caret => write!(f, "^"),
			TokenType::CaretEqual => write!(f, "^="),
			TokenType::Percent => write!(f, "%"),
			TokenType::PercentEqual => write!(f, "%="),

			TokenType::Identifier(ref s) => write!(f, "{}", s),
			TokenType::String(ref s) => write!(f, "{}", s),
			TokenType::Number(ref n) => write!(f, "{}", n),

			TokenType::As => write!(f, "as"),
			TokenType::Break => write!(f, "break"),
			TokenType::Class => write!(f, "class"),
			TokenType::Continue => write!(f, "continue"),
			TokenType::Else => write!(f, "else"),
			TokenType::False => write!(f, "false"),
			TokenType::Fn => write!(f, "fn"),
			TokenType::For => write!(f, "for"),
			TokenType::Get => write!(f, "get"),
			TokenType::If => write!(f, "if"),
			TokenType::Index => write!(f, "index"),
			TokenType::Init => write!(f, "init"),
			TokenType::Let => write!(f, "let"),
			TokenType::Loop => write!(f, "loop"),
			TokenType::Nil => write!(f, "nil"),
			TokenType::Print => write!(f, "print"),
			TokenType::Return => write!(f, "return"),
			TokenType::Set => write!(f, "set"),
			TokenType::Super => write!(f, "super"),
			TokenType::This => write!(f, "this"),
			TokenType::True => write!(f, "true"),
			TokenType::Typeof => write!(f, "typeof"),
			TokenType::While => write!(f, "while"),

			TokenType::StringType => write!(f, "string"),
			TokenType::NumberType => write!(f, "number"),
			TokenType::BooleanType => write!(f, "boolean"),
			TokenType::ObjectType => write!(f, "object"),
			TokenType::ArrayType => write!(f, "array"),
			TokenType::TypeType => write!(f, "type"),

			TokenType::Eof => write!(f, "EOF"),
		}
	}
}

#[derive(Clone, PartialEq, Debug)]
pub struct Token {
	pub token_type: TokenType,
	pub line: usize,
	pub column: usize,
}
