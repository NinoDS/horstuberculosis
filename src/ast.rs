use std::fmt::Display;
use std::ops::Index;
use crate::tokens::Token;
use crate::TokenType;

#[derive(Debug, PartialEq)]
pub enum Statement {
	Block(Vec<Statement>),
	Class(Box<Class>),
	Expression(Expression),
	Function(Box<Function>),
	If(Box<If>),
	Print(Expression),
	Return(Option<Expression>),
	Let(Let),
	While(Box<While>),
	For(Box<For>),
	Break,
	Continue,
	Loop(Box<Statement>),
}

#[derive(Debug, PartialEq)]
pub struct Class {
	pub(crate) name: String,
	pub(crate) superclass: Option<String>,
	pub(crate) methods: Vec<Function>,
	pub(crate) properties: Vec<Statement>,
	pub(crate) index_setter: Option<Function>,
	pub(crate) index_getter: Option<Function>,
	pub(crate) init: Option<Function>,
}
#[derive(Debug, PartialEq)]
pub struct Function {
	pub(crate) name: String,
	pub(crate) params: Vec<String>,
	pub(crate) body: Statement,
	pub(crate) kind: FunctionKind,
}

#[derive(PartialEq, Debug)]
pub enum FunctionKind {
	Method,
	Initializer,
	IndexSetter,
	IndexGetter,
	Function,
	Getter,
	Setter,
}

impl Display for FunctionKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			FunctionKind::Method => write!(f, "method"),
			FunctionKind::Initializer => write!(f, "initializer"),
			FunctionKind::IndexSetter => write!(f, "index setter"),
			FunctionKind::IndexGetter => write!(f, "index getter"),
			FunctionKind::Function => write!(f, "function"),
			FunctionKind::Getter => write!(f, "getter"),
			FunctionKind::Setter => write!(f, "setter"),
		}
	}
}
#[derive(Debug, PartialEq)]
pub struct If {
	pub(crate) condition: Expression,
	pub(crate) then: Statement,
	pub(crate) else_if: Vec<ElseIf>,
	pub(crate) r#else: Option<Statement>,
}
#[derive(Debug, PartialEq)]
pub struct ElseIf {
	pub(crate) condition: Expression,
	pub(crate) then: Statement,
}
#[derive(Debug, PartialEq)]
pub struct Let {
	pub(crate) name: String,
	pub(crate) value: Option<Expression>,
}
#[derive(Debug, PartialEq)]
pub struct While {
	pub(crate) condition: Expression,
	pub(crate) body: Statement,
}
#[derive(Debug, PartialEq)]
pub struct For {
	pub(crate) init: Option<Statement>,
	pub(crate) condition: Option<Expression>,
	pub(crate) update: Option<Expression>,
	pub(crate) body: Statement,
}
#[derive(Debug, PartialEq)]
pub struct Property {
	name: String,
	value: Option<Expression>,
}
#[derive(Debug, PartialEq)]
pub enum Expression {
	AnonymousFunction(AnonymousFunction),
	Assign(Assign),
	Array(Vec<Expression>),
	Binary(Binary),
	Call(Call),
	Cast(Cast),
	IndexGet(IndexGet),
	IndexSet(IndexSet),
	Get(Get),
	Grouping(Box<Expression>),
	Literal(Literal),
	Logical(Logical),
	Set(Set),
	Super(Super),
	This,
	Unary(Unary),
	Variable(String),
}
#[derive(Debug, PartialEq)]
pub struct AnonymousFunction {
	pub(crate) params: Vec<String>,
	pub(crate) body: Box<Statement>,
}
#[derive(Debug, PartialEq)]
pub struct Assign {
	pub(crate) name: String,
	pub(crate) value: Box<Expression>,
	pub operator: Token,
}
#[derive(Debug, PartialEq)]
pub struct Binary {
	pub(crate) left: Box<Expression>,
	pub(crate) operator: TokenType,
	pub(crate) right: Box<Expression>,
}
#[derive(Debug, PartialEq)]
pub struct Call {
	pub(crate) callee: Box<Expression>,
	pub(crate) args: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct Cast {
	pub(crate) value: Box<Expression>,
	pub(crate) r#type: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct IndexGet {
	pub(crate) object: Box<Expression>,
	pub(crate) index: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct IndexSet {
	pub(crate) object: Box<Expression>,
	pub(crate) index: Box<Expression>,
	pub(crate) value: Box<Expression>,
	pub(crate) operator: Token,
}

#[derive(Debug, PartialEq)]
pub struct Get {
	pub(crate) object: Box<Expression>,
	pub(crate) name: String,
}

#[derive(Debug, PartialEq)]
pub struct Logical {
	pub(crate) left: Box<Expression>,
	pub(crate) operator: TokenType,
	pub(crate) right: Box<Expression>,
}
#[derive(Debug, PartialEq)]
pub struct Set {
	pub(crate) object: Box<Expression>,
	pub(crate) name: String,
	pub(crate) value: Box<Expression>,
	pub operator: Token,
}
#[derive(Debug, PartialEq)]
pub struct Super {
	pub(crate) method: Option<String>,
	pub keyword: Token,
}
#[derive(Debug, PartialEq)]
pub struct Unary {
	pub(crate) operator: TokenType,
	pub(crate) right: Box<Expression>,
}
#[derive(Debug, PartialEq)]
pub enum Literal {
	Boolean(bool),
	Number(f64),
	String(String),
	Nil,
	Type(Type),
	Function(AnonymousFunction),
}

#[derive(Debug, PartialEq)]
pub enum Type {
	String,
	Number,
	Boolean,
	Nil,
	Object,
	Fn,
	Array,
	Class,
	Type,
}
