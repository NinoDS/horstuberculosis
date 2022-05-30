pub enum TokenType {
    LeftParen,          // "("
    RightParen,         // ")"
    LeftBrace,          // "{"
    RightBrace,         // "}"
    LeftBracket,        // "["
    RightBracket,       // "]"
    Comma,              // ","
    Minus,              // "-"
    Plus,               // "+"
    Semicolon,          // ";"
    Slash,              // "/"
    Star,               // "*"

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

    Identifier(String),
    String(String),
    Number(f64),

    // Keywords: break,class,continue,else,false,fn,for,get,if,index,init,let,loop,null,print,return,set,super,this,true,typeof,while
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
    NilType = TokenType::Nil,
    FunctionType = TokenType::Fn,
    ObjectType,         // "object"
    ArrayType,          // "array"
    TypeType,           // "type"

    Eof,

}

