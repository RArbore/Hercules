use std::io::{Read, Result};
use std::fs::File;

pub mod tokens;
use lexer::tokens::*;

pub struct Token { pub tok : TokenType, pub line : usize, pub col : usize,
                   start : usize, len : usize, }

pub struct Lexer {
    contents : String, curToken : Option<Token>,
    lines : Vec<(usize, usize)>, // pairs of start and length
    index : usize, length : usize,
    lineno : usize, column : usize, line_start : usize,
}

pub fn is_eof(t : &Token) -> bool {
    match t.tok {
        TokenType::EOF => true,
        _ => false
    }
}

pub fn print_token(t : Token, l : &Lexer) {
    match t.tok {
        TokenType::As           => println!("as"),
        TokenType::By           => println!("by"),
        TokenType::Const        => println!("const"),
        TokenType::Else         => println!("else"),
        TokenType::False        => println!("false"),
        TokenType::Fn           => println!("fn"),
        TokenType::For          => println!("for"),
        TokenType::If           => println!("if"),
        TokenType::Inout        => println!("inout"),
        TokenType::Let          => println!("let"),
        TokenType::Match        => println!("match"),
        TokenType::Module       => println!("module"),
        TokenType::Pub          => println!("pub"),
        TokenType::Return       => println!("return"),
        TokenType::Size         => println!("size"),
        TokenType::Struct       => println!("struct"),
        TokenType::Then         => println!("then"),
        TokenType::To           => println!("to"),
        TokenType::True         => println!("true"),
        TokenType::Type         => println!("type"),
        TokenType::Union        => println!("union"),
        TokenType::Use          => println!("use"),
        TokenType::While        => println!("while"),

        TokenType::Bool         => println!("bool"),
        TokenType::I8           => println!("i8"),
        TokenType::I16          => println!("i16"),
        TokenType::I32          => println!("i32"),
        TokenType::I64          => println!("i64"),
        TokenType::U8           => println!("u8"),
        TokenType::U16          => println!("u16"),
        TokenType::U32          => println!("u32"),
        TokenType::U64          => println!("u64"),
        TokenType::F32          => println!("f32"),
        TokenType::F64          => println!("f64"),
        TokenType::Void         => println!("void"),
    
        TokenType::Add          => println!("+"),
        TokenType::And          => println!("&"),
        TokenType::Ands         => println!("&&"),
        TokenType::Arrow        => println!("=>"),
        TokenType::Bang         => println!("!"),
        TokenType::Bar          => println!("|"),
        TokenType::Bars         => println!("||"),
        TokenType::Colon        => println!(":"),
        TokenType::Colons       => println!("::"),
        TokenType::Comma        => println!(","),
        TokenType::Div          => println!("/"),
        TokenType::Dot          => println!("."),
        TokenType::Equal        => println!("="),
        TokenType::EqualAnd     => println!("&="),
        TokenType::EqualAnds    => println!("&&="),
        TokenType::EqualDiv     => println!("/="),
        TokenType::EqualMinus   => println!("-="),
        TokenType::EqualMod     => println!("%="),
        TokenType::EqualOr      => println!("|="),
        TokenType::EqualOrs     => println!("||="),
        TokenType::EqualPlus    => println!("+="),
        TokenType::EqualShiftL  => println!("<<="),
        TokenType::EqualShiftR  => println!(">>="),
        TokenType::EqualTimes   => println!("*="),
        TokenType::EqualXor     => println!("^="),
        TokenType::Equals       => println!("=="),
        TokenType::EqualsNot    => println!("!="),
        TokenType::Greater      => println!(">"),
        TokenType::GreaterEq    => println!(">="),
        TokenType::Less         => println!("<"),
        TokenType::LessEq       => println!("<="),
        TokenType::Minus        => println!("-"),
        TokenType::Mod          => println!("%"),
        TokenType::Semicolon    => println!(";"),
        TokenType::ShiftL       => println!("<<"),
        TokenType::ShiftR       => println!(">>"),
        TokenType::Star         => println!("*"),
        TokenType::Tilde        => println!("~"),
        TokenType::Underscore   => println!("_"),
        TokenType::Xor          => println!("^"),

        TokenType::CurlyOpen    => println!("{{"),
        TokenType::CurlyClose   => println!("}}"),
        TokenType::ParenOpen    => println!("("),
        TokenType::ParenClose   => println!(")"),
        TokenType::BrackOpen    => println!("["),
        TokenType::BrackClose   => println!("]"),

        TokenType::Ident | TokenType::IntDec | TokenType::IntHex | TokenType::IntBin
        | TokenType::IntOct | TokenType::Float
        | TokenType::DotNumber | TokenType::FuncAttr | TokenType::Unknown
            => println!("{}", l.get_lexeme(&t)),
        
        TokenType::EOF          => println!("EOF"),
        TokenType::Error(e)     =>
            match e {
                LexError::AttrEOF => println!("Lexer Error: Function attribute not closed before EOF"),
                LexError::AttrEOL => println!("Lexer Error: Function attribute not closed before EOL"),
                LexError::CommentEOF => println!("Lexer Error: Block comment not closed before EOF"),
                LexError::HexEmpty => println!("Lexer Error: Incomplete hexadecimal literal"),
                LexError::HexError => println!("Lexer Error: Invalid hexadecimal literal"),
                LexError::BinEmpty => println!("Lexer Error: Incomplete binary literal"),
                LexError::BinError => println!("Lexer Error: Invalid binary literal"),
                LexError::OctEmpty => println!("Lexer Error: Incomplete octal literal"),
                LexError::OctError => println!("Lexer Error: Invalid octal literal"),
                LexError::FloatExpEmpty => println!("Lexer Error: Empty float literal exponent"),
                LexError::InternalError => println!("PANIC: Internal Lexer Error"),
            },
    }
}

// The terminals in the grammar are ('...' indicates literal strings, /.../ indicates regex)
// '!', '!=', /#\[[^\]]*]/, '%', '%=', '&', '&&', '&&=', '&=', '(', ')', '*', '*=', '+', '+=', ',',
// '-', '-=', '.', /\.[0-9]+/, '/', '/=', '//', '/*', ':', '::', ';', '<', '<<', '<<=', '<=', '=',
// '==', '=>', '>', '>=', '>>', '>>=', '[', ']', '{', '}', '^', '^=', '_', '~', '|', '|=', '||',
// '||=', /[0-9]+/, /0x[0-9a-fA-F]+/, /0b[0-1]+/, /0o[0-7]+/, /[0-9]+\.[0-9]*(e[0-9]+)?/,
// /[a-zA-Z][a-zA-Z0-9_\-]*/, 'as', 'bool', 'by', 'const', 'else', 'f32', 'f64', 'false', 'fn',
// 'for', 'i16', 'i32', 'i64', 'i8', 'if', 'inout', let', 'match', 'module', 'pub', 'return',
// 'size', 'struct', 'then', 'to', 'true', 'type', 'u16', 'u32', 'u64', 'u8', 'union', 'use',
// 'void', 'while'
//
// For lexing, we use a state machine, as follows
// 0 : the initial state, also discards whitepsace
// 1 : for strings beginning !
// 2 : for strings beginning #
// 3 : for strings beginning %
// 4 : for strings beginning &
// 5 : for strings beginning *
// 6 : for strings beginning +
// 7 : for strings beginning -
// 8 : for strings beginning .
// 9 : for strings beginning /
// 10: for strings beginning :
// 11: for strings beginning <
// 12: for strings beginning =
// 13: for strings beginning >
// 14: for strings beginning ^
// 15: for strings beginning |
// 16: for strings beginning 0
// 17: for strings beginning [1-9][0-9]*|0[0-9]+ (decimal int lit or beginning of float lit)
// 18: for strings beginning a
// 19: for strings beginning b
// 20: for strings beginning c
// 21: for strings beginning e
// 22: for strings beginning f
// 23: for strings beginning i
// 24: for strings beginning l
// 25: for strings beginning m
// 26: for strings beginning p
// 27: for strings beginning r
// 28: for strings beginning s
// 29: for strings beginning t
// 30: for strings beginning u
// 31: for strings beginning v
// 32: for strings beginning w
// 33: for identifiers which do not match any keyword (for however much has been consumed)
// 34: for function attributes
// 35: for strings beginning &&
// 36: for strings beginning .[0-9]+ (dot number strings)
// 37: for line comments (after capturing //)
// 38: for block comments (after capturing /*)
// 39: for strings beginning <<
// 40: for strings beginning >>
// 41: for strings beginning ||
// 42: for strings beginning 0x (start of hex number)
// 43: for strings beginning 0b (start of bin number)
// 44: for strings beginning 0o (start of oct number)
// 45: for strings beginning [0-9]+.[0-9]* (start of float literal)
// 46: for strings beginning as
// 47: for strings beginning bo
// 48: for strings beginning by
// 49: for strings beginnign co
// 50: for strings beginning el
// 51: for strings beginning f3
// 52: for strings beginning f6
// 53: for strings beginning fa
// 54: for strings beginning fn
// 55: for strings beginning fo
// 56: for strings beginning i1
// 57: for strings beginning i3
// 58: for strings beginning i6
// 59: for strings beginning i8
// 60: for strings beginning if
// 61: for strings beginning in
// 62: for strings beginning le
// 63: for strings beginning ma
// 64: for strings beginning mo
// 65: for strings beginning pa
// 66: for strings beginning pu
// 67: for strings beginning re
// 68: for strings beginning si
// 69: for strings beginning st
// 70: for strings beginning th
// 71: for strings beginning to
// 72: for strings beginning tr
// 73: for strings beginning ty
// 74: for strings beginning u1
// 75: for strings beginning u3
// 76: for strings beginning u6
// 77: for strings beginning u8
// 78: for strings beginning un
// 79: for strings beginning us
// 80: for strings beginning vo
// 81: for strings beginning wh
// 82: for block comments, having just captured a *
// 83: for hex literals
// 84: for bin literals
// 85: for oct literals
// 86: for strings beginning [0-9]+.[0-9]*e (float literal with e)
// 87: for strings beginning boo
// 88: for strings beginning con
// 89: for strings beginning els
// 90: for strings beginning f32
// 91: for strings beginning f64
// 92: for strings beginning fal
// 93: for strings beginning for
// 94: for strings beginning i16
// 95: for strings beginning i32
// 96: for strings beginning i64
// 97: for strings beginning ino
// 98: for strings beginning let
// 99: for strings beginning mat
//100: for strings beginning mod
//101: for strings beginning pac
//102: for strings beginning pub
//103: for strings beginning ret
//104: for strings beginning siz
//105: for strings beginning str
//106: for strings beginning the
//107: for strings beginning tru
//108: for strings beginning typ
//109: for strings beginning u16
//110: for strings beginning u32
//111: for strings beginning u64
//112: for strings beginning uni
//113: for strings beginning use
//114: for strings beginning voi
//115: for strings beginning whi
//116: for floating point literal with exponent
//117: for strings beginning bool
//118: for strings beginning cons
//119: for strings beginning else
//120: for strings beginning fals
//121: for strings beginning inou
//122: for strings beginning matc
//123: for strings beginning modu
//124: for strings beginning pack
//125: for strings beginning retu
//126: for strings beginning size
//127: for strings beginning stru
//128: for strings beginning then
//129: for strings beginning true
//130: for strings beginning type
//131: for strings beginning unio
//132: for strings beginning void
//133: for strings beginning whil
//134: for strings beginning const
//135: for strings beginning false
//136: for strings beginning inout
//137: for strings beginning match
//138: for strings beginning modul
//139: for strings beginning packa
//140: for strings beginning retur
//141: for strings beginning struc
//142: for strings beginning union
//143: for strings beginning while
//144: for strings beginning module
//145: for strings beginning packag
//146: for strings beginning return
//147: for strings beginning struct
//148: for strings beginning package
impl Lexer {
    pub fn new(mut file : File) -> Result<Lexer> {
        let mut contents = String::new();
        match file.read_to_string(&mut contents) {
            Ok(l) => Ok(Lexer { contents : contents, curToken : None, lines : Vec::new(),
                                index : 0, length : l, lineno : 0, column : 0, line_start : 0 }),
            Err(e) => Err(e)
        }
    }

    pub fn get_lexeme(&self, tok : &Token) -> &str {
        let start = tok.start;
        let end = start + tok.len;
        &self.contents[start..end]
    }

    pub fn unlex(&mut self, tok : Token) {
        match self.curToken {
            Some(_) => { eprintln!("Internal Error: Multiple tokens unlex'd");
                         std::process::exit(1); },
            None => { self.curToken = Some(tok); },
        }
    }

    pub fn next_token(&mut self) -> Token {
        let mut curToken : Option<Token> = None;
        std::mem::swap(&mut curToken, &mut self.curToken);
        match curToken {
            Some(t) => { return t; },
            None => {},
        }
        let mut state = 0;
        let mut start = self.index;
        let mut index = self.index;
        let token : TokenType;
        let mut c_depth : u64 = 0;
        loop {
            let eof = index >= self.length;
            let c = if eof { '\0' } else { self.contents.as_bytes()[index] as char };
            match state {
                0 => if eof { token = TokenType::EOF; break }
                     else {
                         match c {
                             ' ' | '\t' => { index += 1; start += 1 },
                             '\n' | '\r' => { // TODO: Correct handling of files with both
                                 self.lineno += 1; self.column = 0; index += 1; start += 1;
                                 self.lines.push((self.line_start, index - self.line_start));
                                 self.line_start = index
                             },
                             
                             '(' => { index += 1; token = TokenType::ParenOpen; break },
                             ')' => { index += 1; token = TokenType::ParenClose; break },
                             '[' => { index += 1; token = TokenType::BrackOpen; break },
                             ']' => { index += 1; token = TokenType::BrackClose; break },
                             '{' => { index += 1; token = TokenType::CurlyOpen; break },
                             '}' => { index += 1; token = TokenType::CurlyClose; break },
                             
                             ',' => { index += 1; token = TokenType::Comma; break },
                             ';' => { index += 1; token = TokenType::Semicolon; break },
                             '_' => { index += 1; token = TokenType::Underscore; break },
                             '~' => { index += 1; token = TokenType::Tilde; break },
                             
                             '!' => { state = 1; index += 1 },
                             '#' => { state = 2; index += 1 },
                             '%' => { state = 3; index += 1 },
                             '&' => { state = 4; index += 1 },
                             '*' => { state = 5; index += 1 },
                             '+' => { state = 6; index += 1 },
                             '-' => { state = 7; index += 1 },
                             '.' => { state = 8; index += 1 },
                             '/' => { state = 9; index += 1 },
                             ':' => { state = 10; index += 1 },
                             '<' => { state = 11; index += 1 },
                             '=' => { state = 12; index += 1 },
                             '>' => { state = 13; index += 1 },
                             '^' => { state = 14; index += 1 },
                             '|' => { state = 15; index += 1 },
                             '0' => { state = 16; index += 1 },
                             '1'..='9' => { state = 17; index += 1 },
                             'a' => { state = 18; index += 1 },
                             'b' => { state = 19; index += 1 },
                             'c' => { state = 20; index += 1 },
                             'e' => { state = 21; index += 1 },
                             'f' => { state = 22; index += 1 },
                             'i' => { state = 23; index += 1 },
                             'l' => { state = 24; index += 1 },
                             'm' => { state = 25; index += 1 },
                             'p' => { state = 26; index += 1 },
                             'r' => { state = 27; index += 1 },
                             's' => { state = 28; index += 1 },
                             't' => { state = 29; index += 1 },
                             'u' => { state = 30; index += 1 },
                             'v' => { state = 31; index += 1 },
                             'w' => { state = 32; index += 1 },
                             'a'..='z' | 'A'..='Z' => { state = 33; index += 1 },
                             _ => { index += 1; token = TokenType::Unknown; break },
                         }
                     },
                1 => if eof { token = TokenType::Bang; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualsNot; break },
                             _ => { token = TokenType::Bang; break },
                         }
                     },
                2 => if eof { token = TokenType::Unknown; break }
                     else {
                         match c {
                             '[' => { state = 34; index += 1 },
                             _ => { token = TokenType::Unknown; break },
                         }
                     },
                3 => if eof { token = TokenType::Mod; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualMod; break },
                             _ => { token = TokenType::Mod; break },
                         }
                     },
                4 => if eof { token = TokenType::And; break }
                     else {
                         match c {
                             '&' => { state = 35; index += 1 },
                             '=' => { index += 1; token = TokenType::EqualAnd; break },
                             _ => { token = TokenType::And; break },
                         }
                     },
                5 => if eof { token = TokenType::Star; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualTimes; break },
                             _ => { token = TokenType::Star; break },
                         }
                     },
                6 => if eof { token = TokenType::Add; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualPlus; break },
                             _ => { token = TokenType::Add; break },
                         }
                     },
                7 => if eof { token = TokenType::Minus; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualMinus; break },
                             _ => { token = TokenType::Minus; break },
                         }
                     },
                8 => if eof { token = TokenType::Dot; break }
                     else {
                         match c {
                             '0'..='9' => { state = 36; index += 1 },
                             _ => { token = TokenType::Dot; break },
                         }
                     },
                9 => if eof { token = TokenType::Div; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualDiv; break },
                             '/' => { state = 37; index += 1 },
                             '*' => { state = 38; index += 1; c_depth = 1 },
                             _ => { token = TokenType::Div; break },
                         }
                     },
                10=> if eof { token = TokenType::Div; break }
                     else {
                        match c {
                            ':' => { index += 1; token = TokenType::Colons; break },
                            _ => { token = TokenType::Colon; break },
                        }
                     },
                11=> if eof { token = TokenType::Less; break }
                     else {
                         match c {
                             '<' => { state = 39; index += 1 },
                             '=' => { index += 1; token = TokenType::LessEq; break },
                             _ => { token = TokenType::Less; break },
                         }
                     },
                12=> if eof { token = TokenType::Equal; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::Equals; break },
                             '>' => { index += 1; token = TokenType::Arrow; break },
                             _ => { token = TokenType::Equal; break },
                         }
                     },
                13=> if eof { token = TokenType::Greater; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::GreaterEq; break },
                             '>' => { state = 40; index += 1 },
                             _ => { token = TokenType::Greater; break },
                         }
                     },
                14=> if eof { token = TokenType::Xor; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualXor; break },
                             _ => { token = TokenType::Xor; break },
                         }
                     },
                15=> if eof { token = TokenType::Bar; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualOr; break },
                             '|' => { state = 41; index += 1 },
                             _ => { token = TokenType::Bar; break },
                         }
                     },
                16=> if eof { token = TokenType::IntDec; break }
                     else {
                         match c {
                             'x' => { state = 42; index += 1 },
                             'b' => { state = 43; index += 1 },
                             'o' => { state = 44; index += 1 },
                             '0'..='9' => { state = 17; index += 1 },
                             '.' => { state = 45; index += 1 },
                             _ => { token = TokenType::IntDec; break },
                         }
                     },
                17=> if eof { token = TokenType::IntDec; break }
                     else {
                         match c {
                             '0'..='9' => index += 1,
                             '.' => { state = 45; index += 1 },
                             _ => { token = TokenType::IntDec; break },
                         }
                     },
                18=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             's' => { state = 46; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                19=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'o' => { state = 47; index += 1 },
                             'y' => { state = 48; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                20=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'o' => { state = 49; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                21=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'l' => { state = 50; index += 1},
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                22=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '3' => { state = 51; index += 1 },
                             '6' => { state = 52; index += 1 },
                             'a' => { state = 53; index += 1 },
                             'n' => { state = 54; index += 1 },
                             'o' => { state = 55; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                23=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '1' => { state = 56; index += 1 },
                             '3' => { state = 57; index += 1 },
                             '6' => { state = 58; index += 1 },
                             '8' => { state = 59; index += 1 },
                             'f' => { state = 60; index += 1 },
                             'n' => { state = 61; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                24=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 62; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                25=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'a' => { state = 63; index += 1 },
                             'o' => { state = 64; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                26=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'u' => { state = 66; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                27=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 67; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                28=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'i' => { state = 68; index += 1 },
                             't' => { state = 69; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                29=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'h' => { state = 70; index += 1 },
                             'o' => { state = 71; index += 1 },
                             'r' => { state = 72; index += 1 },
                             'y' => { state = 73; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                30=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '1' => { state = 74; index += 1 },
                             '3' => { state = 75; index += 1 },
                             '6' => { state = 76; index += 1 },
                             '8' => { state = 77; index += 1 },
                             'n' => { state = 78; index += 1 },
                             's' => { state = 79; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                31=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'o' => { state = 80; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                32=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'h' => { state = 81; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1},
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                33=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                34=> if eof { token = TokenType::Error(LexError::AttrEOF); break }
                     else {
                         match c {
                             ']' => { index += 1; token = TokenType::FuncAttr; break },
                             '\n' => { token = TokenType::Error(LexError::AttrEOL); break },
                             _ => index += 1,
                         }
                     },
                35=> if eof { token = TokenType::Ands; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualAnds; break },
                             _ => { token = TokenType::Ands; break },
                         }
                     },
                36=> if eof { token = TokenType::DotNumber; break }
                     else {
                         match c {
                             '0'..='9' => index += 1,
                             _ => { token = TokenType::DotNumber; break },
                         }
                     },
                37=> if eof { start = index; state = 0 }
                     else {
                         match c {
                             '\n' | '\r' => { start = index; state = 0 },
                             _ => index += 1,
                         }
                     },
                38=> if eof { token = TokenType::Error(LexError::CommentEOF); break }
                     else {
                         match c {
                             '\n' | '\r' => { // TODO: Also handling of both here
                                 self.lineno += 1; self.column = 0; index += 1;
                                 self.lines.push((self.line_start, index - self.line_start));
                                 self.line_start = index
                             },
                             '*' => { state = 82; index += 1 },
                             '/' => { state = 149; index += 1 },
                             _ => index += 1,
                         }
                     },
                39=> if eof { token = TokenType::ShiftL; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualShiftL; break },
                             _ => { token = TokenType::ShiftL; break },
                         }
                     },
                40=> if eof { token = TokenType::ShiftR; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualShiftR; break },
                             _ => { token = TokenType::ShiftR; break },
                         }
                     },
                41=> if eof { token = TokenType::Bars; break }
                     else {
                         match c {
                             '=' => { index += 1; token = TokenType::EqualOrs; break },
                             _ => { token = TokenType::Bars; break },
                         }
                     },
                42=> if eof { token = TokenType::Error(LexError::HexEmpty); break }
                     else {
                         match c {
                             '0'..='9' | 'a'..='f' | 'A'..='F' => { state = 83; index += 1 },
                             'g'..='z' | 'G'..='Z' => {
                                index += 1; token = TokenType::Error(LexError::HexError);
                                break },
                             _ => { token = TokenType::Error(LexError::HexEmpty); break },
                         }
                     },
                43=> if eof { token = TokenType::Error(LexError::BinEmpty); break }
                     else {
                         match c {
                             '0' | '1' => { state = 84; index += 1 },
                             '2'..='9' | 'a'..='z' | 'A'..='Z' => {
                                 index += 1; token = TokenType::Error(LexError::BinError);
                                 break },
                             _ => { token = TokenType::Error(LexError::BinEmpty); break },
                         }
                     },
                44=> if eof { token = TokenType::Error(LexError::OctEmpty); break }
                     else {
                         match c {
                             '0'..='7' => { state = 85; index += 1 },
                             '8' | '9' | 'a'..='z' | 'A'..='Z' => {
                                 index += 1; token = TokenType::Error(LexError::OctError);
                                 break },
                             _ => { token = TokenType::Error(LexError::OctEmpty); break },
                         }
                     },
                45=> if eof { token = TokenType::Float; break }
                     else {
                         match c {
                             '0'..='9' => index += 1,
                             'e' => { state = 86; index += 1 },
                             _ => { token = TokenType::Float; break },
                         }
                     },
                46=> if eof { token = TokenType::As; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::As; break },
                         }
                     },
                47=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'o' => { state = 87; index += 1},
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                48=> if eof { token = TokenType::By; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::By; break },
                         }
                     },
                49=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'n' => { state = 88; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                50=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             's' => { state = 89; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                51=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '2' => { state = 90; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                52=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '4' => { state = 91; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                53=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'l' => { state = 92; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                54=> if eof { token = TokenType::Fn; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Fn; break },
                         }
                     },
                55=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'r' => { state = 93; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                56=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '6' => { state = 94; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                57=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '2' => { state = 95; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                58=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '4' => { state = 96; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                59=> if eof { token = TokenType::I8; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::I8; break },
                         }
                     },
                60=> if eof { token = TokenType::If; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::If; break },
                         }
                     },
                61=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'o' => { state = 97; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                62=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             't' => { state = 98; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                63=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             't' => { state = 99; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                64=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'd' => { state = 100; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                66=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'b' => { state = 102; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                67=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             't' => { state = 103; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                68=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'z' => { state = 104; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                69=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'r' => { state = 105; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                70=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 106; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                71=> if eof { token = TokenType::To; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::To; break },
                         }
                     },
                72=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'u' => { state = 107; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                73=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'p' => { state = 108; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                74=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '6' => { state = 109; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                75=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '2' => { state = 110; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                76=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             '4' => { state = 111; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                77=> if eof { token = TokenType::U8; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::U8; break },
                         }
                     },
                78=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'i' => { state = 112; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                79=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 113; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                80=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'i' => { state = 114; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                81=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'i' => { state = 115; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                82=> if eof { token = TokenType::Error(LexError::CommentEOF); break }
                     else {
                         match c {
                             '/' => { index += 1; c_depth -= 1;
                                      if c_depth == 0 { start = index; state = 0; }
                                      else { state = 38; } },
                             _ => state = 38,
                         }
                     },
                83=> if eof { token = TokenType::IntHex; break }
                     else {
                         match c {
                             '0'..='9' | 'a'..='f' | 'A'..='F' => index += 1,
                             'g'..='z' | 'G'..='Z' => {
                                 index += 1; token = TokenType::Error(LexError::HexError);
                                 break },
                             _ => { token = TokenType::IntHex; break },
                         }
                     },
                84=> if eof { token = TokenType::IntBin; break }
                     else {
                         match c {
                             '0' | '1' => index += 1,
                             '2'..='9' | 'a'..='z' | 'A'..='Z' => {
                                 index += 1; token = TokenType::Error(LexError::BinError);
                                 break },
                             _ => { token = TokenType::IntBin; break },
                         }
                     },
                85=> if eof { token = TokenType::IntOct; break }
                     else {
                         match c {
                             '0'..='7' => index += 1,
                             '8' | '9' | 'a'..='z' | 'A'..='Z' => {
                                 index += 1; token = TokenType::Error(LexError::OctError);
                                 break },
                             _ => { token = TokenType::IntOct; break },
                         }
                     },
                86=> if eof { token = TokenType::Error(LexError::FloatExpEmpty); break }
                     else {
                         match c {
                             '0'..='9' => { state = 116; index += 1 },
                             _ => { token = TokenType::Error(LexError::FloatExpEmpty); break },
                         }
                     },
                87=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'l' => { state = 117; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                88=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             's' => { state = 118; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                89=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 119; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                90=> if eof { token = TokenType::F32; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::F32; break },
                         }
                     },
                91=> if eof { token = TokenType::F64; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::F64; break },
                         }
                     },
                92=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             's' => { state = 120; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                93=> if eof { token = TokenType::For; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::For; break },
                         }
                     },
                94=> if eof { token = TokenType::I16; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::I16; break },
                         }
                     },
                95=> if eof { token = TokenType::I32; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::I32; break },
                         }
                     },
                96=> if eof { token = TokenType::I64; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::I64; break },
                         }
                     },
                97=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'u' => { state = 121; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
                98=> if eof { token = TokenType::Let; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Let; break },
                         }
                     },
                99=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'c' => { state = 122; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               100=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'u' => { state = 123; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               102=> if eof { token = TokenType::Pub; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Pub; break },
                         }
                     },
               103=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'u' => { state = 125; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               104=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 126; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               105=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'u' => { state = 127; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               106=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'n' => { state = 128; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               107=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 129; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               108=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 130; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               109=> if eof { token = TokenType::U16; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::U16; break },
                         }
                     },
               110=> if eof { token = TokenType::U32; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::U32; break },
                         }
                     },
               111=> if eof { token = TokenType::U64; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::U64; break },
                         }
                     },
               112=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'o' => { state = 131; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               113=> if eof { token = TokenType::Use; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Use; break },
                         }
                     },
               114=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'd' => { state = 132; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               115=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'l' => { state = 133; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               116=> if eof { token = TokenType::Float; break }
                     else {
                         match c {
                             '0'..='9' => index += 1,
                             _ => { token = TokenType::Float; break },
                         }
                     },
               117=> if eof { token = TokenType::Bool; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Bool; break },
                         }
                     },
               118=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             't' => { state = 134; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               119=> if eof { token = TokenType::Else; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Else; break },
                         }
                     },
               120=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 135; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               121=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             't' => { state = 136; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               122=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'h' => { state = 137; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               123=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'l' => { state = 138; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               125=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'r' => { state = 140; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               126=> if eof { token = TokenType::Size; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Size; break },
                         }
                     },
               127=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'c' => { state = 141; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               128=> if eof { token = TokenType::Then; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Then; break },
                         }
                     },
               129=> if eof { token = TokenType::True; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::True; break },
                         }
                     },
               130=> if eof { token = TokenType::Type; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Type; break },
                         }
                     },
               131=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'n' => { state = 142; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               132=> if eof { token = TokenType::Void; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Void; break },
                         }
                     },
               133=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 143; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               134=> if eof { token = TokenType::Const; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Const; break },
                         }
                     },
               135=> if eof { token = TokenType::False; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::False; break },
                         }
                     },
               136=> if eof { token = TokenType::Inout; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Inout; break },
                         }
                     },
               137=> if eof { token = TokenType::Match; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Match; break },
                         }
                     },
               138=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'e' => { state = 144; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               140=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             'n' => { state = 146; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               141=> if eof { token = TokenType::Ident; break }
                     else {
                         match c {
                             't' => { state = 147; index += 1 },
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Ident; break },
                         }
                     },
               142=> if eof { token = TokenType::Union; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Union; break },
                         }
                     },
               143=> if eof { token = TokenType::While; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::While; break },
                         }
                     },
               144=> if eof { token = TokenType::Module; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Module; break },
                         }
                     },
               146=> if eof { token = TokenType::Return; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Return; break },
                         }
                     },
               147=> if eof { token = TokenType::Struct; break }
                     else {
                         match c {
                             'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_'
                                 => { state = 33; index += 1 },
                             _ => { token = TokenType::Struct; break },
                         }
                     },
               149=> if eof { token = TokenType::Error(LexError::CommentEOF); break }
                     else {
                         match c {
                             '*' => { c_depth += 1; state = 38; index += 1 },
                             _ => state = 38,
                         }
                     },
                _ => { token = TokenType::Error(LexError::InternalError); break },
            }
        }
        
        self.index = index;
        return Token{ tok : token, line : self.lineno, col : self.column,
                      start : start, len : index - start };
    }
}
