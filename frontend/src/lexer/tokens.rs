pub enum LexError {
    AttrEOF, AttrEOL, CommentEOF,
    HexEmpty, BinEmpty, OctEmpty, FloatExpEmpty,
    HexError, BinError, OctError,
    InternalError,
}

pub enum TokenType {
    As, By, Const, Else, False, Fn, For, If, Inout, Let, Match, Module, Pub, Return, Size, Struct,
    Then, To, True, Type, Union, Use, While,

    Bool, I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, Void,
    
    Add, And, Ands, Arrow, Bang, Bar, Bars, Colon, Colons, Comma, Div, Dot, Equal, EqualAnd,
    EqualAnds, EqualDiv, EqualMinus, EqualMod, EqualOr, EqualOrs, EqualPlus, EqualShiftL,
    EqualShiftR, EqualTimes, EqualXor, Equals, EqualsNot, Greater, GreaterEq, Less, LessEq, Minus,
    Mod, Semicolon, ShiftL, ShiftR, Star, Tilde, Underscore, Xor,

    CurlyOpen, CurlyClose,
    ParenOpen, ParenClose,
    BrackOpen, BrackClose,

    Ident, IntDec, IntHex, IntBin, IntOct,
    Float,
    DotNumber,
    FuncAttr,

    Unknown, EOF, Error(LexError),
}
