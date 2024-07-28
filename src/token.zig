pub const TokenType = enum { ILLEGAL, EOF, IDENT, INT, ASSIGN, PLUS, COMMA, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, FUNCTION, LET, EQ };

pub const Token = struct {
    token_type: TokenType,
    literal: []u8,
};
