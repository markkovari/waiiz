const tests = @import("std").testing;
const expect = tests.expect;

pub const TokenType = enum {
    // Special tokens
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Operators
    EQ,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,

    // Built-in keywords
    LET,
    FUNCTION,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
};

pub const Token = struct {
    tokenType: TokenType,
    literal: []const u8,

    pub fn new(tokenType: TokenType, literal: []const u8) Token {
        return .{ .tokenType = tokenType, .literal = literal };
    }
};

test "newToken" {
    const mem = @import("std").mem;
    const token = Token.new(.PLUS, "+");
    try expect(token.tokenType == .PLUS);
    try expect(mem.eql(u8, token.literal, "+"));
}
