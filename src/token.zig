const tests = @import("std").testing;
const expect = tests.expect;

pub const TokenType = enum { ILLEGAL, EOF, IDENT, INT, ASSIGN, PLUS, COMMA, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, FUNCTION, LET, EQ };

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
