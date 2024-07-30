const tests = @import("std").testing;
const expect = tests.expect;

pub const TokenType = enum { ILLEGAL, EOF, IDENT, INT, ASSIGN, PLUS, COMMA, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, FUNCTION, LET, EQ };

pub const Token = struct {
    tokenType: TokenType,
    literal: []const u8,

    pub fn new(tokenType: TokenType, literal: []const u8) Token {
        return .{ .tokenType = tokenType, .literal = literal };
    }

    pub fn toString(self: Token) []const u8 {
        // return the whole struct serialized into a string
        // for debugging purposes

        // allocate a buffer to hold the string
        var buffer = []u8{0} ** 128;
        var writer = buffer.writer();

        // write the token type
        try writer.print("{s}", self.tokenType);

        // write the literal
        try writer.print(" {s}", self.literal);

        // return the buffer as a slice
        return buffer;
    }
};

test "newToken" {
    const mem = @import("std").mem;
    const token = Token.new(TokenType.PLUS, "+");
    try expect(token.tokenType == TokenType.PLUS);
    try expect(mem.eql(u8, token.literal, "+"));
}
