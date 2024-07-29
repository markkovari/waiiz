const token = @import("token.zig");
const std = @import("std");
const mem = std.mem;
const testing = std.testing;

const TokenType = token.TokenType;
const Token = token.Token;

/// Lexer is a struct that holds the input string and the current position in the input string.
/// It also holds the current character and the next character.
/// The Lexer struct is used to tokenize the input string.
pub const Lexer = struct {
    /// The input string.
    input: []const u8,

    /// The current position in input (points to current char).
    position: usize = 0,

    // current reader position in input (after current char).
    readPosition: usize = 0,

    /// The current character in the input string.
    character: u8 = 0,

    fn init(input: []const u8) Lexer {
        var lexer = Lexer{
            .input = input,
            .position = 0,
            .readPosition = 0,
            .character = input[0],
        };
        // Read the first character on initialization.
        lexer.readChar();
        return lexer;
    }
    fn deinit(self: *Lexer) void {
        //TODO: Implement deinit if needed.
        _ = self;
    }

    fn readChar(self: *Lexer) void {
        if (self.readPosition >= self.input.len) {
            self.character = 0;
        } else {
            self.character = self.input[self.readPosition];
        }
        self.position = self.readPosition;
        self.readPosition += 1;
    }

    fn nextToken(self: *Lexer) Token {
        const currentToken = switch (self.character) {
            '=' => Token.new(TokenType.EQ, &[1]u8{self.character}),
            ';' => Token.new(TokenType.SEMICOLON, &[1]u8{self.character}),
            '(' => Token.new(TokenType.LPAREN, &[1]u8{self.character}),
            ')' => Token.new(TokenType.RPAREN, &[1]u8{self.character}),
            ',' => Token.new(TokenType.COMMA, &[1]u8{self.character}),
            '+' => Token.new(TokenType.PLUS, &[1]u8{self.character}),
            '{' => Token.new(TokenType.LBRACE, &[1]u8{self.character}),
            '}' => Token.new(TokenType.RBRACE, &[1]u8{self.character}),
            0 => Token.new(TokenType.EOF, ""),
            else => Token.new(TokenType.ILLEGAL, &[1]u8{self.character}),
        };
        // Progress to the next character.
        self.readChar();
        return currentToken;
    }
};

test "lexer can be initialized" {
    const input = "let five = 5;";
    var lexer = Lexer.init(input);
    defer lexer.deinit();
    try testing.expect(lexer.input.len == 13);
}

test "lexer can be deinitialized" {
    const input = "let five = 5;";
    var lexer = Lexer.init(input);
    defer lexer.deinit();
    try testing.expect(lexer.input.len == 13);
}

test "lexer reads one token" {
    const input = "=";
    var lexer = Lexer.init(input);
    defer lexer.deinit();
    const tokens = [_]Token{
        Token.new(TokenType.EQ, "="),
        Token.new(TokenType.EOF, ""),
    };
    for (tokens) |expectedToken| {
        const tok = lexer.nextToken();

        try testing.expect(tok.tokenType == expectedToken.tokenType);
        try testing.expect(mem.eql(u8, tok.literal, expectedToken.literal));
    }
}

test "lexer reads the initial tokens" {
    const input = "=+(){},;";
    var lexer = Lexer.init(input);
    defer lexer.deinit();
    const tokens = [_]Token{
        Token.new(TokenType.EQ, "="),
        Token.new(TokenType.PLUS, "+"),
        Token.new(TokenType.LPAREN, "("),
        Token.new(TokenType.RPAREN, ")"),
        Token.new(TokenType.LBRACE, "{"),
        Token.new(TokenType.RBRACE, "}"),
        Token.new(TokenType.COMMA, ","),
        Token.new(TokenType.SEMICOLON, ";"),
        Token.new(TokenType.EOF, ""),
    };
    for (tokens) |expectedToken| {
        const tok = lexer.nextToken();
        try testing.expect(tok.tokenType == expectedToken.tokenType);
        std.debug.print("expected <{s}> <-> actual <{s}>\n", .{ expectedToken.literal, tok.literal });
        try testing.expect(mem.eql(u8, tok.literal, expectedToken.literal));
    }
}
