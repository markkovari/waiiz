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
    readPosition: usize = 1,

    /// The current character in the input string.
    character: u8 = 0,

    fn init(input: []const u8) Lexer {
        var lexer = Lexer{
            .input = input,
            .position = 0,
            .readPosition = 1,
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
            '=' => newToken(TokenType.EQ, self.character),
            ';' => newToken(TokenType.SEMICOLON, self.character),
            '(' => newToken(TokenType.LPAREN, self.character),
            ')' => newToken(TokenType.RPAREN, self.character),
            ',' => newToken(TokenType.COMMA, self.character),
            '+' => newToken(TokenType.PLUS, self.character),
            '{' => newToken(TokenType.LBRACE, self.character),
            '}' => newToken(TokenType.RBRACE, self.character),
            0 => Token{ .token_type = TokenType.EOF, .literal = [_]u8{} },
        };

        // Progress to the next character.
        self.readChar();
        return currentToken;
    }
};

fn newToken(tokenType: TokenType, ch: u8) Token {
    return Token{ .token_type = tokenType, .literal = &ch };
}

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

test "lexer reads the initial tokens" {
    const input = "=+(){},;";
    var lexer = Lexer.init(input);
    defer lexer.deinit();
    const tokens: []Token = [_]Token{
        Token{ .token_type = TokenType.EQ, .literal = '=' },
        Token{ .token_type = TokenType.PLUS, .literal = '+' },
        Token{ .token_type = TokenType.LPAREN, .literal = "(" },
        Token{ .token_type = TokenType.RPAREN, .literal = ")" },
        Token{ .token_type = TokenType.LBRACE, .literal = "{" },
        Token{ .token_type = TokenType.RBRACE, .literal = "}" },
        Token{ .token_type = TokenType.COMMA, .literal = "," },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },
        Token{ .token_type = TokenType.EOF, .literal = []u8{} },
    };
    for (tokens) |expectedToken| {
        const tok = lexer.nextToken();
        try testing.expect(tok.token_type == expectedToken.token_type);
        try testing.expect(mem.eql(u8, tok.literal, expectedToken.literal));
    }
}
