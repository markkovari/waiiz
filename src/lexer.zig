const token = @import("token.zig");
const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const Allocator = std.mem.Allocator;

const TokenType = token.TokenType;
const Token = token.Token;

/// Lexer is a struct that holds the input string and the current position in the input string.
/// It also holds the current character and the next character.
/// The Lexer struct is used to tokenize the input string.
pub const Lexer = struct {
    identifierLookup: std.StringHashMap(TokenType),
    /// The input string.
    input: []const u8,

    /// The current position in input (points to current char).
    position: usize = 0,

    // current reader position in input (after current char).
    readPosition: usize = 0,

    /// The current character in the input string.
    character: u8 = 0,

    fn init(input: []const u8, allocator: Allocator) Lexer {
        var identLookup = std.StringHashMap(TokenType).init(allocator);
        identLookup.put("let", .LET) catch |err| {
            std.debug.print("cannot create hashmap entry {}", .{err});
        };
        identLookup.put("fn", .FUNCTION) catch |err| {
            std.debug.print("cannot create hashmap entry {}", .{err});
        };
        var lexer = Lexer{
            .input = input,
            .position = 0,
            .readPosition = 0,
            .character = input[0],
            .identifierLookup = identLookup,
        };

        // Read the first character on initialization.
        lexer.readChar();
        return lexer;
    }
    fn deinit(self: *Lexer) void {
        self.identifierLookup.deinit();
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
            else => {
                if (isLetter(self.character)) {
                    const literal = self.readIdentifier();
                    if (self.identifierLookup.get(literal)) |tType| {
                        return Token.new(tType, literal);
                    } else {
                        return Token.new(TokenType.ILLEGAL, &[1]u8{self.character});
                    }
                } else {
                    return Token.new(TokenType.ILLEGAL, &[1]u8{self.character});
                }
            },
        };
        // Progress to the next character.
        self.readChar();
        return currentToken;
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;
        while (isLetter(self.character)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }
};

fn isLetter(that: u8) bool {
    return ('a' <= that and that <= 'z') or ('A' <= that and that <= 'Z');
}
test "isLetter works as expected" {
    try testing.expect(isLetter('a'));
    try testing.expect(isLetter('A'));
    try testing.expect(isLetter('x'));
    try testing.expect(!isLetter('9'));
}

test "lexer can be initialized" {
    const testAllocator = std.testing.allocator;
    const input = "let five = 5;";
    var lexer = Lexer.init(input, testAllocator);
    defer lexer.deinit();
    try testing.expect(lexer.input.len == 13);
}

test "lexer can be deinitialized" {
    const testAllocator = std.testing.allocator;

    const input = "let five = 5;";
    var lexer = Lexer.init(input, testAllocator);
    defer lexer.deinit();
    try testing.expect(lexer.input.len == 13);
}

test "lexer reads one token" {
    const testAllocator = std.testing.allocator;
    const input = "=";
    var lexer = Lexer.init(input, testAllocator);
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
    const testAllocator = std.testing.allocator;

    const input = "=+(){},;";
    var lexer = Lexer.init(input, testAllocator);
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
        try testing.expect(mem.eql(u8, tok.literal, expectedToken.literal));
    }
}

test "lexer reads next tokens with multiple chars" {
    const testAllocator = std.testing.allocator;

    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\let result = add(five, ten);
    ;
    var lexer = Lexer.init(input, testAllocator);
    defer lexer.deinit();
    const tokens = [_]Token{
        Token.new(TokenType.LET, "let"),
        Token.new(TokenType.IDENT, "five"),
        Token.new(TokenType.EQ, "="),
        Token.new(TokenType.INT, "5"),
        Token.new(TokenType.SEMICOLON, ";"),
        Token.new(TokenType.LET, "let"),
        Token.new(TokenType.IDENT, "ten"),
        Token.new(TokenType.EQ, "="),
        Token.new(TokenType.INT, "10"),
        Token.new(TokenType.SEMICOLON, ";"),
        Token.new(TokenType.LET, "let"),
        Token.new(TokenType.IDENT, "add"),
        Token.new(TokenType.EQ, "="),
        Token.new(TokenType.FUNCTION, "fn"),
        Token.new(TokenType.LPAREN, "("),
        Token.new(TokenType.IDENT, "x"),
        Token.new(TokenType.COMMA, ","),
        Token.new(TokenType.IDENT, "y"),
        Token.new(TokenType.RPAREN, ")"),
        Token.new(TokenType.LBRACE, "{"),
        Token.new(TokenType.IDENT, "x"),
        Token.new(TokenType.PLUS, "+"),
        Token.new(TokenType.IDENT, "y"),
        Token.new(TokenType.SEMICOLON, ";"),
        Token.new(TokenType.RBRACE, "}"),
        Token.new(TokenType.SEMICOLON, ";"),
        Token.new(TokenType.LET, "let"),
        Token.new(TokenType.IDENT, "result"),
        Token.new(TokenType.EQ, "="),
        Token.new(TokenType.IDENT, "add"),
        Token.new(TokenType.LPAREN, "("),
        Token.new(TokenType.IDENT, "five"),
        Token.new(TokenType.COMMA, ","),
        Token.new(TokenType.IDENT, "ten"),
        Token.new(TokenType.RPAREN, ")"),
        Token.new(TokenType.SEMICOLON, ";"),
        Token.new(TokenType.EOF, ""),
    };

    for (tokens) |expectedToken| {
        const tok = lexer.nextToken();
        try std.io.getStdOut().writeAll(tok.toString());
        try testing.expect(mem.eql(u8, tok.literal, expectedToken.literal));
    }
}
