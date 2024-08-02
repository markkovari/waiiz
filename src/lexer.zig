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

    fn init(input: []const u8, allocator: Allocator) !Lexer {
        var identLookup = std.StringHashMap(TokenType).init(allocator);
        try identLookup.put("fn", .FUNCTION);
        try identLookup.put("let", .LET);
        try identLookup.put("true", .TRUE);
        try identLookup.put("false", .FALSE);
        try identLookup.put("if", .IF);
        try identLookup.put("else", .ELSE);
        try identLookup.put("return", .RETURN);

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
        self.skipWhiteSpace();
        const currentToken = switch (self.character) {
            '=' => eqblk: {
                const nextChar = self.peekChar();
                if (nextChar == '=') {
                    self.readChar();
                    break :eqblk Token.new(.EQ, "==");
                } else {
                    break :eqblk Token.new(.ASSIGN, "=");
                }
            },
            '!' => bangBlock: {
                const nextChar = self.peekChar();
                if (nextChar == '=') {
                    self.readChar();
                    break :bangBlock Token.new(.NEQ, "!=");
                } else {
                    break :bangBlock Token.new(.BANG, "!");
                }
            },
            '-' => Token.new(.MINUS, "-"),
            ';' => Token.new(.SEMICOLON, ";"),
            '(' => Token.new(.LPAREN, "("),
            ')' => Token.new(.RPAREN, ")"),
            ',' => Token.new(.COMMA, ","),
            '+' => Token.new(.PLUS, "+"),
            '{' => Token.new(.LBRACE, "{"),
            '}' => Token.new(.RBRACE, "}"),
            '*' => Token.new(.ASTERISK, "*"),
            '/' => Token.new(.SLASH, "/"),
            '<' => Token.new(.LT, "<"),
            '>' => Token.new(.GT, ">"),
            0 => Token.new(.EOF, ""),
            else => {
                if (isLetter(self.character)) {
                    const literal = self.readIdentifier();
                    if (self.identifierLookup.get(literal)) |tType| {
                        return Token.new(tType, literal);
                    } else {
                        return Token.new(.IDENT, literal);
                    }
                } else if (isDigit(self.character)) {
                    return Token.new(.INT, self.readNumber());
                }
                return Token.new(.ILLEGAL, &[1]u8{self.character});
            },
        };
        self.readChar();
        return currentToken;
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.readPosition >= self.input.len) {
            return 0;
        } else {
            return self.input[self.readPosition];
        }
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;
        while (isLetter(self.character)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }

    fn readNumber(self: *Lexer) []const u8 {
        const position = self.position;
        while (isDigit(self.character)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }

    fn skipWhiteSpace(self: *Lexer) void {
        while (self.character == ' ' or self.character == '\t' or self.character == '\n' or self.character == '\r') {
            self.readChar();
        }
    }
};

fn isLetter(that: u8) bool {
    return ('a' <= that and that <= 'z') or ('A' <= that and that <= 'Z') or that == '_';
}

test "isLetter works as expected" {
    try testing.expect(isLetter('a'));
    try testing.expect(isLetter('A'));
    try testing.expect(isLetter('x'));
    try testing.expect(!isLetter('9'));
}

fn isDigit(that: u8) bool {
    return ('0' <= that and that <= '9');
}

test "isDigit works as expected" {
    try testing.expect(isDigit('0'));
    try testing.expect(isDigit('1'));
    try testing.expect(isDigit('8'));
    try testing.expect(!isDigit('a'));
}

test "lexer can be initialized" {
    const testAllocator = std.testing.allocator;
    const input = "let five = 5;";
    var lexer = try Lexer.init(input, testAllocator);
    defer lexer.deinit();
    try testing.expect(lexer.input.len == 13);
}

test "lexer can be deinitialized" {
    const testAllocator = std.testing.allocator;

    const input = "let five = 5;";
    var lexer = try Lexer.init(input, testAllocator);
    defer lexer.deinit();
    try testing.expect(lexer.input.len == 13);
}

test "lexer reads one token" {
    const testAllocator = std.testing.allocator;
    const input = "=";
    var lexer = try Lexer.init(input, testAllocator);
    defer lexer.deinit();
    const tokens = [_]Token{
        Token.new(.ASSIGN, "="),
        Token.new(.EOF, ""),
    };
    try testing.expectEqual(tokens.len, 2);

    const tok = lexer.nextToken();
    try testing.expectEqual(tok.tokenType, .ASSIGN);
    try testing.expect(mem.eql(u8, tok.literal, tokens[0].literal));

    const tok2 = lexer.nextToken();
    try testing.expectEqual(tok2.tokenType, .EOF);
    try testing.expect(mem.eql(u8, tok2.literal, tokens[1].literal));
}

test "lexer reads the initial tokens" {
    const testAllocator = std.testing.allocator;

    const input = "=+(){},;";
    var lexer = try Lexer.init(input, testAllocator);
    defer lexer.deinit();
    const tokens = [_]Token{
        Token.new(.ASSIGN, "="),
        Token.new(.PLUS, "+"),
        Token.new(.LPAREN, "("),
        Token.new(.RPAREN, ")"),
        Token.new(.LBRACE, "{"),
        Token.new(.RBRACE, "}"),
        Token.new(.COMMA, ","),
        Token.new(.SEMICOLON, ";"),
        Token.new(.EOF, ""),
    };
    for (tokens) |expectedToken| {
        const tok = lexer.nextToken();
        try testing.expect(tok.tokenType == expectedToken.tokenType);
        try testing.expect(mem.eql(u8, tok.literal, expectedToken.literal));
    }
}

test "lexer reads next tokens with multiple chars" {
    const testAllocator = std.testing.allocator;

    const input = "let five = 5;";
    var lexer = try Lexer.init(input, testAllocator);
    defer lexer.deinit();
    const tokens = [_]Token{
        Token.new(.LET, "let"),
        Token.new(.IDENT, "five"),
        Token.new(.ASSIGN, "="),
        Token.new(.INT, "5"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.EOF, ""),
    };

    for (tokens) |expectedToken| {
        const tok = lexer.nextToken();
        try testing.expect(mem.eql(u8, tok.literal, expectedToken.literal));
    }
}

test "lexer reads simple assignment expression" {
    const testAllocator = std.testing.allocator;

    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\let result = add(five, ten);
    ;
    var lexer = try Lexer.init(input, testAllocator);
    defer lexer.deinit();
    const tokens = [_]Token{
        Token.new(.LET, "let"),
        Token.new(.IDENT, "five"),
        Token.new(.ASSIGN, "="),
        Token.new(.INT, "5"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.LET, "let"),
        Token.new(.IDENT, "ten"),
        Token.new(.ASSIGN, "="),
        Token.new(.INT, "10"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.LET, "let"),
        Token.new(.IDENT, "add"),
        Token.new(.ASSIGN, "="),
        Token.new(.FUNCTION, "fn"),
        Token.new(.LPAREN, "("),
        Token.new(.IDENT, "x"),
        Token.new(.COMMA, ","),
        Token.new(.IDENT, "y"),
        Token.new(.RPAREN, ")"),
        Token.new(.LBRACE, "{"),
        Token.new(.IDENT, "x"),
        Token.new(.PLUS, "+"),
        Token.new(.IDENT, "y"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.RBRACE, "}"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.LET, "let"),
        Token.new(.IDENT, "result"),
        Token.new(.ASSIGN, "="),
        Token.new(.IDENT, "add"),
        Token.new(.LPAREN, "("),
        Token.new(.IDENT, "five"),
        Token.new(.COMMA, ","),
        Token.new(.IDENT, "ten"),
        Token.new(.RPAREN, ")"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.EOF, ""),
    };

    for (tokens) |expectedToken| {
        const tok = lexer.nextToken();
        try testing.expectEqual(tok.tokenType, expectedToken.tokenType);
        try testing.expect(mem.eql(u8, tok.literal, expectedToken.literal));
    }
}

test "lexer reads simple assignment expression and some standalone mathematical ones, with return statement" {
    const testAllocator = std.testing.allocator;

    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\10 == 10;
        \\10 != 9;
    ;
    var lexer = try Lexer.init(input, testAllocator);
    defer lexer.deinit();
    const tokens = [_]Token{
        Token.new(.LET, "let"),
        Token.new(.IDENT, "five"),
        Token.new(.ASSIGN, "="),
        Token.new(.INT, "5"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.LET, "let"),
        Token.new(.IDENT, "ten"),
        Token.new(.ASSIGN, "="),
        Token.new(.INT, "10"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.LET, "let"),
        Token.new(.IDENT, "add"),
        Token.new(.ASSIGN, "="),
        Token.new(.FUNCTION, "fn"),
        Token.new(.LPAREN, "("),
        Token.new(.IDENT, "x"),
        Token.new(.COMMA, ","),
        Token.new(.IDENT, "y"),
        Token.new(.RPAREN, ")"),
        Token.new(.LBRACE, "{"),
        Token.new(.IDENT, "x"),
        Token.new(.PLUS, "+"),
        Token.new(.IDENT, "y"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.RBRACE, "}"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.LET, "let"),
        Token.new(.IDENT, "result"),
        Token.new(.ASSIGN, "="),
        Token.new(.IDENT, "add"),
        Token.new(.LPAREN, "("),
        Token.new(.IDENT, "five"),
        Token.new(.COMMA, ","),
        Token.new(.IDENT, "ten"),
        Token.new(.RPAREN, ")"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.BANG, "!"),
        Token.new(.MINUS, "-"),
        Token.new(.SLASH, "/"),
        Token.new(.ASTERISK, "*"),
        Token.new(.INT, "5"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.INT, "5"),
        Token.new(.LT, "<"),
        Token.new(.INT, "10"),
        Token.new(.GT, ">"),
        Token.new(.INT, "5"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.IF, "if"),
        Token.new(.LPAREN, "("),
        Token.new(.INT, "5"),
        Token.new(.LT, "<"),
        Token.new(.INT, "10"),
        Token.new(.RPAREN, ")"),
        Token.new(.LBRACE, "{"),
        Token.new(.RETURN, "return"),
        Token.new(.TRUE, "true"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.RBRACE, "}"),
        Token.new(.ELSE, "else"),
        Token.new(.LBRACE, "{"),
        Token.new(.RETURN, "return"),
        Token.new(.FALSE, "false"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.RBRACE, "}"),
        Token.new(.INT, "10"),
        Token.new(.EQ, "=="),
        Token.new(.INT, "10"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.INT, "10"),
        Token.new(.NEQ, "!="),
        Token.new(.INT, "9"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.EOF, ""),
    };

    for (tokens) |expectedToken| {
        const tok = lexer.nextToken();
        try testing.expectEqual(tok.tokenType, expectedToken.tokenType);
        try testing.expect(mem.eql(u8, tok.literal, expectedToken.literal));
    }
}
