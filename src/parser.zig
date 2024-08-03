const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;

const Program = @import("ast.zig").ProgramNode;

pub const Parser = struct {
    lexer: Lexer,
    curToken: Token,
    peekToken: Token,

    pub fn new(lexer: *Lexer) Parser {
        var _lexer = lexer;
        const _currentToken = _lexer.nextToken();
        const _nextToken = _lexer.nextToken();
        return Parser{
            .lexer = _lexer,
            .curToken = _currentToken,
            .peekToken = _nextToken,
        };
    }

    pub fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Parser) ?Program {
        while (self.curToken.tokenType != .EOF) {
            self.nextToken();
        }
        return null;
    }
};

const testing = @import("std").testing;
test "testLetStatements" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    const lexer = Lexer.init(input, testing.allocator);
    const parser = Parser.new(&lexer);
    try testing.expect(parser != null);
    // const parser = Parser.new(lexer);
    // const program = parser.parseProgram();
    // try testing.expect(program != null);
}
