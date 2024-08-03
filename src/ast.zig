const std = @import("std");
const Token = @import("token.zig").Token;

pub const Node = union(enum) {
    statement: *StatementNode,
    expression: *ExpressionNode,
    program: *ProgramNode,

    pub fn tokenLiteral(self: Node) []u8 {
        switch (self) {
            .statement => return self.statement.tokenLiteral(),
            .expression => return self.expression.tokenLiteral(),
        }
    }
};

const StatementNode = union(enum) {
    let: *LetStatement,
    pub fn tokenLiteral() []u8 {
        return "StatementNode";
    }
};

const Identifier = struct {
    token: Token,
    value: []u8,
    pub fn tokenLiteral(self: Identifier) []u8 {
        return self.value;
    }
};

const LetStatement = struct {
    token: Token, // the token.LET token
    name: Identifier,
    value: ExpressionNode,
    pub fn tokenLiteral(self: LetStatement) []u8 {
        var out = std.ArrayList(u8).init(std.heap.page_allocator);
        defer out.deinit();
        out.appendSlice(self.token.literal);
        out.appendSlice(" ");
        out.appendSlice(self.name.tokenLiteral());
        out.appendSlice(" = ");
        if (self.value != null) {
            out.appendSlice(self.value.tokenLiteral());
        }
        out.appendSlice(";");
        return out.toOwnedSlice();
    }
};

const ExpressionNode = struct {
    pub fn tokenLiteral() []u8 {
        return "ExpressionNode";
    }
};

const ProgramNode = struct {
    statements: []StatementNode,
    pub fn tokenLiteral(self: ProgramNode) []u8 {
        var out = std.ArrayList(u8).init(std.heap.page_allocator);
        defer out.deinit();
        for (self.statements) |statement| {
            if (statement != null) {
                out.appendSlice(statement.tokenLiteral());
            }
        }
        return out.toOwnedSlice();
    }
};
