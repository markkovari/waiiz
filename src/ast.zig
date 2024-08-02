const std = @import("std");

pub const Node = union(enum) {
    statement: *StatementNode,
    expression: *ExpressionNode,

    pub fn tokenLiteral(self: Node) []u8 {
        switch (self) {
            .statement => return self.statement.tokenLiteral(),
            .expression => return self.expression.tokenLiteral(),
        }
    }
};

const StatementNode = struct {
    node: Node,
    pub fn tokenLiteral(self: StatementNode) []u8 {
        return self.node.tokenLiteral();
    }
};

const ExpressionNode = struct {
    node: Node,
    pub fn tokenLiteral(self: ExpressionNode) []u8 {
        return self.node.tokenLiteral();
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
