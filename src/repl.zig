const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("lexer.zig").Lexer;
const ArrayList = std.ArrayList;
const PROMPT = ">> ";

pub fn start(allocator: Allocator) !void {
    while (true) {
        try std.io.getStdOut().writeAll(PROMPT);
        const stdin = std.io.getStdIn().reader();
        // Print the prompt
        // Read the input from the user
        const bare_line = try stdin.readUntilDelimiterAlloc(
            std.heap.page_allocator,
            '\n',
            8192,
        );

        var lexerInstance = try Lexer.init(bare_line, allocator);
        defer std.heap.page_allocator.free(bare_line);
        // Print the tokens

        while (true) {
            const token = lexerInstance.nextToken();
            if (token.tokenType == .EOF) {
                break;
            }
            std.debug.print("{any} -> {c}\n", .{ token.tokenType, token.literal });
        }
    }
}
