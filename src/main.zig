const std = @import("std");
const lexer = @import("lexer.zig");
const repl = @import("repl.zig");

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    // Run the REPL
    try repl.start(alloc);
}

// test "simple test" {
//     var list = std.ArrayList(i32).init(std.testing.allocator);
//     defer list.deinit();
//     try list.append(42);
//     try std.testing.expectEqual(@as(i32, 42), list.pop());
// }
