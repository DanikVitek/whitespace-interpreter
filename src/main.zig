const std = @import("std");
const Allocator = std.mem.Allocator;
const File = std.fs.File;
const ArrayList = std.ArrayList;
const big_int = std.math.big.int;
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator: Allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    _ = args.skip();
    const file_path = args.next() orelse {
        const stderr = std.io.getStdErr().writer();
        try stderr.print("usage: ws.exe <program file path>\n", .{});
        return error.NoFileProvided;
    };

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();
    const file_reader = file.reader();

    const tokens = try tokenizer.readTokens(allocator, file_reader);
    defer allocator.free(tokens);

    const instructions = try parser.readInstructions(allocator, tokens);
    defer allocator.free(instructions);

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    for (instructions, 0..) |instruction, i| {
        const instruction_str = try instruction.toString(allocator);
        defer allocator.free(instruction_str);
        try stdout.print("{d}: {s}\n", .{ i, instruction_str });
    }

    try bw.flush();
}

test {
    _ = @import("tokenizer.zig");
    _ = @import("parser.zig");
    _ = @import("Vm.zig");
}
