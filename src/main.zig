const std = @import("std");
const Allocator = std.mem.Allocator;
const File = std.fs.File;
const ArrayList = std.ArrayList;
const big_int = std.math.big.int;
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const Vm = @import("vm.zig").Vm;

const USAGE: []const u8 = "Usage: ws.exe <tokenize|parse|run> <program file path>\n";

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator: Allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    _ = args.skip();
    const mode_str = std.mem.trim(u8, args.next() orelse {
        const stderr = std.io.getStdErr().writer();
        try stderr.print(USAGE, .{});
        return error.NoFileProvided;
    }, " \t");
    const Mode = enum { tokenize, parse, run };
    const mode: Mode = blk: {
        if (std.mem.eql(u8, mode_str, "tokenize")) {
            break :blk .tokenize;
        } else if (std.mem.eql(u8, mode_str, "parse")) {
            break :blk .parse;
        } else if (std.mem.eql(u8, mode_str, "run")) {
            break :blk .run;
        } else {
            const stderr = std.io.getStdErr().writer();
            try stderr.print(USAGE, .{});
            return error.InvalidMode;
        }
    };
    const file_path = args.next() orelse {
        const stderr = std.io.getStdErr().writer();
        try stderr.print(USAGE, .{});
        return error.NoFileProvided;
    };

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();
    const file_reader = file.reader();

    const tokens = try tokenizer.readTokens(allocator, file_reader);
    defer allocator.free(tokens);

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    if (mode == .tokenize) {
        for (tokens, 0..) |token, i| {
            try stdout.print("{d}: {any}\n", .{ i, token });
        }

        try bw.flush();
        return;
    }

    const instructions = try parser.readInstructions(allocator, tokens);
    defer allocator.free(instructions);

    if (mode == .parse) {
        for (instructions, 0..) |instruction, i| {
            const instruction_str = try instruction.toString(allocator);
            defer allocator.free(instruction_str);
            try stdout.print("{d}: {s}\n", .{ i, instruction_str });
        }

        try bw.flush();
        return;
    }

    const stdin = std.io.getStdIn().reader();
    var vm = Vm(File.Reader, std.io.BufferedWriter(4096, File.Writer).Writer).init(allocator, stdin, stdout);
    try vm.executeMany(instructions);

    try bw.flush();
}

test {
    _ = @import("tokenizer.zig");
    _ = @import("parser.zig");
    _ = @import("Vm.zig");
}
