const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const parser = @import("parser.zig");
const Number = parser.Number;
const Instruction = parser.Instruction;

pub fn Vm(comptime Reader: type, comptime Writer: type) type {
    return struct {
        allocator: Allocator,
        stack: ArrayList(Number),
        reader: Reader,
        writer: Writer,

        const Self = @This();

        pub fn init(allocator: Allocator, reader: Reader, writer: Writer) Self {
            return .{
                .allocator = allocator,
                .stack = ArrayList(Number).init(allocator),
                .reader = reader,
                .writer = writer,
            };
        }

        pub fn executeMany(self: *Self, instructions: []const Instruction) !void {
            for (instructions) |*instruction| {
                try self.executeOne(instruction);
            }
        }

        pub fn executeOne(self: *Self, instruction: *const Instruction) !void {
            switch (instruction.*) {
                .stack_manipulation => |command| switch (command) {
                    .push => |num| try self.stack.append(num),
                    .duplicate => {
                        if (self.stack.items.len == 0) {
                            return error.StackUnderflow;
                        }
                        try self.stack.append(try self.stack.items[self.stack.items.len - 1].clone(self.allocator));
                    },
                    // .copy => |num|,
                    // .swap,
                    // .discard,
                    // .slide => |num|,
                    else => @panic("unimplemented"),
                },
                else => @panic("unimplemented"),
            }
        }
    };
}
