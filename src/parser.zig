const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Token = @import("tokenizer.zig").Token;
const big_int = std.math.big.int;

pub fn readInstructions(allocator: Allocator, tokens: []const Token) ![]const Instruction {
    var instructions = ArrayList(Instruction).init(allocator);
    errdefer instructions.deinit();
    var rest_tokens = tokens;
    while (rest_tokens.len > 0) {
        const instruction = try Instruction.fromTokens(rest_tokens);
        try instructions.append(instruction.instruction);
        rest_tokens = instruction.rest;
    }
    return instructions.toOwnedSlice();
}

/// Instruction Modification Parameter
pub const IMP = enum {
    stack_manipulation,
    arithmetic,
    heap_access,
    flow_control,
    io,

    pub fn fromTokens(tokens: []const Token) error{UnexpectedEOF}!struct { variant: IMP, rest: []const Token } {
        if (tokens.len == 0) {
            return error.UnexpectedEOF;
        }

        switch (tokens[0]) {
            Token.space => return .{ .variant = .stack_manipulation, .rest = tokens[1..] },
            Token.line_feed => return .{ .variant = .flow_control, .rest = tokens[1..] },
            Token.tab => {
                if (tokens.len < 2) {
                    return error.UnexpectedEOF;
                }
                switch (tokens[1]) {
                    Token.space => return .{ .variant = .arithmetic, .rest = tokens[2..] },
                    Token.tab => return .{ .variant = .heap_access, .rest = tokens[2..] },
                    Token.line_feed => return .{ .variant = .io, .rest = tokens[2..] },
                }
            },
        }
    }
};

/// Instruction
///
/// `@sizeOf(Instruction) == 32`
pub const Instruction = union(IMP) {
    stack_manipulation: StackManipulationCommand,
    arithmetic: ArithmeticCommand,
    heap_access: HeapAccessCommand,
    flow_control: FlowControlCommand,
    io: IOCommand,

    const Error = error{
        UnexpectedToken,
        UnexpectedEOF,
    };

    pub fn fromTokens(tokens: []const Token) Error!struct { instruction: Instruction, rest: []const Token } {
        const imp = try IMP.fromTokens(tokens);
        switch (imp.variant) {
            .stack_manipulation => {
                const command = try StackManipulationCommand.fromTokens(imp.rest);
                return .{ .instruction = .{ .stack_manipulation = command.command }, .rest = command.rest };
            },
            .arithmetic => {
                const command = try ArithmeticCommand.fromTokens(imp.rest);
                return .{ .instruction = .{ .arithmetic = command.command }, .rest = command.rest };
            },
            .heap_access => {
                const command = try HeapAccessCommand.fromTokens(imp.rest);
                return .{ .instruction = .{ .heap_access = command.command }, .rest = command.rest };
            },
            .flow_control => {
                const command = try FlowControlCommand.fromTokens(imp.rest);
                return .{ .instruction = .{ .flow_control = command.command }, .rest = command.rest };
            },
            .io => {
                const command = try IOCommand.fromTokens(imp.rest);
                return .{ .instruction = .{ .io = command.command }, .rest = command.rest };
            },
        }
    }

    pub fn toString(self: *const Instruction, allocator: Allocator) ![]const u8 {
        return switch (self.*) {
            .stack_manipulation => |command| try std.fmt.allocPrint(allocator, "stack_manipulation( {s} )", .{try command.toString(allocator)}),
            .arithmetic => |command| try std.fmt.allocPrint(allocator, "arithmetic( {s} )", .{command.toString()}),
            .heap_access => |command| try std.fmt.allocPrint(allocator, "heap_access( {s} )", .{command.toString()}),
            .flow_control => |command| try std.fmt.allocPrint(allocator, "flow_control( {s} )", .{try command.toString(allocator)}),
            .io => |command| try std.fmt.allocPrint(allocator, "io( {s} )", .{command.toString()}),
        };
    }
};

/// Stack Manipulation Command
///
/// `@sizeOf(StackManipulationCommand) == 24`
pub const StackManipulationCommand = union(enum) {
    push: Number,
    duplicate,
    copy: Number,
    swap,
    discard,
    slide: Number,

    const Error = error{
        UnexpectedToken,
        UnexpectedEOF,
    };

    pub fn fromTokens(tokens: []const Token) Error!struct { command: StackManipulationCommand, rest: []const Token } {
        if (tokens.len == 0) {
            return error.UnexpectedEOF;
        }

        switch (tokens[0]) {
            .space => {
                const number = try Number.fromTokens(tokens[1..]);
                return .{ .command = .{ .push = number.number }, .rest = number.rest };
            },
            .tab, .line_feed => {
                if (tokens.len < 2) {
                    return error.UnexpectedEOF;
                }
                switch (tokens[0]) {
                    .tab => switch (tokens[1]) {
                        .space => {
                            const number = try Number.fromTokens(tokens[2..]);
                            return .{ .command = .{ .copy = number.number }, .rest = number.rest };
                        },
                        .line_feed => {
                            const number = try Number.fromTokens(tokens[2..]);
                            return .{ .command = .{ .slide = number.number }, .rest = number.rest };
                        },
                        else => return error.UnexpectedToken,
                    },
                    .line_feed => switch (tokens[1]) {
                        .space => return .{ .command = .duplicate, .rest = tokens[2..] },
                        .tab => return .{ .command = .swap, .rest = tokens[2..] },
                        .line_feed => return .{ .command = .discard, .rest = tokens[2..] },
                    },
                    else => unreachable,
                }
            },
        }
    }

    pub fn toString(self: *const StackManipulationCommand, allocator: Allocator) ![]const u8 {
        return switch (self.*) {
            .push => |number| try std.fmt.allocPrint(allocator, "push( {s} )", .{try number.toString(allocator)}),
            .duplicate => "duplicate",
            .copy => |number| try std.fmt.allocPrint(allocator, "copy( {s} )", .{try number.toString(allocator)}),
            .swap => "swap",
            .discard => "discard",
            .slide => |number| try std.fmt.allocPrint(allocator, "slide( {s} )", .{try number.toString(allocator)}),
        };
    }
};

/// Arithmetic Command
///
/// `@sizeOf(ArithmeticCommand) == 1`
pub const ArithmeticCommand = enum {
    add,
    subtract,
    multiply,
    divide,
    modulo,

    pub fn fromTokens(tokens: []const Token) !struct { command: ArithmeticCommand, rest: []const Token } {
        if (tokens.len < 2) {
            return error.UnexpectedEOF;
        }

        switch (tokens[0]) {
            .space => switch (tokens[1]) {
                .space => return .{ .command = .add, .rest = tokens[2..] },
                .tab => return .{ .command = .subtract, .rest = tokens[2..] },
                .line_feed => return .{ .command = .multiply, .rest = tokens[2..] },
            },
            .tab => switch (tokens[1]) {
                .space => return .{ .command = .divide, .rest = tokens[2..] },
                .tab => return .{ .command = .modulo, .rest = tokens[2..] },
                else => return error.UnexpectedToken,
            },
            else => return error.UnexpectedToken,
        }
    }

    pub fn toString(self: ArithmeticCommand) []const u8 {
        return switch (self) {
            .add => "add",
            .subtract => "subtract",
            .multiply => "multiply",
            .divide => "divide",
            .modulo => "modulo",
        };
    }
};

/// Heap Access Command
///
/// `@sizeOf(HeapAccessCommand) == 1`
pub const HeapAccessCommand = enum {
    store,
    retrieve,

    pub fn fromTokens(tokens: []const Token) !struct { command: HeapAccessCommand, rest: []const Token } {
        if (tokens.len == 0) {
            return error.UnexpectedEOF;
        }

        switch (tokens[0]) {
            .space => return .{ .command = .store, .rest = tokens[1..] },
            .tab => return .{ .command = .retrieve, .rest = tokens[1..] },
            else => return error.UnexpectedToken,
        }
    }

    pub fn toString(self: HeapAccessCommand) []const u8 {
        return switch (self) {
            .store => "store",
            .retrieve => "retrieve",
        };
    }
};

/// Flow Control Command
///
/// `@sizeOf(FlowControlCommand) == 24`
pub const FlowControlCommand = union(enum) {
    mark: Label,
    call: Label,
    jump: Label,
    jump_if_zero: Label,
    jump_if_negative: Label,
    end_subroutine,
    end_program,

    pub fn fromTokens(tokens: []const Token) !struct { command: FlowControlCommand, rest: []const Token } {
        if (tokens.len < 2) {
            return error.UnexpectedEOF;
        }

        switch (tokens[0]) {
            .space => switch (tokens[1]) {
                .space => {
                    const label = try Label.fromTokens(tokens[2..]);
                    return .{ .command = .{ .mark = label.label }, .rest = label.rest };
                },
                .tab => {
                    const label = try Label.fromTokens(tokens[2..]);
                    return .{ .command = .{ .call = label.label }, .rest = label.rest };
                },
                .line_feed => {
                    const label = try Label.fromTokens(tokens[2..]);
                    return .{ .command = .{ .jump = label.label }, .rest = label.rest };
                },
            },
            .tab => switch (tokens[1]) {
                .space => {
                    const label = try Label.fromTokens(tokens[2..]);
                    return .{ .command = .{ .jump_if_zero = label.label }, .rest = label.rest };
                },
                .tab => {
                    const label = try Label.fromTokens(tokens[2..]);
                    return .{ .command = .{ .jump_if_negative = label.label }, .rest = label.rest };
                },
                .line_feed => return .{ .command = .end_subroutine, .rest = tokens[2..] },
            },
            .line_feed => switch (tokens[1]) {
                .line_feed => return .{ .command = .end_program, .rest = tokens[2..] },
                else => return error.UnexpectedToken,
            },
        }
    }

    pub fn toString(self: *const FlowControlCommand, allocator: Allocator) ![]const u8 {
        return switch (self.*) {
            .mark => |label| try std.fmt.allocPrint(allocator, "mark( {s} )", .{try label.toString(allocator)}),
            .call => |label| try std.fmt.allocPrint(allocator, "call( {s} )", .{try label.toString(allocator)}),
            .jump => |label| try std.fmt.allocPrint(allocator, "jump( {s} )", .{try label.toString(allocator)}),
            .jump_if_zero => |label| try std.fmt.allocPrint(allocator, "jump_if_zero( {s} )", .{try label.toString(allocator)}),
            .jump_if_negative => |label| try std.fmt.allocPrint(allocator, "jump_if_negative( {s} )", .{try label.toString(allocator)}),
            .end_subroutine => "end_subroutine",
            .end_program => "end_program",
        };
    }
};

/// I/O Command
///
/// `@sizeOf(IOCommand) == 1`
pub const IOCommand = enum {
    print_char,
    print_number,
    read_char,
    read_number,

    pub fn fromTokens(tokens: []const Token) !struct { command: IOCommand, rest: []const Token } {
        if (tokens.len < 2) {
            return error.UnexpectedEOF;
        }

        switch (tokens[0]) {
            .space => switch (tokens[1]) {
                .space => return .{ .command = .print_char, .rest = tokens[2..] },
                .tab => return .{ .command = .print_number, .rest = tokens[2..] },
                else => return error.UnexpectedToken,
            },
            .tab => switch (tokens[1]) {
                .space => return .{ .command = .read_char, .rest = tokens[2..] },
                .tab => return .{ .command = .read_number, .rest = tokens[2..] },
                else => return error.UnexpectedToken,
            },
            else => return error.UnexpectedToken,
        }
    }

    pub fn toString(self: IOCommand) []const u8 {
        return switch (self) {
            .print_char => "print_char",
            .print_number => "print_number",
            .read_char => "read_char",
            .read_number => "read_number",
        };
    }
};

/// `@sizeOf(Number) == 16`
pub const Number = struct {
    label: Label,

    const Error = error{
        UnexpectedToken,
        UnexpectedEOF,
    };

    pub fn fromTokens(tokens: []const Token) Error!struct { number: Number, rest: []const Token } {
        if (tokens.len < 2) {
            return error.UnexpectedEOF;
        }

        const value = try Label.fromTokens(tokens);
        return .{ .number = .{ .label = value.label }, .rest = value.rest };
    }

    pub fn positive(self: Number) bool {
        return switch (self.label.value[0]) {
            .space => true,
            .tab => false,
            else => unreachable,
        };
    }

    pub fn abs_value(self: Number) Label {
        return .{ .value = self.label.value[1..] };
    }

    // TODO: optimize
    pub fn toBigInt(self: *const Number, allocator: Allocator) Allocator.Error!big_int.Managed {
        var result = try big_int.Managed.init(allocator);
        errdefer result.deinit();
        const label = self.abs_value();
        const len = label.len();
        for (label.value, 0..) |token, idx| {
            switch (token) {
                .tab => {
                    const power_of_two = len - idx - 1;
                    var bit = blk: {
                        var bit = try big_int.Managed.initCapacity(allocator, power_of_two + 1);
                        errdefer bit.deinit();
                        try bit.set(1);
                        try bit.shiftLeft(&bit, power_of_two);
                        break :blk bit;
                    };
                    defer bit.deinit();
                    try result.bitOr(&result, &bit);
                },
                .space => continue,
                else => unreachable,
            }
        }
        result.setSign(self.positive());
        return result;
    }

    pub fn toString(self: *const Number, allocator: Allocator) ![]const u8 {
        var number = try self.toBigInt(allocator);
        defer number.deinit();
        return number.toString(allocator, 10, .upper);
    }

    pub fn clone(self: *const Number, allocator: Allocator) !Number {
        return .{ .label = try self.label.clone(allocator) };
    }
};

/// `@sizeOf(Label) == 16`
pub const Label = struct {
    value: [:.line_feed]const Token,

    fn len(self: Label) usize {
        return self.value.len;
    }

    fn fromTokens(tokens: []const Token) error{UnexpectedEOF}!struct { label: Label, rest: []const Token } {
        if (tokens.len == 0) {
            return error.UnexpectedEOF;
        }
        var sentinel_idx: usize = 0;
        while (tokens[sentinel_idx] != .line_feed) {
            if (tokens.len == 0) {
                return error.UnexpectedEOF;
            }
            sentinel_idx += 1;
        }
        return .{ .label = .{ .value = tokens[0..sentinel_idx :.line_feed] }, .rest = tokens[sentinel_idx + 1 ..] };
    }

    fn toString(self: Label, allocator: Allocator) Allocator.Error![]const u8 {
        var result = try allocator.alloc(u8, self.value.len);
        for (self.value, 0..) |token, idx| {
            result[idx] = switch (token) {
                .space => '0',
                .tab => '1',
                else => unreachable,
            };
        }
        return result;
    }

    fn clone(self: Label, allocator: Allocator) !Label {
        return .{ .value = blk: {
            var dest = try allocator.allocSentinel(Token, self.value.len, .line_feed);
            @memcpy(dest, self.value);
            break :blk dest;
        } };
    }
};

test "label" {
    const testing = std.testing;

    const tokens = [_]Token{
        Token.space,
        Token.space,
        Token.tab,
        Token.line_feed,
        Token.tab,
        Token.space,
        Token.line_feed,
    };
    const label = try Label.fromTokens(&tokens);
    try testing.expectEqual(@as(usize, 3), label.label.len);
    try testing.expectEqual(Token.space, label.label[0]);
    try testing.expectEqual(Token.space, label.label[1]);
    try testing.expectEqual(Token.tab, label.label[2]);
    try testing.expectEqual(@as(usize, 3), label.rest.len);
    try testing.expectEqual(Token.tab, label.rest[0]);
    try testing.expectEqual(Token.space, label.rest[1]);
    try testing.expectEqual(Token.line_feed, label.rest[2]);
}

test "empty label" {
    const testing = std.testing;

    const tokens = [_]Token{
        Token.line_feed,
        Token.tab,
        Token.space,
        Token.line_feed,
    };
    const label = try Label.fromTokens(&tokens);
    try testing.expectEqual(@as(usize, 0), label.label.len);
    try testing.expectEqual(@as(usize, 3), label.rest.len);
    try testing.expectEqual(Token.tab, label.rest[0]);
    try testing.expectEqual(Token.space, label.rest[1]);
    try testing.expectEqual(Token.line_feed, label.rest[2]);
}
