const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const Token = enum(u8) {
    space = ' ',
    tab = '\t',
    line_feed = '\n',
};

pub fn readTokens(allocator: Allocator, reader: anytype) ![]const Token {
    var tokens = ArrayList(Token).init(allocator);
    errdefer tokens.deinit();
    while (try readToken(reader)) |token| {
        try tokens.append(token);
    }
    return tokens.toOwnedSlice();
}

fn readToken(reader: anytype) !?Token {
    const byte = reader.readByte() catch |err| {
        if (err == error.EndOfStream) {
            return null;
        }
        return err;
    };
    switch (byte) {
        ' ', '\t', '\n' => return @enumFromInt(byte),
        else => return @call(.always_tail, readToken, .{reader}),
    }
}
