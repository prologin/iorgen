const std = @import("std");
const builtin = @import("builtin");

/// 'a' a first number
/// 'b' a second number
/// 'c' a third number
/// 'n' This one on a new line
/// 'one_per_line' an integer list, one per line
fn manualFormat(
    a: i32,
    b: i32,
    c: i32,
    n: i32,
    one_per_line: []i32,
) !void {
    // From the function perspective, this is just 4 integers
    _ = .{ a, b, c, n, one_per_line };
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator); // Allocates memory to read inputs
    defer arena.deinit(); // Frees the memory at the end of the scope
    const allocator = arena.allocator();
    const lineReader = LineReader.init(allocator); // The reader used to parse the input

    var i = std.mem.splitScalar(u8, try lineReader.read(), ' ');
    const a = try lineReader.parseType(i32, i.next().?);
    const b = try lineReader.parseType(i32, i.next().?);
    const c = try lineReader.parseType(i32, i.next().?);
    const n = try lineReader.readValue(i32);
    var j = try std.ArrayList(i32).initCapacity(allocator, @intCast(3));
    for (0..@intCast(3)) |_| {
        const k = try lineReader.readValue(i32);
        j.appendAssumeCapacity(k);
    }
    const one_per_line = try j.toOwnedSlice();

    try manualFormat(
        a,
        b,
        c,
        n,
        one_per_line,
    );
}

/// Original at: https://github.com/gaskam-com/zig-utils
const LineReader = struct {
    allocator: std.mem.Allocator,
    reader: std.fs.File.Reader,
    const Self = @This();

    fn init(allocator: std.mem.Allocator) Self {
        return .{ .allocator = allocator, .reader = std.io.getStdIn().reader() };
    }

    /// Reads a line and removes the newline characters(\n, and \r\n for windows)
    fn read(self: Self) ![]const u8 {
        var buffer = std.ArrayList(u8).init(self.allocator);
        errdefer buffer.deinit();
        try self.reader.streamUntilDelimiter(buffer.writer(), '\n', null);
        if (builtin.target.os.tag == .windows and buffer.getLastOrNull() == '\r') _ = buffer.pop();
        return buffer.toOwnedSlice();
    }

    /// Parses a level 1 type
    /// Only accepts Int, Float and string([]const u8) types
    fn parseType(self: Self, comptime ReturnType: type, buf: []const u8) !ReturnType {
        return switch (@typeInfo(ReturnType)) {
            .Int => std.fmt.parseInt(ReturnType, buf, 10),
            .Float => std.fmt.parseFloat(ReturnType, buf),
            // Asserts the type is []const u8
            .Pointer => blk: {
                if (ReturnType != []const u8) return error.UnsupportedType;
                break :blk self.allocator.dupe(u8, buf);
            },
            else => error.UnsupportedType,
        };
    }

    /// Parses one line to a value(only supports int or float types)
    /// Asserts there is a numeric value
    fn readValue(self: Self, comptime ReturnType: type) !ReturnType {
        const value = try read(self);
        defer self.allocator.free(value);
        return self.parseType(ReturnType, value);
    }
};
