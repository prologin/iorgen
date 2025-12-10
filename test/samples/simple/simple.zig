const std = @import("std");
const builtin = @import("builtin");

/// 'n' the first number
/// 'other_number' the second number
fn simple(n: i32, other_number: i32) !void {
    // Just do what you want with these numbers, like sum them.
    _ = .{ n, other_number };
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator); // Allocates memory to read inputs
    defer arena.deinit(); // Frees the memory at the end of the scope
    const allocator = arena.allocator();
    const lineReader = try LineReader.init(allocator); // The reader used to parse the input

    const n = try lineReader.readValue(i32);
    const other_number = try lineReader.readValue(i32);

    try simple(n, other_number);
}

/// Original at: https://github.com/gaskam-com/zig-utils
const LineReader = struct {
    allocator: std.mem.Allocator,
    reader: *std.fs.File.Reader,
    stdin_buffer: []u8,
    const Self = @This();

    fn init(allocator: std.mem.Allocator) !Self {
        const reader = try allocator.create(std.fs.File.Reader);
        const buffer = try allocator.alloc(u8, 1024);
        reader.* = std.fs.File.stdin().reader(buffer);
        return .{ .allocator = allocator, .reader = reader, .stdin_buffer = buffer };
    }

    /// Reads a line and removes the newline characters(\n, and \r\n for windows)
    fn read(self: Self) ![]const u8 {
        var buffer: std.Io.Writer.Allocating = .init(self.allocator);
        errdefer buffer.deinit();
        var interface = &self.reader.interface;
        _ = try interface.streamDelimiter(&buffer.writer, '\n');
        // Remove the newline character
        interface.toss(1);
        if (builtin.target.os.tag == .windows and buffer.getLastOrNull() == '\r') _ = buffer.pop();
        return buffer.toOwnedSlice();
    }

    /// Parses a level 1 type
    /// Only accepts int, float and string([]const u8) types
    fn parseType(self: Self, comptime ReturnType: type, buf: []const u8) !ReturnType {
        return switch (@typeInfo(ReturnType)) {
            .int => std.fmt.parseInt(ReturnType, buf, 10),
            .float => std.fmt.parseFloat(ReturnType, buf),
            // Asserts the type is []const u8
            .pointer => blk: {
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

    fn deinit(self: Self) void {
        self.allocator.free(self.stdin_buffer);
    }
};
