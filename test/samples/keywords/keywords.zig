const std = @import("std");
const builtin = @import("builtin");

/// may conflict in c#
const Console = struct {
    /// the first letter of the alphabet
    a: i32,
    /// an integer
    static: i32,
};

/// may conflict in c#
const System = struct {
    /// not the end of the function
    return_: i32,
    /// not nothing
    void_: []i32,
};

/// not the main function
const Main = struct {
    /// not an integer
    int: System,
    /// should not cause conflict
    if_true: i32,
};

/// 'if_' not a condition
/// 'class' not a class
/// 'i' just a string
/// 'in' not in
/// 'for_' not a loop
/// 'words' contains lots of things
/// 'words_1' an integer
fn keywords(
    if_: i32,
    class: []const u8,
    i: []const u8,
    in: Console,
    for_: []i32,
    words: []Main,
    words_1: i32,
) !void {
    // If this compiles, it is already a good step!
    _ = .{ if_, class, i, in, for_, words, words_1 };
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator); // Allocates memory to read inputs
    defer arena.deinit(); // Frees the memory at the end of the scope
    const allocator = arena.allocator();
    const lineReader = LineReader.init(allocator); // The reader used to parse the input

    const if_ = try lineReader.readValue(i32);
    const class = try lineReader.read();
    const i = try lineReader.read();
    const in = try lineReader.readType(Console, &[_]usize{}, ' ');
    const for_ = try lineReader.readNElements(i32, ' ', @intCast(if_));
    const words = try lineReader.readType([]Main, &[_]usize{ 2, 3 }, ' ');
    const words_1 = try lineReader.readValue(i32);

    try keywords(
        if_,
        class,
        i,
        in,
        for_,
        words,
        words_1,
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

    /// Reads all elements on a line, splits them by `delimiter` and parses them
    /// Asserts there is atlease 1 element
    fn readList(self: Self, comptime ReturnType: type, delimiter: u8) ![]ReturnType {
        const line = try self.read();
        defer self.allocator.free(line);
        var values = std.mem.splitScalar(u8, line, delimiter);
        var output = std.ArrayList(ReturnType).init(self.allocator);

        while (values.next()) |v| {
            try output.append(try self.parseType(ReturnType, v));
        }

        return try output.toOwnedSlice();
    }

    /// Reads N elements on a line, splits them by `delimiter` and parses them
    /// Asserts there is atlease 1 element
    fn readNElements(
        self: Self,
        comptime ReturnType: type,
        delimiter: u8,
        amount: usize,
    ) ![]ReturnType {
        const line = try self.read();
        defer self.allocator.free(line);
        var values = std.mem.splitScalar(u8, line, delimiter);
        var output = try std.ArrayList(ReturnType).initCapacity(
            self.allocator,
            amount,
        );

        for (0..amount) |_| {
            const v = values.next() orelse unreachable;
            output.appendAssumeCapacity(try self.parseType(ReturnType, v));
        }

        return try output.toOwnedSlice();
    }

    /// Reads a complex type
    /// The shape needs to contain atleast n - 1 shapes
    /// the shapes of inside structs should be flattened
    /// The caller is responsible for freeing all the memory
    fn readType(
        self: Self,
        comptime ReturnType: type,
        shape: []const usize,
        delimiter: u8,
    ) !ReturnType {
        const typeInfo = @typeInfo(ReturnType);
        switch (typeInfo) {
            .Int, .Float => {
                return self.readValue(ReturnType);
            },
            .Pointer => {
                const childType = typeInfo.Pointer.child;
                if (ReturnType == []const u8) {
                    return self.read();
                }
                switch (@typeInfo(childType)) {
                    .Int, .Float => {
                        if (shape.len == 0) {
                            return self.readList(childType, delimiter);
                        } else return self.readNElements(
                            childType,
                            delimiter,
                            shape[0],
                        );
                    },
                    .Pointer, .Struct => {
                        if (ReturnType == []const u8) {
                            return try self.read();
                        }
                        var values = try std.ArrayList(childType).initCapacity(
                            self.allocator,
                            shape[0],
                        );
                        for (shape[0]) |_| {
                            values.appendAssumeCapacity(try self.readType(
                                childType,
                                shape[1..],
                                delimiter,
                            ));
                        }
                        return values.toOwnedSlice();
                    },
                    else => return error.UnsupportedType,
                }
            },
            .Struct => {
                const s = try self.allocator.create(ReturnType);
                var line: []const u8 = "";
                var currentIndex: usize = 0;
                var subShape = shape;
                inline for (typeInfo.Struct.fields) |field| {
                    const fieldInfo = @typeInfo(field.type);
                    switch (fieldInfo) {
                        .Int, .Float, .Pointer, .Struct => {
                            if (fieldInfo == .Int or fieldInfo == .Float or
                                (field.type == []const u8 and subShape.len > 0 and subShape[0] == 1))
                            {
                                if (field.type == []const u8 and subShape[0] == 1)
                                    subShape = subShape[1..];
                                if (line.len == currentIndex) {
                                    line = try self.read();
                                }
                                const index = std.mem.indexOfScalarPos(
                                    u8,
                                    line,
                                    currentIndex,
                                    delimiter,
                                );
                                if (index == null) {
                                    @field(s, field.name) = try self.parseType(
                                        field.type,
                                        line[currentIndex..],
                                    );
                                    self.allocator.free(line);
                                    currentIndex = 0;
                                    line = "";
                                } else {
                                    @field(s, field.name) = try self.parseType(
                                        field.type,
                                        line[currentIndex..index.?],
                                    );
                                    currentIndex = index.? + 1;
                                }
                            } else {
                                self.allocator.free(line);
                                line = "";
                                currentIndex = 0;
                                @field(s, field.name) = try self.readType(
                                    field.type,
                                    subShape,
                                    delimiter,
                                );
                                if (subShape.len > 0)
                                    subShape = subShape[1..];
                            }
                        },
                        else => return error.UnsupportedType,
                    }
                }
                self.allocator.free(line);
                defer self.allocator.destroy(s);
                return s.*;
            },
            else => return error.UnsupportedType,
        }
    }
};
