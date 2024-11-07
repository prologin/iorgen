const std = @import("std");
const builtin = @import("builtin");

/// A simple struct
const Struct1 = struct {
    /// a field
    foo: i32,
    /// a field
    bar: i32,
};

/// Represents a position
const Position = struct {
    /// X
    x: i32,
    /// Y
    y: i32,
    /// Z
    z: i32,
};

/// A point's name and position
const Point = struct {
    /// the point's name (single character)
    name: []const u8,
    /// the point's description
    description: []const u8,
    /// the point's position
    pos: Position,
};

/// a struct of chars
const Chars = struct {
    /// a first char
    first_char: []const u8,
    /// a second char
    second_char: []const u8,
    /// a third char
    third_char: []const u8,
};

/// contains a big list inside
const WithList = struct {
    /// int
    int: i32,
    /// list nested 3 times!
    big_list: [][][]i32,
};

/// 'struct_' a struct 1 instance
/// 'n' a number
/// 'struct_list' a list a struct 1
/// 'triangle' a triangle
/// 'struct_chars' a struct of chars
/// 'big_list_struct' the big list struct
fn structs(
    struct_: Struct1,
    n: i32,
    struct_list: []Struct1,
    triangle: []Point,
    struct_chars: Chars,
    big_list_struct: WithList,
) !void {
    // Look at them structs.
    _ = .{ struct_, n, struct_list, triangle, struct_chars, big_list_struct };
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator); // Allocates memory to read inputs
    defer arena.deinit(); // Frees the memory at the end of the scope
    const allocator = arena.allocator();
    const lineReader = LineReader.init(allocator); // The reader used to parse the input

    const struct_ = try lineReader.readType(Struct1, &[_]usize{}, ' ');
    const n = try lineReader.readValue(i32);
    const struct_list = try lineReader.readType([]Struct1, &[_]usize{@intCast(n)}, ' ');
    const triangle = try lineReader.readType([]Point, &[_]usize{ 3, 1 }, ' ');
    const struct_chars = try lineReader.readType(Chars, &[_]usize{ 1, 1, 1 }, ' ');
    const big_list_struct = try lineReader.readType(WithList, &[_]usize{ 2, 2, 2 }, ' ');

    try structs(
        struct_,
        n,
        struct_list,
        triangle,
        struct_chars,
        big_list_struct,
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
