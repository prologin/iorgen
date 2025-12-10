const std = @import("std");
const builtin = @import("builtin");

/// contains a list
const List = struct {
    /// the list's size
    size1: i32,
    /// the integer list
    int_list: []i32,
};

/// contains a string
const String = struct {
    /// the list's size
    size2: i32,
    /// the string list
    string_list: []const u8,
};

/// contains a matrix
const Matrix = struct {
    /// the list's size
    size3: i32,
    /// the list list
    list_list: [][]i32,
};

/// this is not a 'sized struct', but a regular one!
const NotASizedStruct = struct {
    /// not the list's size
    size4: i32,
    /// the integer list
    int_list_n: []i32,
};

/// 'n' the size of the lists
/// 'lists' a list of list of different sizes
/// 'strings' a list of strings of different sizes
/// 'matrices' a list of matrices of different sizes
/// 'same' a list of list of same sizes
fn sizedStruct(
    n: i32,
    lists: []List,
    strings: []String,
    matrices: []Matrix,
    same: []NotASizedStruct,
) !void {
    // The is a special case.
    _ = .{ n, lists, strings, matrices, same };
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator); // Allocates memory to read inputs
    defer arena.deinit(); // Frees the memory at the end of the scope
    const allocator = arena.allocator();
    const lineReader = try LineReader.init(allocator); // The reader used to parse the input

    const n = try lineReader.readValue(i32);
    var i = try std.ArrayList(List).initCapacity(allocator, @intCast(n));
    for (0..@intCast(n)) |_| {
        const size1 = try lineReader.readValue(i32);
        const int_list = try lineReader.readNElements(i32, ' ', @intCast(size1));
        const j = List{
            .size1 = size1,
            .int_list = int_list,
        };
        i.appendAssumeCapacity(j);
    }
    const lists = try i.toOwnedSlice(allocator);
    const strings = try lineReader.readType([]String, &[_]usize{@intCast(n)}, ' ');
    var j = try std.ArrayList(Matrix).initCapacity(allocator, @intCast(2));
    for (0..@intCast(2)) |_| {
        const size3 = try lineReader.readValue(i32);
        const list_list = try lineReader.readType([][]i32, &[_]usize{ @intCast(size3), 2 }, ' ');
        const k = Matrix{
            .size3 = size3,
            .list_list = list_list,
        };
        j.appendAssumeCapacity(k);
    }
    const matrices = try j.toOwnedSlice(allocator);
    const same = try lineReader.readType([]NotASizedStruct, &[_]usize{ @intCast(n), @intCast(n) }, ' ');

    try sizedStruct(
        n,
        lists,
        strings,
        matrices,
        same,
    );
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

    /// Reads all elements on a line, splits them by `delimiter` and parses them
    /// Asserts there is atlease 1 element
    fn readList(self: Self, comptime ReturnType: type, delimiter: u8) ![]ReturnType {
        const line = try self.read();
        defer self.allocator.free(line);
        var values = std.mem.splitScalar(u8, line, delimiter);
        var output: std.ArrayList(ReturnType) = .empty;

        while (values.next()) |v| {
            try output.append(self.allocator, try self.parseType(ReturnType, v));
        }

        return try output.toOwnedSlice(self.allocator);
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

        return try output.toOwnedSlice(self.allocator);
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
            .int, .float => {
                return self.readValue(ReturnType);
            },
            .pointer => {
                const childType = typeInfo.pointer.child;
                if (ReturnType == []const u8) {
                    return self.read();
                }
                switch (@typeInfo(childType)) {
                    .int, .float => {
                        if (shape.len == 0) {
                            return self.readList(childType, delimiter);
                        } else return self.readNElements(
                            childType,
                            delimiter,
                            shape[0],
                        );
                    },
                    .pointer, .@"struct" => {
                        if (ReturnType == []const u8) {
                            return try self.read();
                        }
                        var values = try std.ArrayList(childType).initCapacity(
                            self.allocator,
                            shape[0],
                        );
                        for (0..shape[0]) |_| {
                            values.appendAssumeCapacity(try self.readType(
                                childType,
                                shape[1..],
                                delimiter,
                            ));
                        }
                        return values.toOwnedSlice(self.allocator);
                    },
                    else => return error.UnsupportedType,
                }
            },
            .@"struct" => {
                const s = try self.allocator.create(ReturnType);
                var line: []const u8 = "";
                var currentIndex: usize = 0;
                var subShape = shape;
                inline for (typeInfo.@"struct".fields) |field| {
                    const fieldInfo = @typeInfo(field.type);
                    switch (fieldInfo) {
                        .int, .float, .pointer, .@"struct" => {
                            if (fieldInfo == .int or fieldInfo == .float or
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

    fn deinit(self: Self) void {
        self.allocator.free(self.stdin_buffer);
    }
};
