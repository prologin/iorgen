# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2024 Kamil Leys and Lebaube Gaspard (gaskam.com)
"""Generate a Zig parser"""

import textwrap
from typing import List, Set
from iorgen.types import FormatStyle, Input, Type, TypeEnum, Variable
from iorgen.utils import pascal_case, camel_case, snake_case, IteratorName


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Zig"""
    candidate = snake_case(name)
    if is_keyword(candidate):
        return f"{candidate}_"
    return candidate


def struct_name(name: str) -> str:
    """Transform a struct name into a valid one for Zig"""
    candidate = pascal_case(name)
    if is_keyword(candidate):
        return f"{candidate}_"
    return candidate


# Template used to simplify basic io tasks
TEMPLATE_IO = {
    "start": """
/// Original at: https://github.com/gaskam-com/zig-utils
const LineReader = struct {
    allocator: std.mem.Allocator,
    reader: std.fs.File.Reader,
    const Self = @This();

    fn init(allocator: std.mem.Allocator) Self {
        return .{ .allocator = allocator, .reader = std.io.getStdIn().reader() };
    }""",
    "read": """

    /// Reads a line and removes the newline characters(\\n, and \\r\\n for windows)
    fn read(self: Self) ![]const u8 {
        var buffer = std.ArrayList(u8).init(self.allocator);
        errdefer buffer.deinit();
        try self.reader.streamUntilDelimiter(buffer.writer(), '\\n', null);
        if (builtin.target.os.tag == .windows and buffer.getLastOrNull() == '\\r') _ = buffer.pop();
        return buffer.toOwnedSlice();
    }""",
    "parseType": """

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
    }""",
    "readValue": """

    /// Parses one line to a value(only supports int or float types)
    /// Asserts there is a numeric value
    fn readValue(self: Self, comptime ReturnType: type) !ReturnType {
        const value = try read(self);
        defer self.allocator.free(value);
        return self.parseType(ReturnType, value);
    }""",
    "readList": """

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
    }""",
    "readNElements": """

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
    }""",
    "readType": """

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
    }""",
    "end": "\n};",
}

TEMPLATE_DEPENDENTS = {
    "read": [],
    "parseType": [],
    "readValue": ["read", "parseType"],
    "readList": ["read", "parseType"],
    "readNElements": ["read", "parseType"],
    "readType": ["read", "parseType", "readValue", "readList", "readNElements"],
}

# Template used to print values the appropriate way for CI/CD tests
TEMPLATE_PRINT = """
fn writeFlattened(allocator: std.mem.Allocator, writer: std.fs.File.Writer, values: anytype) !void {
    const typeInfo = @typeInfo(@TypeOf(values));
    const writeFloat = struct {
        fn call(subWriter: @TypeOf(writer), value: f64) !void {
            if (value == 0) {
                try subWriter.print("{d}", .{0});
            }else if (std.math.approxEqAbs(f64, value, 0, 1e-4)) {
                const log =  - std.math.floor(std.math.log10(@abs(value)));
                if (log < 10) {
                    try subWriter.print("{d}e-0{d}", .{value * std.math.pow(f64, @as(f64, 10), log), @as(i32, @intFromFloat(log))});
                } else {
                    try subWriter.print("{d}e-{d}", .{value * std.math.pow(f64, @as(f64, 10), log), @as(i32, @intFromFloat(log))});
                }
            } else {
                try subWriter.print("{d}", .{value});
            }
        }
    }.call;
    switch (typeInfo) {
        .Int => try writer.print("{d}\\n", .{values}),
        .Float => {
            try writeFloat(writer, values);
            try writer.print("\\n", .{});
        },
        .Pointer => {
            if (@TypeOf(values) == []const u8) {
                try writer.print("{s}\\n", .{values});
            } else {
                const childTypeInfo = @typeInfo(typeInfo.Pointer.child);
                if (childTypeInfo == .Int or childTypeInfo == .Float) {
                    for (0..values.len) |i| {
                        if (childTypeInfo == .Float) try writeFloat(writer, values[i]) else try writer.print("{d}", .{values[i]});
                        if (i != values.len - 1) try writer.print(" ", .{});
                    }
                    try writer.print("\\n", .{});
                } else {
                    for (0..values.len) |i| {
                        try writeFlattened(allocator, writer, values[i]);
                    }
                }
            }
        },
        .Struct => {
            const fields = typeInfo.Struct.fields;
            var lastFieldInline = false;
            inline for (fields) |field| {
                const fieldTypeInfo = @typeInfo(field.type);
                switch (fieldTypeInfo) {
                    .Int => {
                        if (lastFieldInline) {
                            try writer.print(" {d}", .{@field(values, field.name)});
                        } else {
                            lastFieldInline = true;
                            try writer.print("{d}", .{@field(values, field.name)});
                        }
                    },
                    .Float => {
                        if (lastFieldInline) {
                            try writer.print(" ", .{});
                        } else {
                            lastFieldInline = true;
                        }
                        try writeFloat(writer, @field(values, field.name));
                    },
                    .Pointer, .Struct => {
                        if (field.type == []const u8 and @field(values, field.name).len == 1) {
                            if (lastFieldInline) {
                                try writer.print(" {s}", .{@field(values, field.name)});
                            } else {
                                lastFieldInline = true;
                                try writer.print("{s}", .{@field(values, field.name)});
                            }
                        } else {
                            if (lastFieldInline) {
                                try writer.print("\\n", .{});
                                lastFieldInline = false;
                            }
                            if (field.type == []const u8) {
                                try writer.print("{s}\\n", .{@field(values, field.name)});
                            } else try writeFlattened(allocator, writer, @field(values, field.name));
                        }
                    },
                    else => error.UnsupportedType,
                }
            }
            if (lastFieldInline) try writer.print("\\n", .{});
        },
        else => error.UnsupportedType,
    }
}"""

# Template used to initialize the main function
TEMPLATE_MAIN = """pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator); // Allocates memory to read inputs
    defer arena.deinit(); // Frees the memory at the end of the scope
    const allocator = arena.allocator();
    const lineReader = LineReader.init(allocator); // The reader used to parse the input
"""


class ParserZig:
    """Create the Zig code to parse an input"""

    # Must be > 0
    indentation = 4

    def __init__(self, input_data: Input, reprint: bool) -> None:
        self.input = input_data
        self.allocator = False  # type: bool
        self.templates = set()  # type: Set[str]
        self.main = []  # type: List[str]
        self.reprint = reprint
        self.iterator = IteratorName([var.name for var in input_data.input])

    def type_str(self, type_: Type, is_list_item: bool = False) -> str:
        """Return the Zig name for a type"""
        # pylint: disable=too-many-return-statements
        if type_.main == TypeEnum.INT:
            return "i32"
        if type_.main == TypeEnum.FLOAT:
            return "f64"
        if type_.main == TypeEnum.CHAR:
            if is_list_item:
                return "u8"
            return "[]const u8"
        if type_.main == TypeEnum.STR:
            return "[]const u8"
        if type_.main == TypeEnum.STRUCT:
            return pascal_case(type_.struct_name)
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated
            type_name = self.type_str(type_.encapsulated, True)
            return f"[]{'const ' if type_.encapsulated.main == TypeEnum.CHAR else ''}{type_name}"
        raise Exception

    def type_format(self, type_: Type) -> str:
        """Return the Zig format for a type"""
        if type_.main == TypeEnum.INT:
            return "d"
        if type_.main == TypeEnum.FLOAT:
            return "d"
        if type_.main == TypeEnum.CHAR:
            return "c"
        if type_.main == TypeEnum.STR:
            return "s"
        if type_.main == TypeEnum.STRUCT:
            return "any"
        if type_.main == TypeEnum.LIST:
            return "any"
        raise Exception

    def get_sizes(self, type_: Type, is_parent_list: bool = False) -> str:
        """Return the sizes of the subElements of a type"""
        try:
            size = str(int(type_.size))
        except ValueError:
            size = f"@intCast({var_name(type_.size)})"

        if type_.main == TypeEnum.STRUCT:
            fields = self.get_struct_fields(type_.struct_name)
            result = []
            for field in fields:
                sizes = self.get_sizes(field.type)
                if sizes != "":
                    result.append(sizes)
            return ", ".join(result)
        if type_.main == TypeEnum.CHAR and not is_parent_list:
            return "1"
        if not type_.encapsulated:
            return ""

        sizes = self.get_sizes(type_.encapsulated, True)
        if sizes != "":
            return f"{size}, {sizes}"
        return f"{size}"

    def is_type_global(self, name: str) -> bool:
        """Check if a type is global"""
        try:
            self.input.get_var(name)
            return True
        except StopIteration:
            return False

    def is_simple_type(self, type_: Type, nested: bool = False) -> bool:
        """Check if a type is simple"""
        if type_.main == TypeEnum.STRUCT:
            for field in self.get_struct_fields(type_.struct_name):
                if not self.is_simple_type(field.type, True):
                    return False
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.size.isdecimal() or not nested:
                return self.is_simple_type(type_.encapsulated, True)
            return self.is_type_global(type_.size) and self.is_simple_type(
                type_.encapsulated, True
            )
        return True

    def read_lines(self, name: str, type_: Type, depth: int = 0) -> None:
        """Read one or several lines and store them into the right place(s)"""

        sizes = self.get_sizes(type_)
        if sizes.find(",") != -1:
            sizes = " " + sizes + " "
        argument = f"{self.type_str(type_)}, &[_]usize{{{sizes}}}, ' '"
        indentation = " " * self.indentation * depth

        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT):
            self.main.append(
                f"{indentation}const {name}"
                + f" = try lineReader.readValue({self.type_str(type_)});"
            )
            self.templates.add("readValue")
        elif type_.main in (TypeEnum.CHAR, TypeEnum.STR):
            self.main.append(f"{indentation}const {name}" + " = try lineReader.read();")
            self.templates.add("read")
        elif (
            type_.main == TypeEnum.LIST
            and type_.encapsulated.main  # type: ignore[union-attr]
            in (
                TypeEnum.INT,
                TypeEnum.FLOAT,
            )
        ):
            assert type_.encapsulated is not None
            self.main.append(
                f"{indentation}const {name} = try lineReader"
                + f".readNElements({self.type_str(type_.encapsulated)}, ' ', {sizes});"
            )
            self.templates.add("readList")
        else:
            self.main.append(
                f"{indentation}const {var_name(name)}"
                + f" = try lineReader.readType({argument});"
            )
            self.templates.add("readType")

    def get_struct_fields(self, name: str) -> List[Variable]:
        """Read a struct to extract the field names"""
        for struct in self.input.structs:
            if struct.name == name:
                return struct.fields
        return []

    def read_complex(self, name: str, type_: Type, depth: int = 0) -> None:
        """Read a list of unknown size(using allocators)"""
        if self.is_simple_type(type_):
            self.read_lines(var_name(name), type_, depth)
        else:
            indentation = " " * self.indentation * depth
            if type_.main == TypeEnum.LIST:
                assert type_.encapsulated is not None
                list_name = self.iterator.new_it()
                inner_name = self.iterator.new_it()
                self.main.extend(
                    (
                        indentation
                        + f"var {var_name(list_name)}"
                        + f" = try std.ArrayList({self.type_str(type_.encapsulated)})"
                        + f".initCapacity(allocator, @intCast({type_.size}));",
                        indentation + "for (0..@intCast({})) |_| {{".format(type_.size),
                    )
                )

                self.read_complex(inner_name, type_.encapsulated, depth + 1)

                self.main.extend(
                    (
                        indentation
                        + " " * self.indentation
                        + f"{var_name(list_name)}.appendAssumeCapacity({inner_name});",
                        indentation + "}",
                        indentation
                        + f"const {var_name(name)} = try {var_name(list_name)}.toOwnedSlice();",
                    )
                )
                self.iterator.pop_it()
                # self.iterator.pop_it() # This stays global so we cannot pop it
            else:
                assert type_.main == TypeEnum.STRUCT
                fields: List[Variable] = self.get_struct_fields(type_.struct_name)
                for field in fields:
                    self.read_complex(field.name, field.type, depth)

                values = [
                    (
                        f"{indentation + ' ' * self.indentation}.{var_name(field.name)}"
                        + f" = {var_name(field.name)},"
                    )
                    for field in fields
                ]

                self.main.extend(
                    (
                        indentation
                        + f"const {var_name(name)} = {struct_name(type_.struct_name)}{{",
                        *values,
                        indentation + "};",
                    )
                )

    def read_var(self, var: Variable, no_endline: bool, iterator_name: str) -> None:
        """Read a variable"""
        if var.format_style == FormatStyle.FORCE_NEWLINES:
            assert var.type.encapsulated is not None
            list_name = self.iterator.new_it()
            inner_name = self.iterator.new_it()
            self.main.extend(
                (
                    "var {} = try std.ArrayList({}).initCapacity(allocator, @intCast({}));".format(
                        var_name(list_name),
                        self.type_str(var.type.encapsulated),
                        var.type.size,
                    ),
                    "for (0..@intCast({})) |_| {{".format(var.type.size),
                )
            )
            self.read_lines(inner_name, var.type.encapsulated, depth=1)
            self.main.extend(
                (
                    " " * self.indentation
                    + f"{var_name(list_name)}.appendAssumeCapacity({inner_name});",
                    "}",
                    f"const {var_name(var.name)} = try {var_name(list_name)}.toOwnedSlice();",
                )
            )
            self.iterator.pop_it()
        elif no_endline:
            self.main.append(
                f"const {var_name(var.name)} = try lineReader.parseType("
                + f"{self.type_str(var.type)}, {iterator_name}.next().?);"
            )
            self.templates.add("parseType")
        elif self.is_simple_type(var.type):
            self.read_lines(
                var_name(var.name),
                var.type,
            )
        else:
            self.read_complex(var.name, var.type)

    def gen_output_func(self) -> str:
        """Generates the function part of the content"""
        output = ""
        for var in self.input.input:
            output += "/// '{}' {}\n".format(var_name(var.name), var.comment)

        if len(self.input.input) > 3:
            output += f"fn {camel_case(self.input.name)}(\n"
            for var in self.input.input:
                output += (
                    f"{self.indentation * ' '}{var_name(var.name)}: "
                    f"{self.type_str(var.type)},\n"
                )

            output += ") !void {\n"

        else:
            content = ""
            content += ", ".join(
                [
                    var_name(var.name) + ": " + self.type_str(var.type)
                    for var in self.input.input
                ]
            )
            output += f"fn {camel_case(self.input.name)}({content}) !void {{\n"

        wrapped = textwrap.fill(
            self.input.output,
            width=87 - self.indentation,
            initial_indent=f"{' ' * self.indentation}// ",
            subsequent_indent=f"{' ' * self.indentation}// ",
        )

        if self.reprint:
            output += f"{self.indentation * ' '}const stdout = std.io.getStdOut().writer();\n\n"
            output += f"{self.indentation * ' '}const allocator = std.heap.page_allocator;\n\n"

            no_endline_stack = []

            for var in self.input.input:
                if var.format_style == FormatStyle.NO_ENDLINE:
                    no_endline_stack.append(var_name(var.name))
                elif var.format_style == FormatStyle.FORCE_NEWLINES:
                    tmp_var_name = self.iterator.new_it()
                    output += "".join(
                        [
                            self.indentation * " ",
                            f"for ({var_name(var.name)}) |{tmp_var_name}| {{\n",
                            self.indentation * 2 * " ",
                            f"try writeFlattened(allocator, stdout, {tmp_var_name});\n",
                            f"{self.indentation * ' '}}}\n",
                        ]
                    )
                    self.iterator.pop_it()
                else:
                    output += f"{self.indentation * ' '}try writeFlattened(allocator, stdout, "
                    if len(no_endline_stack) > 0:
                        output += (
                            ".{"
                            + ", ".join(no_endline_stack)
                            + f", {var_name(var.name)}"
                            + "}"
                        )
                        no_endline_stack = []
                    else:
                        output += var_name(var.name)
                    output += ");\n"
        else:
            content = ""
            content += ", ".join([var_name(var.name) for var in self.input.input])
            output += f"{wrapped}\n{self.indentation * ' '}_ = .{{ {content} }};\n"

        output += "}\n\n"
        return output

    def content(self) -> str:
        """Return the parser content"""
        # pylint: disable=too-many-branches

        # The standard-library
        output = 'const std = @import("std");\n'
        # Used to detect windows and perform micro-optimization
        output += 'const builtin = @import("builtin");\n\n'

        for struct in self.input.structs:
            output += "/// {}\n".format(struct.comment)
            output += "const {} = struct {{\n".format(struct_name(struct.name))
            for field in struct.fields:
                output += " " * self.indentation + f"/// {field.comment}\n"
                output += " " * self.indentation + "{}: {},\n".format(
                    var_name(field.name), self.type_str(field.type)
                )
            output += "};\n\n"

        output += self.gen_output_func()

        output += (
            "\n".join(
                f"{self.indentation * (len(line) - len(line.lstrip())) // 4 * ' '}{line.lstrip()}"
                for line in TEMPLATE_MAIN.split("\n")
            )
            + "\n"
        )

        for line in self.main:
            output += f"{self.indentation * ' '}{line}\n"

        output += "\n"

        if len(self.input.input) > 3:
            output += f"{self.indentation * ' '}try {camel_case(self.input.name)}(\n"
            for var in self.input.input:
                output += f"{self.indentation * 2 * ' '}{var_name(var.name)},\n"

            output += f"{self.indentation * ' '});\n"

        else:
            content = ", ".join([var_name(var.name) for var in self.input.input])
            output += (
                self.indentation * " "
                + f"try {camel_case(self.input.name)}({content});\n"
            )

        output += "}\n"

        io_dependencies = self.templates.copy()

        for template in self.templates:
            for dependency in TEMPLATE_DEPENDENTS[template]:
                io_dependencies.add(dependency)

        # We want to keep the order of the templates
        output += TEMPLATE_IO["start"]
        for name, template in TEMPLATE_IO.items():
            if name in io_dependencies:
                output += template

        output += TEMPLATE_IO["end"]

        if self.reprint:
            output += "\n" + TEMPLATE_PRINT

        return output + "\n"


def gen_zig(input_data: Input, reprint: bool = False) -> str:
    """Generate a Zig code to parse input"""
    parser = ParserZig(input_data, reprint)
    no_endline = False
    iterator_name = ""
    for var in input_data.input:
        if var.format_style == FormatStyle.NO_ENDLINE:
            if no_endline is False:
                iterator_name = parser.iterator.new_it()
                parser.main.append(
                    f"var {iterator_name} = std.mem.splitScalar(u8, try lineReader.read(), ' ');"
                )
            no_endline = True
        parser.read_var(var, no_endline, iterator_name)
        if var.format_style != FormatStyle.NO_ENDLINE:
            no_endline = False
    return parser.content()


# Language keywords

# Doesn't include types starting with u or i (unsigned and unsigned ints),
# uses is_type to check for these
# See: https://ziglang.org/documentation/master/#Primitive-Types for full reference
TYPES = [
    "isize",
    "usize",
    "c_char",
    "c_short",
    "c_ushort",
    "c_int",
    "c_uint",
    "c_long",
    "c_ulong",
    "c_longlong",
    "c_ulonglong",
    "c_longdouble",
    "f16",
    "f32",
    "f64",
    "f80",
    "f128",
    "bool",
    "anyopaque",
    "void",
    "noreturn",
    "type",
    "anyerror",
    "comptime_int",
    "comptime_float",
]


def is_type(value: str) -> bool:
    """Return wether the provided input is a type"""
    if (
        len(value) > 0
        and (value[0] == "u" or value[0] == "i")
        and value[1:].isnumeric()
    ):
        return True
    return value in TYPES


# See: https://ziglang.org/documentation/master/#Keyword-Reference
KEYWORDS = [
    "addrspace",
    "align",
    "allowzero",
    "and",
    "anyframe",
    "anytype",
    "asm",
    "async",
    "await",
    "break",
    "callconv",
    "catch",
    "comptime",
    "const",
    "continue",
    "defer",
    "else",
    "enum",
    "errdefer",
    "error",
    "export",
    "extern",
    "fn",
    "for",
    "if",
    "inline",
    "linksection",
    "noalias",
    "noinline",
    "nosuspend",
    "opaque",
    "or",
    "orelse",
    "packed",
    "pub",
    "resume",
    "return",
    "struct",
    "suspend",
    "switch",
    "test",
    "threadlocal",
    "try",
    "union",
    "unreachable",
    "usingnamespace",
    "var",
    "volatile",
    "while",
    "allocator",  # Custom keywords
    "builtin",
    "lineReader",
    "LineReader",
    "main",
    "writeFlattened",
    "std",
    "stdout",
]


def is_keyword(value: str) -> bool:
    """Returns a boolean indicating if the input is a reserved keyword"""
    return value in KEYWORDS or is_type(value)
