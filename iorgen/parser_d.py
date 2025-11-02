# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2019-2025 Sacha Delanoue
"""Generate a D parser"""

import textwrap

from iorgen.types import FormatStyle, Input, Type, TypeEnum, Variable
from iorgen.utils import camel_case, pascal_case, IteratorName

INDENTATION = "    "  # https://dlang.org/dstyle.html#whitespace


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for D"""
    # https://dlang.org/dstyle.html#naming_general
    candidate = camel_case(name)
    if candidate in KEYWORDS or candidate in USED_SYMBOLS:
        return candidate + "_"  # https://dlang.org/dstyle.html#naming_keywords
    return candidate


def struct_name(name: str) -> str:
    """Transform a struct name into a valid one for D"""
    # https://dlang.org/dstyle.html#naming_classes
    candidate = pascal_case(name)
    return candidate


def type_str(type_: Type) -> str:
    """Return the D name for a type"""
    if type_.main == TypeEnum.INT:
        return "int"
    if type_.main == TypeEnum.FLOAT:
        return "double"
    if type_.main == TypeEnum.CHAR:
        return "char"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.STRUCT:
        return struct_name(type_.struct_name)
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated
    return f"{type_str(type_.encapsulated)}[]"


def format_specifier(type_: Type, read: bool = True) -> str:
    """Return D print format specifier for a type"""
    if type_.main == TypeEnum.INT:
        return "%d"
    if type_.main == TypeEnum.CHAR:
        return "%c"
    if type_.main == TypeEnum.STR:
        return "%s"
    if type_.main == TypeEnum.FLOAT:
        return "%g" if read else "%.15g"
    assert False
    return "%s"


class ParserD:
    """Create the D code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data
        self.imports = {"std.stdio": {"stdin"}}
        self.iterator = IteratorName([var.name for var in input_data.input])

    def add_import(self, module: str, symbol: str) -> None:
        """Add a new import statement"""
        if module in self.imports:
            self.imports[module].add(symbol)
        else:
            self.imports[module] = {symbol}

    def read_line(self, name: str, type_: Type) -> str:
        """Read a variable in one line of stdin"""
        assert type_.fits_in_one_line(self.input.structs)
        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
            return f'stdin.readf("{format_specifier(type_)}\\n", &{name});'
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated
            self.add_import("std.array", "split")
            self.add_import("std.conv", "to")
            if type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT):
                self.add_import("std.array", "array")
                self.add_import("std.algorithm.iteration", "map")
                cast = type_str(type_.encapsulated)
                return f"{name} = stdin.readln.split.map!(to!{cast}).array;"
            assert type_.encapsulated.main == TypeEnum.CHAR
            self.add_import("std.string", "chop")
            return name + " = stdin.readln.chop.to!(char[]);"
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        return 'stdin.readf("{}\\n", {});'.format(
            " ".join(format_specifier(i.type) for i in struct.fields),
            ", ".join("&" + name + "." + var_name(i.name) for i in struct.fields),
        )

    def read_lines(
        self,
        name: str,
        type_: Type,
        size: str,
        style: FormatStyle = FormatStyle.DEFAULT,
    ) -> list[str]:
        """Read a variable in one line or several lines of stdin"""
        if type_.fits_in_one_line(self.input.structs, style):
            return [self.read_line(name, type_)]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated
            index = self.iterator.new_it()
            lines = [
                f"{name}.length = {size};",
                "for (size_t {0} = 0; {0} < {1}.length; {0}++)".format(index, name),
                "{",
            ]
            lines.extend(
                [
                    INDENTATION + i
                    for i in self.read_lines(
                        f"{name}[{index}]",
                        type_.encapsulated,
                        var_name(type_.encapsulated.size),
                    )
                ]
            )
            self.iterator.pop_it()
            return lines + ["}"]
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        lines = []
        for f_name, f_type, f_size in struct.fields_name_type_size(
            f"{name}.{{}}", var_name
        ):
            lines.extend(self.read_lines(f_name, f_type, f_size))
        return lines

    def read_var(self, var: Variable) -> list[str]:
        """Read a variable from stdin"""
        return [f"{type_str(var.type)} {var_name(var.name)};"] + self.read_lines(
            var_name(var.name), var.type, var_name(var.type.size), var.format_style
        )

    def print_lines(
        self, name: str, type_: Type, style: FormatStyle = FormatStyle.DEFAULT
    ) -> list[str]:
        """Print a D variable"""
        if type_.main in (TypeEnum.INT, TypeEnum.STR, TypeEnum.CHAR):
            self.add_import("std.stdio", "writeln")
            return [f"writeln({name});"]
        if type_.main == TypeEnum.FLOAT:
            self.add_import("std.stdio", "writeln")
            self.add_import("std.format", "format")
            return [f'writeln(format("%.15g", {name}));']
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated
            if (
                type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
                and style != FormatStyle.FORCE_NEWLINES
            ):
                self.add_import("std.array", "join")
                self.add_import("std.algorithm.iteration", "map")
                if type_.encapsulated.main == TypeEnum.INT:
                    self.add_import("std.conv", "to")
                    convert = "to!string"
                else:
                    self.add_import("std.format", "format")
                    convert = '(x) => format("%.15g", x)'
                return [f'writeln(join({name}.map!({convert}), " "));']
            if type_.encapsulated.main == TypeEnum.CHAR:
                return [f"writeln({name});"]
            index = self.iterator.new_it()
            lines = [
                "for (size_t {0} = 0; {0} < {1}.length; {0}++)".format(index, name),
                "{",
            ]
            lines.extend(
                [
                    INDENTATION + i
                    for i in self.print_lines(f"{name}[{index}]", type_.encapsulated)
                ]
            )
            self.iterator.pop_it()
            return lines + ["}"]
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        lines = []
        if type_.fits_in_one_line(self.input.structs, style):
            self.add_import("std.stdio", "writefln")
            lines = [
                'writefln("{}", {});'.format(
                    " ".join(format_specifier(i.type, False) for i in struct.fields),
                    ", ".join(name + "." + var_name(i.name) for i in struct.fields),
                )
            ]
        else:
            for field in struct.fields:
                lines.extend(
                    self.print_lines(f"{name}.{var_name(field.name)}", field.type)
                )
        return lines

    def function(self, reprint: bool) -> list[str]:
        """Return the code of the function to complete by the end user"""
        lines = ["/**", "Params:"]
        lines.extend(
            f"{INDENTATION}{var_name(i.name)} = {i.comment}" for i in self.input.input
        )
        lines.append("*/")
        lines.extend(
            [
                "void {}({})".format(
                    var_name(self.input.name),
                    ", ".join(
                        type_str(i.type) + " " + var_name(i.name)
                        for i in self.input.input
                    ),
                ),
                "{",
            ]
        )
        if reprint:
            for variables in self.input.get_all_vars():
                if len(variables) == 1:
                    var = variables[0]
                    lines.extend(
                        INDENTATION + i
                        for i in self.print_lines(
                            var_name(var.name), var.type, var.format_style
                        )
                    )
                else:
                    self.add_import("std.stdio", "writeln")
                    lines.append(
                        INDENTATION
                        + "writeln("
                        + ', " ", '.join(var_name(i.name) for i in variables)
                        + ");"
                    )
        else:
            lines.extend(
                textwrap.wrap(
                    self.input.output,
                    79,
                    initial_indent=INDENTATION + "// TODO ",
                    subsequent_indent=INDENTATION + "// ",
                )
            )
        return lines + ["}"]

    def content(self, reprint: bool) -> str:
        """Return content of the D file for parsing the input"""
        output = ""
        for struct in self.input.structs:
            output += f"/// {struct.comment}\n"
            output += f"struct {struct_name(struct.name)}\n{{\n"
            for field in struct.fields:
                output += INDENTATION + "{} {}; /// {}\n".format(
                    type_str(field.type), var_name(field.name), field.comment
                )
            output += "}\n\n"
        output += "\n".join(self.function(reprint)) + "\n\n"
        output += "void main()\n{\n"
        for variables in self.input.get_all_vars():
            if len(variables) == 1:
                var = variables[0]
                for line in self.read_var(var):
                    output += INDENTATION + line + "\n"
            else:
                assert all(var.type.main == TypeEnum.INT for var in variables)
                output += (
                    INDENTATION
                    + f"int {', '.join(var_name(i.name) for i in variables)};\n"
                )
                output += (
                    INDENTATION
                    + f'stdin.readf("{" ".join(["%d"] * len(variables))}\\n", '
                    + f"{', '.join('&' + var_name(i.name) for i in variables)});\n"
                )
        args = (var_name(i.name) for i in self.input.input)
        output += "\n{}{}({});\n".format(
            INDENTATION, var_name(self.input.name), ", ".join(args)
        )
        output += "}\n"

        imports = ""
        for module in sorted(self.imports.keys()):
            imports += "import {} : {};\n".format(
                module, ", ".join(sorted(self.imports[module]))
            )
        return f"module {var_name(self.input.name)};\n\n{imports}\n{output}"


def gen_d(input_data: Input, reprint: bool = False) -> str:
    """Generate a D code to parse input"""
    return ParserD(input_data).content(reprint)


# https://dlang.org/spec/lex.html#keywords
KEYWORDS = [
    "abstract",
    "alias",
    "align",
    "asm",
    "assert",
    "auto",
    "body",
    "bool",
    "break",
    "byte",
    "case",
    "cast",
    "catch",
    "cdouble",
    "cent",
    "cfloat",
    "char",
    "class",
    "const",
    "continue",
    "creal",
    "dchar",
    "debug",
    "default",
    "delegate",
    "delete",
    "deprecated",
    "do",
    "double",
    "else",
    "enum",
    "export",
    "extern",
    "false",
    "final",
    "finally",
    "float",
    "for",
    "foreach",
    "foreach_reverse",
    "function",
    "goto",
    "idouble",
    "if",
    "ifloat",
    "immutable",
    "import",
    "in",
    "inout",
    "int",
    "interface",
    "invariant",
    "ireal",
    "is",
    "lazy",
    "long",
    "macro",
    "mixin",
    "module",
    "new",
    "nothrow",
    "null",
    "out",
    "override",
    "package",
    "pragma",
    "private",
    "protected",
    "public",
    "pure",
    "real",
    "ref",
    "return",
    "scope",
    "shared",
    "short",
    "static",
    "struct",
    "super",
    "switch",
    "synchronized",
    "template",
    "this",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typeof",
    "ubyte",
    "ucent",
    "uint",
    "ulong",
    "union",
    "unittest",
    "ushort",
    "version",
    "void",
    "wchar",
    "while",
    "with",
]

USED_SYMBOLS = [
    "array",
    "char",
    "chop",
    "format",
    "int",
    "join",
    "main",
    "map",
    "split",
    "std",
    "stdin",
    "string",
    "to",
    "writefln",
    "writeln",
]
