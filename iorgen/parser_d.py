# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2019 Sacha Delanoue
"""Generate a D parser"""

import textwrap

from typing import List

from iorgen.types import Input, Type, TypeEnum, Variable
from iorgen.utils import camel_case, pascal_case, IteratorName

INDENTATION = "    "  # https://dlang.org/dstyle.html#whitespace

# https://dlang.org/spec/lex.html#keywords
KEYWORDS = [
    "abstract", "alias", "align", "asm", "assert", "auto", "body", "bool",
    "break", "byte", "case", "cast", "catch", "cdouble", "cent", "cfloat",
    "char", "class", "const", "continue", "creal", "dchar", "debug", "default",
    "delegate", "delete", "deprecated", "do", "double", "else", "enum",
    "export", "extern", "false", "final", "finally", "float", "for", "foreach",
    "foreach_reverse", "function", "goto", "idouble", "if", "ifloat",
    "immutable", "import", "in", "inout", "int", "interface", "invariant",
    "ireal", "is", "lazy", "long", "macro", "mixin", "module", "new",
    "nothrow", "null", "out", "override", "package", "pragma", "private",
    "protected", "public", "pure", "real", "ref", "return", "scope", "shared",
    "short", "static", "struct", "super", "switch", "synchronized", "template",
    "this", "throw", "true", "try", "typedef", "typeid", "typeof", "ubyte",
    "ucent", "uint", "ulong", "union", "unittest", "ushort", "version", "void",
    "wchar", "while", "with"
]

USED_SYMBOLS = [
    "array", "char", "chop", "int", "join", "main", "map", "split", "std",
    "stdin", "string", "to", "writefln", "writeln"
]


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
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.CHAR:
        return "char"
    if type_.main == TypeEnum.STRUCT:
        return struct_name(type_.struct_name)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated
        return "{}[]".format(type_str(type_.encapsulated))
    assert False
    return ""


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
        if type_.main == TypeEnum.INT:
            return 'stdin.readf("%d\\n", &{});'.format(name)
        if type_.main == TypeEnum.STR:
            return 'stdin.readf("%s\\n", &{});'.format(name)
        if type_.main == TypeEnum.CHAR:
            return 'stdin.readf("%c\\n", &{});'.format(name)
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated
            self.add_import("std.array", "split")
            self.add_import("std.conv", "to")
            if type_.encapsulated.main == TypeEnum.INT:
                self.add_import("std.array", "array")
                self.add_import("std.algorithm.iteration", "map")
                return name + " = stdin.readln.split.map!(to!int).array;"
            assert type_.encapsulated.main == TypeEnum.CHAR
            self.add_import("std.string", "chop")
            return name + ' = stdin.readln.chop.to!(char[]);'
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        return 'stdin.readf("{}\\n", {});'.format(
            " ".join("%c" if i.type.main == TypeEnum.CHAR else "%d"
                     for i in struct.fields),
            ", ".join("&" + name + "." + var_name(i.name)
                      for i in struct.fields))

    def read_lines(self, name: str, type_: Type, size: str) -> List[str]:
        """Read a variable in one line or several lines of stdin"""
        if type_.fits_in_one_line(self.input.structs):
            return [self.read_line(name, type_)]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated
            index = self.iterator.new_it()
            lines = [
                "{}.length = {};".format(name, size),
                "for (size_t {0} = 0; {0} < {1}.length; {0}++)".format(
                    index, name), "{"
            ]
            lines.extend([
                INDENTATION + i for i in self.read_lines(
                    "{}[{}]".format(name, index), type_.encapsulated,
                    var_name(type_.encapsulated.size))
            ])
            self.iterator.pop_it()
            return lines + ["}"]
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        lines = []
        for f_name, f_type, f_size in struct.fields_name_type_size(
                "{}.{{}}".format(name), var_name):
            lines.extend(self.read_lines(f_name, f_type, f_size))
        return lines

    def read_var(self, var: Variable) -> List[str]:
        """Read a variable from stdin"""
        return ["{} {};".format(type_str(var.type), var_name(var.name))
                ] + self.read_lines(var_name(var.name), var.type,
                                    var_name(var.type.size))

    def print_lines(self, name: str, type_: Type) -> List[str]:
        """Print a D variable"""
        if type_.main in (TypeEnum.INT, TypeEnum.STR, TypeEnum.CHAR):
            self.add_import("std.stdio", "writeln")
            return ["writeln({});".format(name)]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated
            if type_.encapsulated.main == TypeEnum.INT:
                self.add_import("std.array", "join")
                return ['writeln(join({}.map!(to!string), " "));'.format(name)]
            if type_.encapsulated.main == TypeEnum.CHAR:
                return ["writeln({});".format(name)]
            index = self.iterator.new_it()
            lines = [
                "for (size_t {0} = 0; {0} < {1}.length; {0}++)".format(
                    index, name), "{"
            ]
            lines.extend([
                INDENTATION + i for i in self.print_lines(
                    "{}[{}]".format(name, index), type_.encapsulated)
            ])
            self.iterator.pop_it()
            return lines + ["}"]
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        if type_.fits_in_one_line(self.input.structs):
            self.add_import("std.stdio", "writefln")
            return [
                'writefln("{}", {});'.format(
                    " ".join("%c" if i.type.main == TypeEnum.CHAR else "%d"
                             for i in struct.fields),
                    ", ".join(name + "." + var_name(i.name)
                              for i in struct.fields))
            ]
        lines = []
        for field in struct.fields:
            lines.extend(
                self.print_lines("{}.{}".format(name, var_name(field.name)),
                                 field.type))
        return lines

    def function(self, reprint: bool) -> List[str]:
        """Return the code of the function to complete by the end user"""
        lines = ["/**", "Params:"]
        lines.extend(
            "{}{} = {}".format(INDENTATION, var_name(i.name), i.comment)
            for i in self.input.input)
        lines.append("*/")
        lines.extend([
            "void {}({})".format(
                var_name(self.input.name), ", ".join(
                    type_str(i.type) + " " + var_name(i.name)
                    for i in self.input.input)), "{"
        ])
        if reprint:
            for var in self.input.input:
                lines.extend(
                    INDENTATION + i
                    for i in self.print_lines(var_name(var.name), var.type))
        else:
            lines.extend(
                textwrap.wrap(self.input.output,
                              79,
                              initial_indent=INDENTATION + "// TODO ",
                              subsequent_indent=INDENTATION + "// "))
        return lines + ["}"]

    def content(self, reprint: bool) -> str:
        """Return content of the D file for parsing the input"""
        output = ""
        for struct in self.input.structs:
            output += "/// {}\n".format(struct.comment)
            output += "struct {}\n{{\n".format(struct_name(struct.name))
            for field in struct.fields:
                output += INDENTATION + "{} {}; /// {}\n".format(
                    type_str(field.type), var_name(field.name), field.comment)
            output += "}\n\n"
        output += "\n".join(self.function(reprint)) + "\n\n"
        output += "void main()\n{\n"
        for var in self.input.input:
            output += "\n".join(INDENTATION + i
                                for i in self.read_var(var)) + "\n"
        args = (var_name(i.name) for i in self.input.input)
        output += "\n{}{}({});\n".format(INDENTATION,
                                         var_name(self.input.name),
                                         ", ".join(args))
        output += "}\n"

        imports = ""
        for module in sorted(self.imports.keys()):
            imports += "import {} : {};\n".format(
                module, ", ".join(sorted(self.imports[module])))
        return imports + "\n" + output


def gen_d(input_data: Input, reprint: bool = False) -> str:
    """Generate a D code to parse input"""
    return ParserD(input_data).content(reprint)
