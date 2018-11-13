# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a Go parser"""

import textwrap
from typing import List
from iorgen.types import Input, Type, TypeEnum, Variable
from iorgen.utils import pascal_case, camel_case, IteratorName

KEYWORDS = [
    "break", "case", "chan", "const", "continue", "default", "defer", "else",
    "fallthrough", "for", "func", "go", "goto", "if", "import", "interface",
    "map", "package", "range", "return", "select", "struct", "switch", "type",
    "var"
]

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Go"""
    candidate = camel_case(name)
    if candidate in KEYWORDS:
        return candidate + "_"
    if candidate in ("main", "fmt", "make", "len"):
        return candidate + "_"
    return candidate


def struct_name(name: str) -> str:
    """Transform a struct name into a valid one for Go"""
    candidate = pascal_case(name)
    if candidate in KEYWORDS:
        return candidate + "_"
    return candidate


def type_str(type_: Type) -> str:
    """Return the Go name for a type"""
    if type_.main == TypeEnum.INT:
        return "int"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.CHAR:
        return "byte"
    if type_.main == TypeEnum.STRUCT:
        return struct_name(type_.struct_name)
    assert type_.encapsulated
    if type_.main == TypeEnum.LIST:
        return "[]{}".format(type_str(type_.encapsulated))
    assert False
    return ""


class ParserGo():
    """Create the Go code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.iterator = IteratorName([var.name for var in input_data.input])

    def read_line(self, name: str, type_: Type, indent_lvl: int) -> List[str]:
        """Read an entire line and store it into the right place(s)"""
        assert type_.fits_it_one_line(self.input.structs)
        indent = INDENTATION * indent_lvl
        if type_.main in (TypeEnum.INT, TypeEnum.STR):
            return [indent + 'fmt.Scanln(&{})'.format(name)]
        if type_.main == TypeEnum.CHAR:
            return [indent + 'fmt.Scanf("%c\\n", &{})'.format(name)]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return [indent + 'fmt.Scanln(&{})'.format(name)]
            inner_name = self.iterator.new_it()
            lines = [indent + "for {} := range {} {{".format(inner_name, name)]
            lines.append(indent + INDENTATION +
                         'fmt.Scan(&{}[{}])'.format(name, inner_name))
            self.iterator.pop_it()
            return lines + [indent + '}']
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            return [
                indent + "fmt.Scanln({})".format(", ".join(
                    "&{}.{}".format(name, var_name(f.name))
                    for f in struct.fields))
            ]
        assert False
        return []

    def read_lines(self, name: str, type_: Type, indent_lvl: int) -> List[str]:
        """Read one or several lines and store them into the right place(s)"""
        lines = []
        if type_.main == TypeEnum.LIST and indent_lvl != 0:
            lines.append("{}{} = make({}, {})".format(
                INDENTATION * indent_lvl, name, type_str(type_), type_.size))
        if type_.fits_it_one_line(self.input.structs):
            return lines + self.read_line(name, type_, indent_lvl)
        if type_.main == TypeEnum.STRUCT:
            for field in self.input.get_struct(type_.struct_name).fields:
                lines.extend(
                    self.read_lines("{}.{}".format(name, var_name(field.name)),
                                    field.type, indent_lvl))
            return lines
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            inner_name = self.iterator.new_it()
            lines.append("{}for {} := range {} {{".format(
                INDENTATION * indent_lvl, inner_name, name))
            lines.extend(
                self.read_lines("{}[{}]".format(name, inner_name),
                                type_.encapsulated, indent_lvl + 1))
            lines.append(INDENTATION * indent_lvl + "}")
            self.iterator.pop_it()
            return lines
        assert False
        return lines

    def read_var(self, var: Variable) -> List[str]:
        """Read a variable"""
        make = False
        if var.type.main == TypeEnum.LIST:
            assert var.type.encapsulated is not None
            if var.type.encapsulated.main != TypeEnum.CHAR:
                make = True
        lines = []
        if make:
            lines.append("{} := make({}, {})".format(
                var_name(var.name), type_str(var.type),
                var_name(var.type.size)))
        else:
            lines.append("var {} {}".format(
                var_name(var.name), type_str(var.type)))
        lines.extend(self.read_lines(var_name(var.name), var.type, 0))
        return lines

    def call(self, reprint: bool) -> List[str]:
        """Declare and call the function take all inputs in arguments"""
        lines = []
        name = var_name(self.input.name)
        arguments = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            lines.append("// {}: {}".format(arg_name, arg.comment))
            arguments.append("{} {}".format(arg_name, type_str(arg.type)))
        lines.append("func {}({}) {{".format(name, ", ".join(arguments)))
        if reprint:
            for var in self.input.input:
                lines.extend(self.print_lines(var_name(var.name), var.type, 1))
        else:
            lines.extend([
                INDENTATION + i
                for i in textwrap.wrap("/* TODO " + self.input.output +
                                       " */", 79 - len(INDENTATION))
            ])
        return lines + ['}']

    def print_line(self, name: str, type_: Type, indent_lvl: int) -> List[str]:
        """Print the content of a var that holds in one line"""
        assert type_.fits_it_one_line(self.input.structs)
        indent = INDENTATION * indent_lvl
        if type_.main in (TypeEnum.INT, TypeEnum.STR):
            return [indent + "fmt.Println({});".format(name)]
        if type_.main == TypeEnum.CHAR:
            return [indent + 'fmt.Printf("%c\\n", {});'.format(name)]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return [indent + "fmt.Println(string({}));".format(name)]
            index = self.iterator.new_it()
            lines = [
                indent + "for {} := range {} {{".format(index, name),
                indent + INDENTATION + "fmt.Print({}[{}])".format(name, index)
            ]
            lines.extend([
                indent + INDENTATION + "if {} < len({}) - 1 {{".format(
                    index, name), indent + 2 * INDENTATION + 'fmt.Print(" ")',
                indent + INDENTATION + "}"
            ])
            self.iterator.pop_it()
            return lines + [indent + "}", indent + "fmt.Println()"]
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            return [
                indent + 'fmt.Printf("{}\\n", {})'.format(
                    " ".join("%d" if x.type.main == TypeEnum.INT else "%c"
                             for x in struct.fields), ", ".join(
                                 "{}.{}".format(name, var_name(x.name))
                                 for x in struct.fields))
            ]
        assert False
        return []

    def print_lines(self, name: str, type_: Type,
                    indent_lvl: int) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        if type_.fits_it_one_line(self.input.structs):
            return self.print_line(name, type_, indent_lvl)
        if type_.main == TypeEnum.STRUCT:
            lines = []
            for field in self.input.get_struct(type_.struct_name).fields:
                lines.extend(
                    self.print_lines(
                        "{}.{}".format(name, var_name(field.name)), field.type,
                        indent_lvl))
            return lines
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            inner_name = self.iterator.new_it()
            lines = [
                "{}for _, {} := range {} {{".format(INDENTATION * indent_lvl,
                                                    inner_name, name)
            ]
            lines.extend(
                self.print_lines(inner_name, type_.encapsulated,
                                 indent_lvl + 1))
            lines.append(INDENTATION * indent_lvl + "}")
            self.iterator.pop_it()
            return lines
        assert False
        return []

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        output = 'package main\n\nimport "fmt"\n\n'
        for struct in self.input.structs:
            output += "// {}\n".format(struct.comment)
            output += "type {} struct {{\n".format(struct_name(struct.name))
            for field in struct.fields:
                output += INDENTATION + "{} {} // {}\n".format(
                    var_name(field.name), type_str(field.type), field.comment)
            output += "}\n\n"
        output += "\n".join(self.call(reprint)) + "\n\n"
        output += "func main() {\n"
        for var in self.input.input:
            for line in self.read_var(var):
                output += INDENTATION + line + "\n"
        output += INDENTATION + "{}({});\n".format(
            var_name(self.input.name), ", ".join(
                [var_name(i.name) for i in self.input.input]))
        output += "}\n"
        return output


def gen_go(input_data: Input, reprint: bool = False) -> str:
    """Generate a Go code to parse input"""
    return ParserGo(input_data).content(reprint)
