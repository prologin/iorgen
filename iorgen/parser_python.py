# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a Python 3 parser"""

import textwrap
from keyword import iskeyword
from typing import List

from iorgen.types import Input, Type, TypeEnum, Variable
from iorgen.utils import pascal_case, snake_case


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Python"""
    candidate = snake_case(name)
    return candidate + '_' if iskeyword(candidate) else candidate


def struct_name(name: str) -> str:
    """Transform a struct name into a valid one for Python"""
    candidate = pascal_case(name)
    return candidate + '_' if iskeyword(candidate) else candidate


def read_line(type_: Type, input_data: Input) -> str:
    """Generate the Python code to read a line of given type"""
    assert type_.fits_it_one_line(input_data.structs)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return "list(input())"
        if type_.encapsulated.main == TypeEnum.INT:
            return "list(map(int, input().split()))"
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        begin = "dict(zip([{}], ".format(", ".join(
            '"{}"'.format(i[0]) for i in struct.fields))
        if all(i[1].main == TypeEnum.INT for i in struct.fields):
            return begin + "map(int, input().split())))"
        if all(i[1].main == TypeEnum.CHAR for i in struct.fields):
            return begin + "input().split()))"
        assert False, "Not implemented"
    return {
        TypeEnum.INT: "int(input())",
        TypeEnum.CHAR: "input()[0]",
        TypeEnum.STR: "input()"
    }[type_.main]


def read_lines(type_: Type, input_data: Input) -> str:
    """Generate the Python code to read the lines for a given type"""
    if type_.fits_it_one_line(input_data.structs):
        return read_line(type_, input_data)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        return "[{} for _ in range({})]".format(
            read_lines(type_.encapsulated, input_data), var_name(type_.size))
    if type_.main == TypeEnum.STRUCT:
        return "{{{}}}".format(", ".join(
            '"{}": {}'.format(field[0], read_lines(field[1], input_data))
            for field in input_data.get_struct(type_.struct_name).fields))
    assert False
    return ""


def print_line(name: str, type_: Type, input_data: Input) -> str:
    """Print the content of a var in one line"""
    assert type_.fits_it_one_line(input_data.structs)
    if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
        return "print({})".format(name)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return "print(''.join({}))".format(name)
        if type_.encapsulated.main == TypeEnum.INT:
            return "print(' '.join(map(str, {})))".format(name)
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        return "print({})".format(", ".join(
            '{}["{}"]'.format(name, i[0]) for i in struct.fields))
    assert False
    return ""


class ParserPython():
    """Create the Python code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data
        self.main = []  # type: List[str]
        self.method = []  # type: List[str]
        self.indentation = 4

    def read_var(self, var: Variable) -> None:
        """Read a variable"""
        self.main.append("{} = {}".format(
            var_name(var.name), read_lines(var.type, self.input)))

    def call(self, reprint: bool) -> None:
        """Declare and call the function take all inputs in arguments"""
        name = var_name(self.input.name)
        self.method.append("def {}({}):".format(
            name, ", ".join(var_name(i.name) for i in self.input.input)))
        self.method.append(" " * self.indentation + '"""')
        self.method.extend([
            "{}:param {}: {}".format(" " * self.indentation,
                                     var_name(arg.name), arg.comment)
            for arg in self.input.input
        ])
        self.method.append(" " * self.indentation + '"""')
        if reprint:
            for var in self.input.input:
                self.method.extend(
                    self.print_lines(var_name(var.name), var.type, 1))
        else:
            self.method.extend([
                " " * self.indentation + "#" + i
                for i in textwrap.wrap(" TODO " + self.input.output, 78 -
                                       self.indentation)
            ])
            self.method.append(" " * self.indentation + "pass")
        self.main.append("{}({})".format(
            name, ", ".join([var_name(i.name) for i in self.input.input])))

    def print_lines(self, name: str, type_: Type,
                    indent_lvl: int = 0) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        indent = " " * self.indentation * indent_lvl
        if type_.fits_it_one_line(self.input.structs):
            return [indent + print_line(name, type_, self.input)]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            inner = name + "It"
            return [
                indent + "for {} in {}:".format(inner, name)
            ] + self.print_lines(inner, type_.encapsulated, indent_lvl + 1)
        if type_.main == TypeEnum.STRUCT:
            lines = []
            for i in self.input.get_struct(type_.struct_name).fields:
                lines.extend(
                    self.print_lines('{}["{}"]'.format(name, i[0]), i[1],
                                     indent_lvl))
            return lines
        assert False
        return []

    def content(self) -> str:
        """Return the parser content"""
        output = ""
        for line in self.method:
            output += line + "\n"
        if self.method:
            output += "\n"
        output += "if __name__ == '__main__':\n"
        for line in self.main:
            output += ' ' * self.indentation + line + "\n"
        return output


def gen_python(input_data: Input, reprint: bool = False) -> str:
    """Generate a Python code to parse input"""
    parser = ParserPython(input_data)
    for var in input_data.input:
        parser.read_var(var)
    parser.call(reprint)
    return parser.content()
