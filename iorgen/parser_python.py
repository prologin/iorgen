# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
"""Generate a Python 3 parser"""

import textwrap
from keyword import iskeyword
from typing import List

from iorgen.types import Input, Type, TypeEnum, Variable
from iorgen.utils import pascal_case, snake_case

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Python"""
    candidate = snake_case(name)
    return (
        candidate + "_"
        if iskeyword(candidate) or candidate == "dataclass"
        else candidate
    )


def class_name(name: str) -> str:
    """Transform a class name into a valid one for Python"""
    candidate = pascal_case(name)
    return candidate + "_" if candidate == "List" else candidate


def type_str(type_: Type) -> str:
    """Return a description for a type"""
    if type_.main == TypeEnum.INT:
        return "int"
    if type_.main == TypeEnum.STR:
        return "str"
    if type_.main == TypeEnum.CHAR:
        return "str"
    if type_.main == TypeEnum.STRUCT:
        return class_name(type_.struct_name)
    assert type_.encapsulated
    assert type_.main == TypeEnum.LIST
    # we should import List from typing when needed
    return f"List[{type_str(type_.encapsulated)}]"


def decl_classes(input_data: Input) -> List[str]:
    """Return declarations of structs as data classes"""
    lines = []
    if input_data.structs:
        lines = ["from dataclasses import dataclass", "from typing import List", "", ""]
    for struct in input_data.structs:
        lines.append("@dataclass")
        lines.append(f"class {class_name(struct.name)}:")
        lines.append(f'{INDENTATION}"""{struct.comment}"""')
        lines.append("")
        for field in struct.fields:
            lines.append(
                f"{INDENTATION}{var_name(field.name)}: "
                + f"{type_str(field.type)}  # {field.comment}"
            )
        lines.append("")
        lines.append("")
    return lines


def read_line(type_: Type, input_data: Input) -> str:
    """Generate the Python code to read a line of given type"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return "list(input())"
        assert type_.encapsulated.main == TypeEnum.INT
        return "list(map(int, input().split()))"
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        if all(i.type.main == TypeEnum.INT for i in struct.fields):
            return f"{class_name(struct.name)}(*map(int, input().split()))"
        if all(i.type.main == TypeEnum.CHAR for i in struct.fields):
            return f"{class_name(struct.name)}(*input().split())"
        return "{}(*map({}, ({}), input().split()))".format(
            class_name(struct.name),
            "lambda x, y: int(y) if x else y",
            ", ".join(
                "1" if i.type.main == TypeEnum.INT else "0" for i in struct.fields
            ),
        )
    return {
        TypeEnum.INT: "int(input())",
        TypeEnum.CHAR: "input()[0]",
        TypeEnum.STR: "input()",
    }[type_.main]


def read_lines(type_: Type, size: str, input_data: Input) -> List[str]:
    """Generate the Python code to read the lines for a given type"""
    if type_.fits_in_one_line(input_data.structs):
        return [read_line(type_, input_data)]
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        lines = read_lines(
            type_.encapsulated, var_name(type_.encapsulated.size), input_data
        )
        if len(lines) == 1:
            candidate = "[{} for _ in range({})]".format(lines[0], size)
            if len(candidate) <= 75:
                return [candidate]
        if len(lines[-1]) < 5:
            lines[-1] += " for _ in range({})]".format(size)
        else:
            lines.append("for _ in range({})".format(size))
            lines.append("]")
        if len(lines[0]) < 5:
            lines[0] = "[" + lines[0]
        else:
            lines = ["["] + [INDENTATION + i for i in lines]
        return lines
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    if struct.is_sized_struct():
        inner = "i"
        lines = read_lines(struct.fields[1].type, inner, input_data)
        return (
            [f"(lambda {inner}: {class_name(struct.name)}(", INDENTATION + f"{inner},"]
            + [INDENTATION + i for i in lines]
            + ["))(int(input()))"]
        )
    fields = []
    for field in struct.fields:
        lines = read_lines(field.type, var_name(field.type.size), input_data)
        lines[-1] += ","
        fields.extend([INDENTATION + i for i in lines])
    return [f"{class_name(struct.name)}("] + fields + [")"]


def print_line(name: str, type_: Type, input_data: Input) -> str:
    """Print the content of a var in one line"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
        return "print({})".format(name)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return "print(''.join({}))".format(name)
        assert type_.encapsulated.main == TypeEnum.INT
        return "print(' '.join(map(str, {})))".format(name)
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return "print({})".format(
        ", ".join(f"{name}.{var_name(field.name)}" for field in struct.fields)
    )


class ParserPython:
    """Create the Python code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data
        self.main = []  # type: List[str]
        self.method = []  # type: List[str]

    def read_var(self, var: Variable) -> List[str]:
        """Read a variable"""
        lines = read_lines(var.type, var_name(var.type.size), self.input)
        lines[0] = "{} = {}".format(var_name(var.name), lines[0])
        return lines

    def call(self, reprint: bool) -> None:
        """Declare and call the function take all inputs in arguments"""
        name = var_name(self.input.name)
        self.method.append(
            "def {}({}):".format(
                name, ", ".join(var_name(i.name) for i in self.input.input)
            )
        )
        self.method.append(INDENTATION + '"""')
        for arg in self.input.input:
            self.method.append(
                "{}:param {}: {}".format(INDENTATION, var_name(arg.name), arg.comment)
            )
            self.method.append(
                "{}:type {}: {}".format(
                    INDENTATION, var_name(arg.name), type_str(arg.type)
                )
            )
        self.method.append(INDENTATION + '"""')
        if reprint:
            for var in self.input.input:
                self.method.extend(self.print_lines(var_name(var.name), var.type, 1))
        else:
            self.method.extend(
                textwrap.wrap(
                    self.input.output,
                    79,
                    initial_indent=INDENTATION + "# " + "TODO ",
                    subsequent_indent=INDENTATION + "# ",
                )
            )
            self.method.append(INDENTATION + "pass")
        self.main.append(
            "{}({})".format(
                name, ", ".join([var_name(i.name) for i in self.input.input])
            )
        )

    def print_lines(self, name: str, type_: Type, indent_lvl: int = 0) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        indent = INDENTATION * indent_lvl
        if type_.fits_in_one_line(self.input.structs):
            return [indent + print_line(name, type_, self.input)]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            inner = "iT" + str(abs(hash(name)))  # unique name
            return [indent + "for {} in {}:".format(inner, name)] + self.print_lines(
                inner, type_.encapsulated, indent_lvl + 1
            )
        assert type_.main == TypeEnum.STRUCT
        lines = []
        for i in self.input.get_struct(type_.struct_name).fields:
            lines.extend(
                self.print_lines(f"{name}.{var_name(i.name)}", i.type, indent_lvl)
            )
        return lines

    def content(self) -> str:
        """Return the parser content"""
        output = ""
        for line in decl_classes(self.input):
            output += line + "\n"
        for line in self.method:
            output += line + "\n"
        if self.method:
            output += "\n"
        output += "\nif __name__ == '__main__':\n"
        for line in self.main:
            output += INDENTATION + line + "\n"
        return output


def gen_python(input_data: Input, reprint: bool = False) -> str:
    """Generate a Python code to parse input"""
    parser = ParserPython(input_data)
    for var in input_data.input:
        parser.main.extend(parser.read_var(var))
    parser.call(reprint)
    return parser.content()
