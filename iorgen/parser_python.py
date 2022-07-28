# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
"""Generate a Python 3 parser"""

import textwrap
from keyword import iskeyword
from typing import List

from iorgen.types import FormatStyle, Input, Type, TypeEnum
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
    if type_.main == TypeEnum.FLOAT:
        return "float"
    if type_.main == TypeEnum.STR:
        return "str"
    if type_.main == TypeEnum.CHAR:
        return "str"
    if type_.main == TypeEnum.STRUCT:
        return class_name(type_.struct_name)
    assert type_.encapsulated
    assert type_.main == TypeEnum.LIST
    return f"List[{type_str(type_.encapsulated)}]"


def decl_classes(input_data: Input) -> List[str]:
    """Return declarations of structs as data classes"""
    lines = []
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


def decl_imports(input_data: Input) -> List[str]:
    """Return import declarations"""
    lines = []
    if input_data.structs:
        lines.append("from dataclasses import dataclass")
    has_list = False
    for struct in input_data.structs:
        for field in struct.fields:
            if field.type.main == TypeEnum.LIST:
                has_list = True
    for var in input_data.input:
        if var.type.main == TypeEnum.LIST:
            has_list = True
    if has_list:
        lines.append("from typing import List")
    if lines:
        lines.extend(["", ""])
    return lines


def wrap_line(begin: str, end: str, args: List[str], indent_lvl: int = 0) -> List[str]:
    """Wrap a line of function define/call just like black would do"""
    max_chars = 88
    args_size = len(", ".join(args))
    if len(begin) + len(end) + args_size + len(INDENTATION * indent_lvl) <= max_chars:
        return [f"{INDENTATION * indent_lvl}{begin}{', '.join(args)}{end}"]
    if args_size + len(INDENTATION * (indent_lvl + 1)) <= max_chars:
        return [
            f"{INDENTATION * indent_lvl}{begin}",
            f"{INDENTATION * (indent_lvl + 1)}{', '.join(args)}",
            f"{INDENTATION * indent_lvl}{end}",
        ]
    return (
        [f"{INDENTATION * indent_lvl}{begin}"]
        + [f"{INDENTATION * (indent_lvl + 1)}{line}," for line in args]
        + [f"{INDENTATION * indent_lvl}{end}"]
    )


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


def read_lines(
    type_: Type, size: str, input_data: Input, style: FormatStyle = FormatStyle.DEFAULT
) -> List[str]:
    """Generate the Python code to read the lines for a given type"""
    if type_.fits_in_one_line(input_data.structs, style):
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
        lines.append("for _ in range({})".format(size))
        if len(lines[0]) < 5:
            lines[0] = "[" + lines[0]
        else:
            lines = ["["] + [INDENTATION + i for i in lines]
        lines.append("]")
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


def read_vars(input_data: Input) -> List[str]:
    """Read all input variables"""
    lines = []
    for variables in input_data.get_all_vars():
        if len(variables) == 1:
            var = variables[0]
            var_lines = read_lines(
                var.type, var_name(var.type.size), input_data, var.format_style
            )
            var_lines[0] = f"{var_name(var.name)} = {var_lines[0]}"
            lines.extend(var_lines)
        else:
            assert all(var.type.main == TypeEnum.INT for var in variables)
            lines.append(
                ", ".join(var_name(i.name) for i in variables)
                + " = map(int, input().split())"
            )
    return lines


def print_line(name: str, type_: Type, input_data: Input, style: FormatStyle) -> str:
    """Print the content of a var in one line"""
    assert type_.fits_in_one_line(input_data.structs, style)
    if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
        end = ", end=' '" if style == FormatStyle.NO_ENDLINE else ""
        return f"print({name}{end})"
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

    def call(self, reprint: bool) -> None:
        """Declare and call the function take all inputs in arguments"""
        name = var_name(self.input.name)
        self.method.extend(
            wrap_line(
                f"def {name}(",
                ") -> None:",
                [f"{var_name(i.name)}: {type_str(i.type)}" for i in self.input.input],
            )
        )
        self.method.append(INDENTATION + '"""')
        for arg in self.input.input:
            self.method.append(
                "{}:param {}: {}".format(INDENTATION, var_name(arg.name), arg.comment)
            )
        self.method.append(INDENTATION + '"""')
        if reprint:
            for var in self.input.input:
                self.method.extend(
                    self.print_lines(var_name(var.name), var.type, 1, var.format_style)
                )
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
        self.main.extend(
            wrap_line(f"{name}(", ")", [var_name(i.name) for i in self.input.input])
        )

    def print_lines(
        self,
        name: str,
        type_: Type,
        indent_lvl: int,
        style: FormatStyle = FormatStyle.DEFAULT,
    ) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        indent = INDENTATION * indent_lvl
        if type_.fits_in_one_line(self.input.structs, style):
            return [indent + print_line(name, type_, self.input, style)]
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

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        output = ""
        for line in decl_imports(self.input):
            output += line + "\n"
        for line in decl_classes(self.input):
            output += line + "\n"

        self.main.extend(read_vars(self.input))
        self.call(reprint)
        for line in self.method:
            output += line + "\n"
        if self.method:
            output += "\n"
        output += '\nif __name__ == "__main__":\n'
        for line in self.main:
            output += INDENTATION + line + "\n"
        return output


def gen_python(input_data: Input, reprint: bool = False) -> str:
    """Generate a Python code to parse input"""
    return ParserPython(input_data).content(reprint)
