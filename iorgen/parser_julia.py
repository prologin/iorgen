# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2021 Léo Lanteri Thauvin
"""Generate a Julia parser"""

import textwrap
from typing import List

from iorgen.types import Input, Struct, Type, TypeEnum, Variable
from iorgen.utils import snake_case, pascal_case


INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Julia"""
    candidate = snake_case(name)
    return candidate + "_" if candidate in RESERVED else candidate


def struct_name(name: str) -> str:
    """Transform a struct name into a valid one for Julia"""
    candidate = pascal_case(name)
    if candidate in RESERVED:
        return candidate + "_"
    return candidate


def type_str(type_: Type, input_data: Input) -> str:
    """Return a description for a type"""
    if type_.main == TypeEnum.INT:
        return "Int"
    if type_.main == TypeEnum.STR:
        return "String"
    if type_.main == TypeEnum.CHAR:
        return "Char"
    if type_.main == TypeEnum.STRUCT:
        return struct_name(type_.struct_name)
    assert type_.encapsulated
    assert type_.main == TypeEnum.LIST
    return "Vector{{{}}}".format(type_str(type_.encapsulated, input_data))


def read_line(type_: Type, input_data: Input, input_str: str = "readline()") -> str:
    """Generate the Julia code to read a line of given type"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return f"collect({input_str})"
        assert type_.encapsulated.main == TypeEnum.INT
        return f"map(s -> parse(Int, s), split({input_str}))"
    if type_.main == TypeEnum.STRUCT:
        return f"parse({struct_name(type_.struct_name)}, {input_str})"
    return {
        TypeEnum.INT: f"parse(Int, {input_str})",
        TypeEnum.CHAR: f"{input_str}[1]",
        TypeEnum.STR: input_str,
    }[type_.main]


def read_lines(type_: Type, size: str, input_data: Input) -> List[str]:
    """Generate the Julia code to read the lines for a given type"""
    if type_.fits_in_one_line(input_data.structs):
        return [read_line(type_, input_data)]
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        lines = read_lines(
            type_.encapsulated, var_name(type_.encapsulated.size), input_data
        )
        if len(lines) == 1:
            candidate = "[ {} for _=1:{} ]".format(lines[0], size)
            if len(candidate) <= 75:
                return [candidate]
        if len(lines[-1]) < 5:
            lines[-1] += " for _=1:{} ]".format(size)
        else:
            lines.append("for _=1:{}".format(size))
            lines.append("]")
        if len(lines[0]) < 5:
            lines[0] = "[ " + lines[0]
        else:
            lines = ["["] + [INDENTATION + i for i in lines]
        return lines
    assert type_.main == TypeEnum.STRUCT
    return [f"read_struct_{var_name(type_.struct_name)}()"]


def print_line(name: str, type_: Type, input_data: Input) -> str:
    """Print the content of a var in one line"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
        return "println({})".format(name)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return "println(join({}))".format(name)
        assert type_.encapsulated.main == TypeEnum.INT
        return "println(join({}, ' '))".format(name)
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return "println({})".format(
        ", ' ', ".join("{}.{}".format(name, var_name(i.name)) for i in struct.fields)
    )


class ParserJulia:
    """Create the Julia code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data
        self.main = []  # type: List[str]
        self.method = []  # type: List[str]
        self.import_parse = False  # whether to import Base.parse

    def read_var(self, var: Variable) -> List[str]:
        """Read a variable"""
        lines = read_lines(var.type, var_name(var.type.size), self.input)
        lines[0] = "{} = {}".format(var_name(var.name), lines[0])
        return lines

    def decl_struct(self, struct: Struct) -> List[str]:
        """Return the Julia code for declaring a struct"""
        lines = [
            '"""',
            struct.comment,
            '"""',
            f"struct {struct_name(struct.name)}",
        ]

        for field in struct.fields:
            lines += [
                f'    """{field.comment}"""',
                f"    {var_name(field.name)}::{type_str(field.type, self.input)}",
            ]

        return lines + ["end", ""]

    def def_read_struct(self, struct: Struct) -> List[str]:
        """Generate the Julia `parse` function for a struct"""
        s_name = struct_name(struct.name)

        if Type(TypeEnum.STRUCT, struct_name=struct.name).fits_in_one_line(
            self.input.structs
        ):
            self.import_parse = True
            parsed_fields = [
                read_line(field.type, self.input, input_str=f"s[{i + 1}]")
                for i, field in enumerate(struct.fields)
            ]
            return [
                f"function parse(::Type{{{s_name}}}, s::AbstractString)",
                "    s = split(s)",
                "    {}({})".format(s_name, ", ".join(parsed_fields)),
                "end",
            ]

        fields = ", ".join(var_name(field.name) for field in struct.fields)
        lines = []
        lines.append(f"function read_struct_{var_name(struct.name)}()")

        for field in struct.fields:
            lines.extend(INDENTATION + line for line in self.read_var(field))

        return lines + [
            f"    {s_name}({fields})",
            "end",
        ]

    def call(self, reprint: bool) -> None:
        """Declare and call the function take all inputs in arguments"""
        name = var_name(self.input.name)
        self.method.append('"""')
        for arg in self.input.input:
            self.method.append(
                "- `{}::{}`: {}".format(
                    var_name(arg.name), type_str(arg.type, self.input), arg.comment
                )
            )
        self.method.append('"""')
        self.method.append(
            "function {}({})".format(
                name,
                ", ".join(
                    "{}::{}".format(var_name(i.name), type_str(i.type, self.input))
                    for i in self.input.input
                ),
            )
        )
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
        self.method.append("end")
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
            return (
                [indent + "for {} in {}".format(inner, name)]
                + self.print_lines(inner, type_.encapsulated, indent_lvl + 1)
                + [indent + "end"]
            )
        assert type_.main == TypeEnum.STRUCT
        lines = []
        for i in self.input.get_struct(type_.struct_name).fields:
            lines.extend(
                self.print_lines(
                    "{}.{}".format(name, var_name(i.name)), i.type, indent_lvl
                )
            )
        return lines

    def content(self) -> str:
        """Return the parser content"""
        output = ""
        struct_parsers = [self.def_read_struct(struct) for struct in self.input.structs]
        if self.import_parse:
            output += "import Base: parse\n\n"
        for struct in self.input.structs:
            for line in self.decl_struct(struct):
                output += line + "\n"
            output += "\n"
        for line in self.method:
            output += line + "\n"
        if self.method:
            output += "\n"
        for parser in struct_parsers:
            for line in parser:
                output += line + "\n"
            output += "\n"
        for line in self.main:
            output += line + "\n"
        return output


def gen_julia(input_data: Input, reprint: bool = False) -> str:
    """Generate a Julia code to parse input"""
    parser = ParserJulia(input_data)
    for var in input_data.input:
        parser.main.extend(parser.read_var(var))
    parser.call(reprint)
    return parser.content()


RESERVED = [
    "baremodule",
    "begin",
    "break",
    "catch",
    "const",
    "continue",
    "do",
    "else",
    "elseif",
    "end",
    "export",
    "false",
    "finally",
    "for",
    "function",
    "global",
    "if",
    "import",
    "let",
    "local",
    "macro",
    "module",
    "parse",
    "quote",
    "return",
    "struct",
    "true",
    "try",
    "using",
    "while",
    "Char",
    "Int",
    "Main",
    "String",
    "Vector",
]
