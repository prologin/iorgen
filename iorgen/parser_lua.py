# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
"""Generate a Lua parser"""

import textwrap
from typing import List

from iorgen.types import FormatStyle, Input, Type, TypeEnum, Variable
from iorgen.utils import snake_case, IteratorName

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Lua"""
    candidate = snake_case(name)
    return candidate + "_" if candidate in KEYWORDS else candidate


def type_str(type_: Type, input_data: Input, show_table: bool = True) -> str:
    """Transform a type into a string description for documentation"""
    if type_.main in (TypeEnum.INT, TypeEnum.FLOAT):
        return "number"
    if type_.main == TypeEnum.CHAR:
        return "string"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        return "{}array[{}]".format(
            "table: " if show_table else "",
            type_str(type_.encapsulated, input_data, False),
        )
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return "{}{{{}}}".format(
        "table: " if show_table else "",
        ", ".join(
            '"{}": {}'.format(v.name, type_str(v.type, input_data, False))
            for v in struct.fields
        ),
    )


def format_specifier(type_: Type, read: bool = True) -> str:
    """Return lua print format specifier for a type"""
    if type_.main == TypeEnum.INT:
        return "-?%d+" if read else "%s"
    if type_.main == TypeEnum.FLOAT:
        return "-?%g+" if read else "%.15g"
    return "%S" if read else "%s"


def read_line(
    name: str, type_: Type, input_data: Input, iterator: IteratorName
) -> List[str]:
    """Generate the Lua code to read a line of given type"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        inner = iterator.new_it()
        iterator.pop_it()
        if type_.encapsulated.main == TypeEnum.CHAR:
            return [
                "{} = {{}}".format(name),
                'io.read():gsub(".",function({0}) table.insert({1}, {0}) end)'.format(
                    inner, name
                ),
            ]
        assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
        specifier = format_specifier(type_.encapsulated)
        return [
            f"{name} = {{}}",
            f'for {inner} in string.gmatch(io.read(), "{specifier}") do',
            INDENTATION + "table.insert({}, tonumber({}))".format(name, inner),
            "end",
        ]
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        words = iterator.new_it()
        iterator.pop_it()
        pattern = " ".join(f"({format_specifier(i.type)})" for i in struct.fields)
        keys = (
            '["{}"]'.format(i.name) if " " in i.name else i.name for i in struct.fields
        )
        values = (
            (
                "tonumber({}[{}])".format(words, i + 1)
                if f.type.main in (TypeEnum.INT, TypeEnum.FLOAT)
                else "{}[{}]".format(words, i + 1)
            )
            for (i, f) in enumerate(struct.fields)
        )
        return [
            'local {} = {{string.match(io.read(), "{}")}}'.format(words, pattern),
            "{} = {{{}}}".format(
                name, ", ".join(i + " = " + j for (i, j) in zip(keys, values))
            ),
        ]
    return [
        name
        + " = "
        + {
            TypeEnum.INT: "tonumber(io.read())",
            TypeEnum.FLOAT: "tonumber(io.read())",
            TypeEnum.CHAR: "io.read()",
            TypeEnum.STR: "io.read()",
        }[type_.main]
    ]


def read_lines(
    var: Variable, size: str, input_data: Input, iterator: IteratorName
) -> List[str]:
    """Generate the Lua code to read the lines for a given type"""
    if var.fits_in_one_line(input_data.structs):
        return read_line(var.name, var.type, input_data, iterator)
    if var.type.main == TypeEnum.LIST:
        assert var.type.encapsulated is not None
        inner = iterator.new_it()
        lines = read_lines(
            Variable(f"{var.name}[{inner}]", "", var.type.encapsulated),
            var_name(var.type.encapsulated.size),
            input_data,
            iterator,
        )
        iterator.pop_it()
        return (
            [f"{var.name} = {{}}", f"for {inner} = 1, {size} do"]
            + [INDENTATION + i for i in lines]
            + ["end"]
        )
    assert var.type.main == TypeEnum.STRUCT
    struct = input_data.get_struct(var.type.struct_name)
    lines = [var.name + " = {}"]
    for i, field in enumerate(struct.fields):
        f_size = var_name(field.type.size)
        if i == 1 and struct.is_sized_struct():
            f_size = f'{var.name}["{struct.fields[0].name}"]'
        lines.extend(
            read_lines(
                Variable(f'{var.name}["{field.name}"]', "", field.type),
                f_size,
                input_data,
                iterator,
            )
        )
    return lines


def read_vars(input_data: Input, iterator: IteratorName) -> List[str]:
    """Read all input variables"""
    all_lines = []
    for variables in input_data.get_all_vars():
        if len(variables) == 1:
            var = variables[0]
            lines = read_lines(
                Variable(var_name(var.name), "", var.type, var.format_style),
                var_name(var.type.size),
                input_data,
                iterator,
            )
            if lines[0].startswith("local"):  # struct
                lines[1] = "local " + lines[1]
            else:
                lines[0] = "local " + lines[0]
            all_lines.extend(lines)
        else:
            assert all(var.type.main == TypeEnum.INT for var in variables)
            all_lines.append(
                'local {} = string.match(io.read(), "{}")'.format(
                    ", ".join(var_name(i.name) for i in variables),
                    " ".join(["(-?%d+)"] * len(variables)),
                )
            )
    return all_lines


def print_line(name: str, type_: Type, input_data: Input, style: FormatStyle) -> str:
    """Print the content of a var in one line"""
    assert type_.fits_in_one_line(input_data.structs, style)
    if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
        return (
            f"io.write({name}, ' ')"
            if style == FormatStyle.NO_ENDLINE
            else f"print({name})"
        )
    if type_.main == TypeEnum.FLOAT:
        return f'print(string.format("%.15g", {name}))'
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return 'print(table.concat({}, ""))'.format(name)
        if type_.encapsulated.main == TypeEnum.INT:
            return f'print(table.concat({name}, " "))'
        assert type_.encapsulated.main == TypeEnum.FLOAT
        return (
            f"for __i, __f in ipairs({name}) do "
            'if __i == 1 then io.write(string.format("%.15g", __f)) else '
            'io.write(string.format(" %.15g", __f)) end end print()'
        )
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return 'print(string.format("{}", {}))'.format(
        " ".join(format_specifier(f.type, read=False) for f in struct.fields),
        ", ".join(f'{name}["{i.name}"]' for i in struct.fields),
    )


def print_lines(
    name: str,
    type_: Type,
    input_data: Input,
    indent_lvl: int,
    style: FormatStyle = FormatStyle.DEFAULT,
) -> List[str]:
    """Print the content of a var that holds in one or more lines"""
    indent = INDENTATION * indent_lvl
    if type_.fits_in_one_line(input_data.structs, style):
        return [indent + print_line(name, type_, input_data, style)]
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        inner = "iT{}".format(indent_lvl)
        return (
            [indent + "for _, {} in ipairs({}) do".format(inner, name)]
            + print_lines(inner, type_.encapsulated, input_data, indent_lvl + 1)
            + [indent + "end"]
        )
    assert type_.main == TypeEnum.STRUCT
    lines = []
    for i in input_data.get_struct(type_.struct_name).fields:
        lines.extend(
            print_lines('{}["{}"]'.format(name, i.name), i.type, input_data, indent_lvl)
        )
    return lines


def call(input_data: Input, reprint: bool) -> List[str]:
    """Declare and call the function take all inputs in arguments"""
    name = var_name(input_data.name)
    lines = [
        "-- {} ({}): {}".format(
            var_name(arg.name), type_str(arg.type, input_data), arg.comment
        )
        for arg in input_data.input
    ]
    lines.append(
        "function {}({})".format(
            name, ", ".join(var_name(i.name) for i in input_data.input)
        )
    )
    if reprint:
        for var in input_data.input:
            lines.extend(
                print_lines(
                    var_name(var.name),
                    var.type,
                    input_data,
                    1,
                    var.format_style,
                )
            )
    else:
        lines.extend(
            textwrap.wrap(
                input_data.output,
                79,
                initial_indent=INDENTATION + "-- TODO ",
                subsequent_indent=INDENTATION + "-- ",
            )
        )
    return lines + ["end"]


def gen_lua(input_data: Input, reprint: bool = False) -> str:
    """Generate a Lua code to parse input"""
    iterator = IteratorName([var.name for var in input_data.input] + [input_data.name])
    output = "\n".join(call(input_data, reprint)) + "\n\n"
    for line in read_vars(input_data, iterator):
        output += line + "\n"
    args = (var_name(i.name) for i in input_data.input)
    output += "\n{}({})\n".format(var_name(input_data.name), ", ".join(args))
    return output


KEYWORDS = [
    "and",
    "break",
    "do",
    "else",
    "elseif",
    "end",
    "false",
    "for",
    "function",
    "if",
    "in",
    "local",
    "nil",
    "not",
    "or",
    "repeat",
    "return",
    "then",
    "true",
    "until",
    "while",
] + ["io", "string", "table", "tonumber"]
