# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
"""Generate a Ruby parser"""

import textwrap
from typing import List

from iorgen.types import FormatStyle, Input, Type, TypeEnum
from iorgen.utils import snake_case, IteratorName


INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Ruby"""
    candidate = snake_case(name)
    return candidate + "_" if candidate in KEYWORDS else candidate


def parse_type(type_: Type) -> str:
    """Return ruby method to parse a type."""
    if type_.main == TypeEnum.INT:
        return ":to_i"
    if type_.main == TypeEnum.FLOAT:
        return ":to_f"
    return ":to_s"


def read_line(type_: Type, input_data: Input) -> str:
    """Generate the Ruby code to read a line of given type"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return 'STDIN.gets.chomp.split("")'
        assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
        return f"STDIN.gets.split.map(&{parse_type(type_.encapsulated)})"
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        keys = ", ".join('"{}"'.format(i.name) for i in struct.fields)
        for inner_type, parse in (
            (TypeEnum.INT, "split.map(&:to_i)"),
            (TypeEnum.FLOAT, "split.map(&:to_f)"),
            (TypeEnum.CHAR, 'chomp.split(" ")'),
        ):
            if all(i.type.main == inner_type for i in struct.fields):
                return f"Hash[[{keys}].zip(STDIN.gets.{parse})]"
        fields = ", ".join(parse_type(i.type) for i in struct.fields)
        block = "|k,v,s| [k, v.send(s)]"
        return f"Hash[[{keys}].zip(STDIN.gets.split, [{fields}]).map{{ {block} }}]"
    return {
        TypeEnum.INT: "STDIN.gets.to_i",
        TypeEnum.FLOAT: "STDIN.gets.to_f",
        TypeEnum.CHAR: "STDIN.gets[0]",
        TypeEnum.STR: "STDIN.gets.chomp",
    }[type_.main]


def read_lines(
    type_: Type,
    size: str,
    input_data: Input,
    iterator: IteratorName,
    style: FormatStyle = FormatStyle.DEFAULT,
) -> List[str]:
    """Generate the Ruby code to read the lines for a given type"""
    if type_.fits_in_one_line(input_data.structs, style):
        return [read_line(type_, input_data)]
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        lines = read_lines(
            type_.encapsulated, var_name(type_.encapsulated.size), input_data, iterator
        )
        if len(lines) == 1:
            candidate = "Array.new({}) {{ {} }}".format(size, lines[0])
            if len(candidate) <= 75:
                return [candidate]
        if lines[0][0] == "{":
            lines[0] = "Array.new({}) {{ {}".format(size, lines[0])
        else:
            lines = ["Array.new({}) {{".format(size)] + [INDENTATION + i for i in lines]
        if lines[-1][-1] == "}":
            lines[-1] += " }"
        else:
            lines.append("}")
        return lines
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    if struct.is_sized_struct():
        inner = iterator.new_it()
        lines = read_lines(struct.fields[1].type, inner, input_data, iterator)
        iterator.pop_it()
        lines[0] = '"{}" => {}'.format(struct.fields[1].name, lines[0])
        return (
            [
                "(lambda {{ |{}| {{".format(inner),
                INDENTATION + '"{}" => {},'.format(struct.fields[0].name, inner),
            ]
            + [INDENTATION + i for i in lines]
            + ["} }).call(STDIN.gets.to_i)"]
        )
    fields = []
    for i, field in enumerate(struct.fields):
        lines = read_lines(field.type, var_name(field.type.size), input_data, iterator)
        lines[0] = '{}"{}" => {}'.format(INDENTATION, field.name, lines[0])
        if i != len(struct.fields) - 1:
            lines[-1] += ","
        fields.append(lines[0])
        fields.extend([INDENTATION + i for i in lines[1:]])
    return ["{"] + fields + ["}"]


def read_vars(input_data: Input, iterator: IteratorName) -> List[str]:
    """Read all input variables"""
    lines = []
    for variables in input_data.get_all_vars():
        if len(variables) == 1:
            var = variables[0]
            var_lines = read_lines(
                var.type,
                var_name(var.type.size),
                input_data,
                iterator,
                var.format_style,
            )
            var_lines[0] = f"{var_name(var.name)} = {var_lines[0]}"
            lines.extend(var_lines)
        else:
            assert all(var.type.main == TypeEnum.INT for var in variables)
            lines.append(
                ", ".join(var_name(i.name) for i in variables)
                + " = STDIN.gets.split.map(&:to_i)"
            )
    return lines


def print_line(name: str, type_: Type, input_data: Input) -> str:
    """Print the content of a var in one line"""

    def print_type(name: str, type_: Type) -> str:
        """Print the content of a variable (with a special case for float)"""
        if type_.main != TypeEnum.FLOAT:
            return name
        return f"sprintf('%.15g', {name})"

    assert type_.fits_in_one_line(input_data.structs)
    if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
        return f"puts {print_type(name, type_)}"
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return f'puts {name}.join("")'
        if type_.encapsulated.main == TypeEnum.INT:
            return f'puts {name}.join(" ")'
        assert type_.encapsulated.main == TypeEnum.FLOAT
        return f'puts {name}.map{{ |x| sprintf("%.15g", x) }}.join(" ")'
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return "print {}".format(
        ', " ", '.join(print_type(f'{name}["{i.name}"]', i.type) for i in struct.fields)
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
        return [indent + print_line(name, type_, input_data)]
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        inner = "iT{}".format(indent_lvl)
        return (
            [indent + "{}.each {{ |{}|".format(name, inner)]
            + print_lines(inner, type_.encapsulated, input_data, indent_lvl + 1)
            + [indent + "}"]
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
        "# +{}+:: {}".format(var_name(arg.name), arg.comment)
        for arg in input_data.input
    ]
    lines.append(
        "def {}({})".format(name, ", ".join(var_name(i.name) for i in input_data.input))
    )
    if reprint:
        lines.append(INDENTATION + '$\\ = "\\n"')
        for variables in input_data.get_all_vars():
            if len(variables) == 1:
                var = variables[0]
                lines.extend(
                    print_lines(
                        var_name(var.name), var.type, input_data, 1, var.format_style
                    )
                )
            else:
                lines.append(
                    INDENTATION
                    + "puts "
                    + f'[{", ".join(var_name(i.name) for i in variables)}].join(" ")'
                )
    else:
        lines.extend(
            textwrap.wrap(
                input_data.output,
                79,
                initial_indent=INDENTATION + "# " + "TODO ",
                subsequent_indent=INDENTATION + "# ",
            )
        )
    return lines + ["end"]


def gen_ruby(input_data: Input, reprint: bool = False) -> str:
    """Generate a Ruby code to parse input"""
    iterator = IteratorName([var.name for var in input_data.input] + [input_data.name])
    output = "\n".join(call(input_data, reprint)) + "\n\n"
    for line in read_vars(input_data, iterator):
        output += line + "\n"
    args = (var_name(i.name) for i in input_data.input)
    output += "\n{}({})\n".format(var_name(input_data.name), ", ".join(args))
    return output


KEYWORDS = [
    "alias",
    "and",
    "begin",
    "break",
    "case",
    "class",
    "def",
    "defined?",
    "do",
    "else",
    "elsif",
    "end",
    "ensure",
    "false",
    "for",
    "if",
    "in",
    "module",
    "next",
    "nil",
    "not",
    "or",
    "redo",
    "rescue",
    "retry",
    "return",
    "self",
    "super",
    "then",
    "true",
    "undef",
    "unless",
    "until",
    "when",
    "while",
    "yield ",
]
