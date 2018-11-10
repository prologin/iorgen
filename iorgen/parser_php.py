# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a PHP parser"""

import textwrap
from typing import List

from iorgen.types import Input, Type, TypeEnum
from iorgen.utils import snake_case

KEYWORDS = [
    'abstract', 'and', 'array', 'as', 'break', 'callable', 'case', 'catch',
    'class', 'clone', 'const', 'continue', 'declare', 'default', 'die', 'do',
    'echo', 'else', 'elseif', 'empty', 'enddeclare', 'endfor', 'endforeach',
    'endif', 'endswitch', 'endwhile', 'eval', 'exit', 'extends', 'final',
    'for', 'foreach', 'function', 'global', 'goto', 'if', 'implements',
    'include', 'include_once', 'instanceof', 'insteadof', 'interface', 'isset',
    'list', 'namespace', 'new', 'or', 'print', 'private', 'protected',
    'public', 'require', 'require_once', 'return', 'static', 'switch', 'throw',
    'trait', 'try', 'unset', 'use', 'var', 'while', 'xor'
]


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for PHP"""
    snake = snake_case(name)
    try:
        int(snake)
        return snake
    except ValueError:
        return "$" + snake


def function_name(name: str) -> str:
    """Transform a function name into a valid one for PHP"""
    candidate = snake_case(name)
    return candidate + "_" if candidate in KEYWORDS else candidate


def read_line(type_: Type, input_data: Input) -> str:
    """Generate the PHP code to read a line of given type"""
    assert type_.fits_it_one_line(input_data.structs)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return "str_split(trim(fgets(STDIN)))"
        if type_.encapsulated.main == TypeEnum.INT:
            return "array_map('intval', explode(' ', fgets(STDIN)))"
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        begin = "array_combine([{}], ".format(", ".join(
            '"{}"'.format(i.name) for i in struct.fields))
        if all(i.type.main == TypeEnum.INT for i in struct.fields):
            return begin + "array_map('intval', explode(' ', fgets(STDIN))))"
        if all(i.type.main == TypeEnum.CHAR for i in struct.fields):
            return begin + "str_split(trim(fgets(STDIN))))"
        assert False, "Not implemented"
    return {
        TypeEnum.INT: "intval(trim(fgets(STDIN)))",
        TypeEnum.CHAR: "fgets(STDIN)[0]",
        TypeEnum.STR: "trim(fgets(STDIN))"
    }[type_.main]


def read_vars(input_data: Input) -> List[str]:
    """Generate the PHP code to read all input variables"""
    return [
        "{} = {};".format(
            var_name(var.name), read_lines(var.type, input_data))
        for var in input_data.input
    ]


def read_lines(type_: Type, input_data: Input) -> str:
    """Generate the PHP code to read the lines for a given type"""
    if type_.fits_it_one_line(input_data.structs):
        return read_line(type_, input_data)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        return "array_map(function() {{ return {}; }}, range(1, {}))".format(
            read_lines(type_.encapsulated, input_data), var_name(type_.size))
    if type_.main == TypeEnum.STRUCT:
        return "array({})".format(", ".join(
            '"{}" => {}'.format(i.name, read_lines(i.type, input_data))
            for i in input_data.get_struct(type_.struct_name).fields))
    assert False
    return ""


def print_line(name: str, type_: Type, input_data: Input) -> str:
    """Print the content of a var in one line"""
    assert type_.fits_it_one_line(input_data.structs)
    if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
        return 'echo {}, "\\n";'.format(name)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return 'echo join("", {}), "\\n";'.format(name)
        if type_.encapsulated.main == TypeEnum.INT:
            return 'echo join(" ", {}), "\\n";'.format(name)
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        return 'echo {}, "\\n";'.format(", ' ', ".join(
            '{}["{}"]'.format(name, i.name) for i in struct.fields))
    assert False
    return ""


def print_lines(input_data: Input, name: str, type_: Type,
                indent_lvl: int = 0) -> List[str]:
    """Print the content of a var that holds in one or more lines"""
    indent = "    " * indent_lvl
    if type_.fits_it_one_line(input_data.structs):
        return [indent + print_line(name, type_, input_data)]
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        inner = name + "It"
        return [indent + "foreach ({} as &{}) {{".format(name, inner)
                ] + print_lines(input_data, inner, type_.encapsulated,
                                indent_lvl + 1) + [indent + "}"]
    if type_.main == TypeEnum.STRUCT:
        lines = []
        for i in input_data.get_struct(type_.struct_name).fields:
            lines.extend(
                print_lines(input_data, '{}["{}"]'.format(name, i.name),
                            i.type, indent_lvl))
        return lines
    assert False
    return []


def call(input_data: Input, reprint: bool) -> List[str]:
    """Declare the function that takes all inputs in arguments"""
    out = ["/**"] + [
        " * @param {} {}".format(var_name(arg.name), arg.comment)
        for arg in input_data.input
    ] + [" */"]
    out.append("function {}({}) {{".format(
        function_name(input_data.name),
        ", ".join(("" if i.type in (TypeEnum.INT, TypeEnum.CHAR) else "&") +
                  var_name(i.name) for i in input_data.input)))
    if reprint:
        for var in input_data.input:
            out.extend(
                print_lines(input_data, var_name(var.name), var.type, 1))
    else:
        out.extend([
            "    " + i
            for i in textwrap.wrap("/* TODO " + input_data.output + " */", 75)
        ])
    out.append("}")
    return out


def gen_php(input_data: Input, reprint: bool = False) -> str:
    """Generate a PHP code to parse input"""
    output = "<?php\n"
    output += "\n".join(call(input_data, reprint))
    output += "\n\n"
    output += "\n".join(read_vars(input_data))
    args = (var_name(i.name) for i in input_data.input)
    output += "\n{}({});".format(
        function_name(input_data.name), ", ".join(args))
    return output + "\n"
