# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
"""Generate a PHP parser"""

import textwrap
from typing import List

from iorgen.types import Input, Type, TypeEnum
from iorgen.utils import snake_case, IteratorName

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for PHP"""
    if not name:
        return ""
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


def type_str(type_: Type, input_data: Input) -> str:
    """Transform a type into a string description for documentation"""
    if type_.main == TypeEnum.INT:
        return "int"
    if type_.main == TypeEnum.CHAR:
        return "string"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        return "{}[]".format(type_str(type_.encapsulated, input_data))
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    # the following is not PHPDoc, but it does not allow for anything more than
    # 'array'
    return "(array{{{}}})".format(
        ", ".join(
            '"{}": {}'.format(v.name, type_str(v.type, input_data))
            for v in struct.fields
        )
    )


def read_line(type_: Type, input_data: Input) -> str:
    """Generate the PHP code to read a line of given type"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return "preg_split('//', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY)"
        assert type_.encapsulated.main == TypeEnum.INT
        return (
            "array_map('intval', preg_split('/ /', "
            "trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY))"
        )
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        begin = "array_combine([{}], ".format(
            ", ".join('"{}"'.format(i.name) for i in struct.fields)
        )
        if all(i.type.main == TypeEnum.INT for i in struct.fields):
            return begin + "array_map('intval', explode(' ', fgets(STDIN))))"
        # Treat them all as char. It would be best to cast the integers, if
        # they are any, but this is painful to write as a one liner.
        return begin + "explode(' ', trim(fgets(STDIN))))"
    return {
        TypeEnum.INT: "intval(trim(fgets(STDIN)))",
        TypeEnum.CHAR: "fgets(STDIN)[0]",
        TypeEnum.STR: "trim(fgets(STDIN))",
    }[type_.main]


class ParserPHP:
    """Create the PHP code to parse an input"""

    def __init__(self, input_data: Input):
        self.input = input_data
        self.iterator = IteratorName(
            [var.name for var in input_data.input] + [input_data.name]
        )

    def read_lines(self, name: str, type_: Type, size: str) -> List[str]:
        """Generate the PHP code to read the lines for a given type"""
        if type_.fits_in_one_line(self.input.structs):
            return [read_line(type_, self.input)]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            iterator = "$" + self.iterator.new_it()
            lines = []
            if type_.encapsulated.fits_in_one_line(self.input.structs):
                # Here we don't really care if the encapsulated type really fits on
                # on line. What we are interested in, is that the fact that the code
                # generated to read it will fit on one line, and will need to be
                # directly assigned to a variable
                lines = [
                    f"{name}[{iterator}] = {read_line(type_.encapsulated, self.input)};"
                ]
            else:
                tmp_name = "$" + self.iterator.new_it()
                lines = self.read_lines(
                    tmp_name,
                    type_.encapsulated,
                    var_name(type_.encapsulated.size),
                )
                lines.append(f"{name}[{iterator}] = {tmp_name};")
                self.iterator.pop_it()
            self.iterator.pop_it()
            prefix = [
                f"{name} = new SplFixedArray({size});",
                f"for ({iterator} = 0; {iterator} < {size}; {iterator}++) {{",
            ]
            return prefix + [INDENTATION + i for i in lines] + ["}"]
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        lines = []
        for f_name, f_type, f_size in struct.fields_name_type_size(
            '{}["{{}}"]'.format(name), lambda x: x
        ):
            read = self.read_lines(f_name, f_type, f_size)
            if len(read) == 1:
                read[0] = "{} = {};".format(f_name, read[0])
            lines.extend(read)
        return ["{} = [];".format(name)] + lines

    def read_vars(self) -> List[str]:
        """Generate the PHP code to read all input variables"""
        lines = []
        for var in self.input.input:
            read = self.read_lines(
                var_name(var.name), var.type, var_name(var.type.size)
            )
            if len(read) == 1:
                read[0] = "{} = {};".format(var_name(var.name), read[0])
            lines.extend(read)
        return lines


def print_line(name: str, type_: Type, input_data: Input) -> str:
    """Print the content of a var in one line"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
        return 'echo {}, "\\n";'.format(name)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return 'echo join("", {}), "\\n";'.format(name)
        assert type_.encapsulated.main == TypeEnum.INT
        return 'echo join(" ", {}), "\\n";'.format(name)
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return 'echo {}, "\\n";'.format(
        ", ' ', ".join('{}["{}"]'.format(name, i.name) for i in struct.fields)
    )


def print_lines(
    input_data: Input, name: str, type_: Type, indent_lvl: int = 0
) -> List[str]:
    """Print the content of a var that holds in one or more lines"""
    indent = INDENTATION * indent_lvl
    if type_.fits_in_one_line(input_data.structs):
        return [indent + print_line(name, type_, input_data)]
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        inner = "$iT" + str(abs(hash(name)))  # quick way to have a unique name
        return (
            [indent + f"foreach ({name} as $_ => {inner}) {{"]
            + print_lines(input_data, inner, type_.encapsulated, indent_lvl + 1)
            + [indent + "}"]
        )
    assert type_.main == TypeEnum.STRUCT
    lines = []
    for i in input_data.get_struct(type_.struct_name).fields:
        lines.extend(
            print_lines(input_data, '{}["{}"]'.format(name, i.name), i.type, indent_lvl)
        )
    return lines


def call(input_data: Input, reprint: bool) -> List[str]:
    """Declare the function that takes all inputs in arguments"""
    # https://docs.phpdoc.org/
    out = (
        ["/**"]
        + [
            " * @param {} {} {}".format(
                type_str(arg.type, input_data), var_name(arg.name), arg.comment
            )
            for arg in input_data.input
        ]
        + [" */"]
    )
    out.append(
        "function {}({}) {{".format(
            function_name(input_data.name),
            ", ".join(
                ("" if i.type.main in (TypeEnum.INT, TypeEnum.CHAR) else "&")
                + var_name(i.name)
                for i in input_data.input
            ),
        )
    )
    if reprint:
        for var in input_data.input:
            out.extend(print_lines(input_data, var_name(var.name), var.type, 1))
    else:
        out.extend(
            [
                INDENTATION + i
                for i in textwrap.wrap("/* TODO " + input_data.output + " */", 75)
            ]
        )
    out.append("}")
    return out


def gen_php(input_data: Input, reprint: bool = False) -> str:
    """Generate a PHP code to parse input"""
    output = "<?php\n"
    output += "\n".join(call(input_data, reprint))
    output += "\n\n"
    output += "\n".join(ParserPHP(input_data).read_vars())
    args = (var_name(i.name) for i in input_data.input)
    output += "\n{}({});".format(function_name(input_data.name), ", ".join(args))
    return output + "\n"


KEYWORDS = [
    "abstract",
    "and",
    "array",
    "as",
    "break",
    "callable",
    "case",
    "catch",
    "class",
    "clone",
    "const",
    "continue",
    "declare",
    "default",
    "die",
    "do",
    "echo",
    "else",
    "elseif",
    "empty",
    "enddeclare",
    "endfor",
    "endforeach",
    "endif",
    "endswitch",
    "endwhile",
    "eval",
    "exit",
    "extends",
    "final",
    "fn",
    "for",
    "foreach",
    "function",
    "global",
    "goto",
    "if",
    "implements",
    "include",
    "include_once",
    "instanceof",
    "insteadof",
    "interface",
    "isset",
    "list",
    "namespace",
    "new",
    "or",
    "print",
    "private",
    "protected",
    "public",
    "require",
    "require_once",
    "return",
    "static",
    "switch",
    "throw",
    "trait",
    "try",
    "unset",
    "use",
    "var",
    "while",
    "xor",
]
