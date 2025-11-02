# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2025 Sacha Delanoue
"""Generate a PHP parser"""

import textwrap

from iorgen.types import FormatStyle, Input, Type, TypeEnum
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
    if type_.main == TypeEnum.FLOAT:
        return "double"
    if type_.main == TypeEnum.CHAR:
        return "string"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        return f"{type_str(type_.encapsulated, input_data)}[]"
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    # the following is not PHPDoc, but it does not allow for anything more than
    # 'array'
    return "(array{{{}}})".format(
        ", ".join(f'"{v.name}": {type_str(v.type, input_data)}' for v in struct.fields)
    )


def read_line(type_: Type, input_data: Input) -> str:
    """Generate the PHP code to read a line of given type"""

    def cast_type(type_: Type) -> str:
        return {
            TypeEnum.INT: "intval",
            TypeEnum.FLOAT: "floatval",
            TypeEnum.CHAR: "strval",
        }[type_.main]

    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return "preg_split('//', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY)"
        assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
        return (
            f"array_map('{cast_type(type_.encapsulated)}', preg_split('/ /', "
            "trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY))"
        )
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        if all(i.type.main == TypeEnum.INT for i in struct.fields):
            cast = "array_map('intval', explode(' ', fgets(STDIN)))"
        elif all(i.type.main == TypeEnum.FLOAT for i in struct.fields):
            cast = "array_map('floatval', explode(' ', fgets(STDIN)))"
        elif all(i.type.main == TypeEnum.CHAR for i in struct.fields):
            cast = "explode(' ', trim(fgets(STDIN)))"
        else:
            fields = ", ".join(f"'{cast_type(f.type)}'" for f in struct.fields)
            func = "fn($f, $x) => $f($x)"
            cast = f"array_map({func}, [{fields}], explode(' ', trim(fgets(STDIN))))"
        return "array_combine([{}], {})".format(
            ", ".join(f'"{i.name}"' for i in struct.fields), cast
        )
    return {
        TypeEnum.INT: "intval(trim(fgets(STDIN)))",
        TypeEnum.FLOAT: "floatval(trim(fgets(STDIN)))",
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

    def read_lines(
        self, name: str, type_: Type, size: str, style: FormatStyle
    ) -> list[str]:
        """Generate the PHP code to read the lines for a given type"""
        if type_.fits_in_one_line(self.input.structs, style):
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
                    FormatStyle.DEFAULT,
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
            f'{name}["{{}}"]', lambda x: x
        ):
            read = self.read_lines(f_name, f_type, f_size, FormatStyle.DEFAULT)
            if len(read) == 1:
                read[0] = f"{f_name} = {read[0]};"
            lines.extend(read)
        return [f"{name} = [];"] + lines

    def read_vars(self) -> list[str]:
        """Generate the PHP code to read all input variables"""
        lines = []
        for variables in self.input.get_all_vars():
            name = ", ".join(var_name(i.name) for i in variables)
            type_ = variables[0].type
            style = variables[0].format_style
            if len(variables) != 1:
                name = f"list({name})"
                assert all(var.type.main == TypeEnum.INT for var in variables)
                type_ = Type(TypeEnum.LIST, str(len(variables)), Type(TypeEnum.INT))
                style = FormatStyle.DEFAULT
            size = var_name(type_.size)
            read = self.read_lines(name, type_, size, style)
            if len(read) == 1:
                read[0] = f"{name} = {read[0]};"
            lines.extend(read)
        return lines


def print_line(name: str, type_: Type, input_data: Input, style: FormatStyle) -> str:
    """Print the content of a var in one line"""
    assert type_.fits_in_one_line(input_data.structs, style)

    def print_type(name: str, type_: Type) -> str:
        """Print the content of a variable (with a special case for float)"""
        if type_.main != TypeEnum.FLOAT:
            return name
        return (
            "preg_replace('/e-([1-9])$/', 'e-0$1', "
            f"str_replace('.0e', 'e', sprintf('%.15g', {name})))"
        )

    if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
        endline = " " if style == FormatStyle.NO_ENDLINE else r"\n"
        return f'echo {print_type(name, type_)}, "{endline}";'
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return f'echo join("", {name}), "\\n";'
        if type_.encapsulated.main == TypeEnum.INT:
            return f'echo join(" ", {name}), "\\n";'
        assert type_.encapsulated.main == TypeEnum.FLOAT
        return (
            'echo join(" ", array_map(fn($x) => '
            f'{print_type("$x", type_.encapsulated)}, {name})), "\\n";'
        )
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return 'echo {}, "\\n";'.format(
        ", ' ', ".join(print_type(f'{name}["{i.name}"]', i.type) for i in struct.fields)
    )


def print_lines(
    input_data: Input,
    name: str,
    type_: Type,
    indent_lvl: int,
    style: FormatStyle = FormatStyle.DEFAULT,
) -> list[str]:
    """Print the content of a var that holds in one or more lines"""
    indent = INDENTATION * indent_lvl
    if type_.fits_in_one_line(input_data.structs, style):
        return [indent + print_line(name, type_, input_data, style)]
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
        lines.extend(print_lines(input_data, f'{name}["{i.name}"]', i.type, indent_lvl))
    return lines


def call(input_data: Input, reprint: bool) -> list[str]:
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
                (
                    ""
                    if i.type.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR)
                    else "&"
                )
                + var_name(i.name)
                for i in input_data.input
            ),
        )
    )
    if reprint:
        for var in input_data.input:
            out.extend(
                print_lines(
                    input_data, var_name(var.name), var.type, 1, var.format_style
                )
            )
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
