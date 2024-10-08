# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
# Copyright 2019 Victor Collod
"""Generate a Javascript parser"""

import textwrap
from typing import List

from iorgen.types import FormatStyle, Input, Type, TypeEnum, Variable
from iorgen.utils import camel_case, IteratorName, WordsName

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Javascript"""
    candidate = camel_case(name)
    if candidate in ("process", "stdin", "line", "main"):
        return candidate + "_"
    return candidate + "_" if candidate in KEYWORDS else candidate


def type_str(type_: Type, input_data: Input) -> str:
    """Return the Javascript name for a type"""
    # https://jsdoc.app/tags-type.html
    if type_.main in (TypeEnum.INT, TypeEnum.FLOAT):
        return "number"
    if type_.main == TypeEnum.CHAR:
        return "string"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        return "{{{}}}".format(
            ", ".join(
                "{}: {}".format(
                    v.name if " " not in v.name else "'{}'".format(v.name),
                    type_str(v.type, input_data),
                )
                for v in struct.fields
            )
        )
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated
    return "Array.<{}>".format(type_str(type_.encapsulated, input_data))


class ParserJS:
    """Create the Javascript code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data
        existing_names = [var_name(var.name) for var in input_data.input] + [
            var_name(input_data.name)
        ]
        self.iterator = IteratorName(existing_names)
        self.words = WordsName(existing_names)

    def read_line(
        self, decl: bool, name: str, type_: Type, size: str, indent_lvl: int
    ) -> List[str]:
        # pylint: disable=too-many-arguments
        # pylint: disable=too-many-positional-arguments
        """Generate the Javascript code to read a line of given type"""
        assert type_.fits_in_one_line(self.input.structs)
        indent = INDENTATION * indent_lvl
        start = indent + ("const " if decl else "") + name + " = "
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return [start + 'stdin[line++].split("");']
            assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
            return [start + 'stdin[line++].split(" ", {}).map(Number);'.format(size)]
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            words = self.words.next_name()
            lines = [
                indent + 'const {} = stdin[line++].split(" ");'.format(words),
                start + "{",
            ]
            lines.extend(
                indent
                + INDENTATION
                + "{}: {}{}".format(
                    var_name(field.name),
                    (
                        "{}[{}]".format(words, i)
                        if field.type.main == TypeEnum.CHAR
                        else "Number({}[{}])".format(words, i)
                    ),
                    "," if i != len(struct.fields) - 1 else "",
                )
                for i, field in enumerate(struct.fields)
            )
            return lines + [indent + "};"]
        return [
            start
            + {
                TypeEnum.INT: "Number(stdin[line++]);",
                TypeEnum.FLOAT: "Number(stdin[line++]);",
                TypeEnum.CHAR: "stdin[line++];",
                TypeEnum.STR: "stdin[line++];",
            }[type_.main]
        ]

    def read_lines(
        self, decl: bool, var: Variable, size: str, indent_lvl: int
    ) -> List[str]:
        # pylint: disable=too-many-arguments
        """Generate the Javascript code to read the lines for a given type"""
        if var.fits_in_one_line(self.input.structs):
            return self.read_line(decl, var.name, var.type, size, indent_lvl)
        indent = INDENTATION * indent_lvl
        if var.type.main == TypeEnum.LIST:
            assert var.type.encapsulated is not None
            lines = [indent + f"{'const ' if decl else ''}{var.name} = [];"]
            iterator = self.iterator.new_it()
            inner_name = self.iterator.new_it()
            lines.append(
                indent + "for (let {0} = 0; {0} < {1}; {0}++) {{".format(iterator, size)
            )
            self.words.push_scope()
            lines.extend(
                self.read_lines(
                    True,
                    Variable(inner_name, "", var.type.encapsulated),
                    var_name(var.type.encapsulated.size),
                    indent_lvl + 1,
                )
            )
            lines.append(indent + INDENTATION + f"{var.name}.push({inner_name});")
            self.words.pop_scope()
            self.iterator.pop_it()
            self.iterator.pop_it()
            return lines + [indent + "}"]
        assert var.type.main == TypeEnum.STRUCT
        struct = self.input.get_struct(var.type.struct_name)
        lines = [indent + f"{'const ' if decl else ''}{var.name} = {{}};"]
        for f_name, f_type, f_size in struct.fields_name_type_size(
            f"{var.name}.{{}}", var_name
        ):
            lines.extend(
                self.read_lines(False, Variable(f_name, "", f_type), f_size, indent_lvl)
            )
        return lines

    def read_vars(self) -> List[str]:
        """Generate the Javascript code to read all input variables"""
        lines = []
        for variables in self.input.get_all_vars():
            if len(variables) == 1:
                var = variables[0]
                lines.extend(
                    self.read_lines(
                        True,
                        Variable(var_name(var.name), "", var.type, var.format_style),
                        var_name(var.type.size),
                        1,
                    )
                )
            else:
                lines.append(
                    INDENTATION
                    + "const ["
                    + ", ".join(var_name(i.name) for i in variables)
                    + '] = stdin[line++].split(" ").map(Number);'
                )
        return lines


def print_line(name: str, type_: Type, input_data: Input) -> str:
    """Print the content of a var in one line"""

    def print_type(type_: Type, name: str) -> str:
        if type_.main == TypeEnum.FLOAT:
            return f"{name}.toString().replace(/e-([1-9])$/,'e-0$1')"
        return name

    assert type_.fits_in_one_line(input_data.structs)
    if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
        return f"console.log({print_type(type_, name)});"
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return 'console.log({}.join(""));'.format(name)
        assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
        return (
            f"console.log({name}.map("
            f'x => {print_type(type_.encapsulated, "x")}).join(" "));'
        )
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return 'console.log([{}].join(" "));'.format(
        ", ".join(
            print_type(i.type, f"{name}.{var_name(i.name)}") for i in struct.fields
        )
    )


def print_lines(
    input_data: Input,
    name: str,
    type_: Type,
    indent_lvl: int,
    style: FormatStyle = FormatStyle.DEFAULT,
) -> List[str]:
    """Print the content of a var that holds in one or more lines"""
    indent = "    " * indent_lvl
    if type_.fits_in_one_line(input_data.structs, style):
        return [indent + print_line(name, type_, input_data)]
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        inner = name.replace(".", "_") + "_elem"
        return (
            [indent + "{}.forEach(function({}) {{".format(name, inner)]
            + print_lines(input_data, inner, type_.encapsulated, indent_lvl + 1)
            + [indent + "});"]
        )
    assert type_.main == TypeEnum.STRUCT
    lines = []
    for i in input_data.get_struct(type_.struct_name).fields:
        lines.extend(
            print_lines(
                input_data, "{}.{}".format(name, var_name(i.name)), i.type, indent_lvl
            )
        )
    return lines


def call(input_data: Input, reprint: bool) -> List[str]:
    """Declare the function that takes all inputs in arguments"""
    out = (
        ["/**"]
        + [
            " * @param {{{}}} {} {}".format(
                type_str(arg.type, input_data), var_name(arg.name), arg.comment
            )
            for arg in input_data.input
        ]
        + [" * @returns {void}", " */"]
    )
    out.append(
        "function {}({}) {{".format(
            var_name(input_data.name),
            ", ".join(var_name(i.name) for i in input_data.input),
        )
    )
    if reprint:
        for variables in input_data.get_all_vars():
            if len(variables) == 1:
                var = variables[0]
                out.extend(
                    print_lines(
                        input_data, var_name(var.name), var.type, 1, var.format_style
                    )
                )
            else:
                out.append(
                    INDENTATION
                    + "console.log("
                    + ", ".join(var_name(i.name) for i in variables)
                    + ");"
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


def gen_javascript(input_data: Input, reprint: bool = False) -> str:
    """Generate a Javascript code to parse input"""

    placeholder = "\n".join(call(input_data, reprint))
    parser = "\n".join(ParserJS(input_data).read_vars())
    args = (var_name(i.name) for i in input_data.input)
    placeholder_call = "{}({});".format(var_name(input_data.name), ", ".join(args))

    return """"use strict";

{placeholder}

function main(stdin) {{
{ind}let line = 0;

{parser}
{ind}{placeholder_call}
}}

let stdin = "";
process.stdin.on("data", data => stdin += data.toString())
             .on("end", () => main(stdin.split("\\n")));
""".format(
        ind=INDENTATION,
        placeholder=placeholder,
        parser=parser,
        placeholder_call=placeholder_call,
    )


KEYWORDS = [
    "abstract",
    "arguments",
    "await",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "double",
    "else",
    "enum",
    "eval",
    "export",
    "extends",
    "false",
    "final",
    "finally",
    "float",
    "for",
    "function",
    "goto",
    "if",
    "implements",
    "import",
    "in",
    "instanceof",
    "int",
    "interface",
    "let",
    "long",
    "native",
    "new",
    "null",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "static",
    "super",
    "switch",
    "synchronized",
    "this",
    "throw",
    "throws",
    "transient",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "volatile",
    "while",
    "with",
    "yield",
]
