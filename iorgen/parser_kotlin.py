# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2023 Chloé Magliulo
# Copyright 2023 Sacha Delanoue

"""Generate a Kotlin parser"""

import textwrap
from typing import List

from iorgen.types import TypeEnum, Type, Variable, Input
from iorgen.utils import camel_case, pascal_case, WordsName

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Kotlin"""
    candidate = camel_case(name)
    if candidate in ["reader", "main"]:
        return candidate + "_"
    return f"`{candidate}`" if candidate in KEYWORDS else candidate


def class_name(name: str) -> str:
    """Transform a class name into a valid one for Kotlin"""
    candidate = pascal_case(name)
    if candidate in BUILT_IN_CLASSNAMES:
        return candidate + "_"
    return candidate


def type_str(type_: Type) -> str:
    """Return the Kotlin name for a type"""
    if type_.main == TypeEnum.INT:
        return "Int"
    if type_.main == TypeEnum.FLOAT:
        return "Double"
    if type_.main == TypeEnum.CHAR:
        return "Char"
    if type_.main == TypeEnum.STR:
        return "String"
    if type_.main == TypeEnum.STRUCT:
        return class_name(type_.struct_name)
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated

    return f"List<{type_str(type_.encapsulated)}>"


def parse_type(type_: Type, name: str) -> str:
    """Return Kotlin code to parse a type."""
    return {
        TypeEnum.INT: f"{name}.toInt()",
        TypeEnum.FLOAT: f"{name}.toDouble()",
        TypeEnum.CHAR: f"{name}[0]",
        TypeEnum.STR: name,
    }[type_.main]


class ParserKotlin:
    """Create the Kotlin code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.imports = {"java.io.BufferedReader", "java.io.InputStreamReader"}
        existing_names = [var.name for var in input_data.input] + [
            var_name(input_data.name)
        ]
        self.words = WordsName(existing_names)

    def read_line(
        self, decl: bool, name: str, type_: Type, indent_lvl: int
    ) -> List[str]:
        """Read an entire line and store it into the right place(s)"""
        assert type_.fits_in_one_line(self.input.structs)
        indent = INDENTATION * indent_lvl
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            if name:
                self.words.push_scope()
                words = self.words.next_name()
                lines = [
                    indent
                    + f'{f"val {name}: {type_str(type_)}" if decl else words}'
                    + ' = reader.readLine().split(" ").let {',
                    indent + INDENTATION + class_name(type_.struct_name) + "(",
                ]
                self.words.pop_scope()
            else:
                lines = [
                    indent + 'reader.readLine().split(" ").let {',
                    indent + INDENTATION + class_name(type_.struct_name) + "(",
                ]
            lines.extend(
                f"{indent + INDENTATION * 2}{var_name(f.name)} = "
                f"{parse_type(f.type, f'it[{i}]')},"
                for i, f in enumerate(struct.fields)
            )
            lines[-1] = lines[-1].removesuffix(",")  # no trailing comma in kotlin < 1.4

            lines.append(indent + INDENTATION + ")")
            lines.append(indent + "}")
            return lines

        command = ""
        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
            command = parse_type(type_, "reader.readLine()")
        else:
            assert type_.main == TypeEnum.LIST
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                command = "reader.readLine().toList()"
            else:
                assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
                command = (
                    'reader.readLine().split(" ").filter {{ !it.isBlank() }}'
                    ".map(String::{})".format(
                        "toInt"
                        if type_.encapsulated.main == TypeEnum.INT
                        else "toDouble"
                    )
                )

        assert command
        if name:
            decl_name = f"val {name}: {type_str(type_)}" if decl else name
            return [f"{indent}{decl_name} = {command}"]
        return [indent + command]

    def read_lines(
        self, decl: bool, var: Variable, size: str, indent_lvl: int
    ) -> List[str]:
        """Read one or several lines and store them into the right place(s)"""
        if var.fits_in_one_line(self.input.structs):
            return self.read_line(decl, var.name, var.type, indent_lvl)
        indent = INDENTATION * indent_lvl
        if var.type.main == TypeEnum.STRUCT:
            lines = []
            struct = self.input.get_struct(var.type.struct_name)
            for f_name, f_type, f_size in struct.fields_name_type_size("{}", var_name):
                lines.extend(
                    self.read_lines(
                        True, Variable(f_name, "", f_type), f_size, indent_lvl
                    )
                )

            if var.name:
                lines.append(
                    indent
                    + "{} = {}(".format(
                        f"val {var.name}: {type_str(var.type)}" if decl else var.name,
                        class_name(var.type.struct_name),
                    )
                )
            else:
                lines.append(
                    indent
                    + "{}(".format(
                        class_name(var.type.struct_name),
                    )
                )

            for f_name, f_type, f_size in struct.fields_name_type_size("{}", var_name):
                lines.append(f"{indent + INDENTATION}{f_name} = {f_name},")
            lines[-1] = lines[-1].removesuffix(",")  # no trailing comma in kotlin < 1.4

            lines.append(f"{indent})")
            return lines
        assert var.type.main == TypeEnum.LIST
        assert var.type.encapsulated is not None
        far_inner_type = var.type.encapsulated
        while far_inner_type.main == TypeEnum.LIST:
            assert far_inner_type.encapsulated is not None
            far_inner_type = far_inner_type.encapsulated

        if var.name:
            lines = [
                "{}{} = List({}) {{ _ ->".format(
                    indent,
                    f"val {var.name}: {type_str(var.type)}" if decl else var.name,
                    size,
                )
            ]
        else:
            lines = ["{}List({}) {{ _ ->".format(indent, size)]

        lines.extend(
            self.read_lines(
                True,
                Variable("", "", var.type.encapsulated),
                var_name(var.type.encapsulated.size),
                indent_lvl + 1,
            )
        )

        return lines + [indent + "}"]

    def call(self, reprint: bool) -> List[str]:
        """Declare and call the function take all inputs in arguments"""
        lines = ["/**"]
        arguments = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            lines.append(" * @param {} {}".format(arg_name, arg.comment))
            arguments.append("{}: {}".format(arg_name, type_str(arg.type)))
        lines.append(" */")
        lines.append(f"fun {var_name(self.input.name)}(")

        for arg in self.input.input:
            lines.append(f"{INDENTATION}{var_name(arg.name)}: {type_str(arg.type)},")
        lines[-1] = lines[-1].removesuffix(",")  # no trailing comma in kotlin < 1.4

        lines.append(") {")

        if reprint:
            for variables in self.input.get_all_vars():
                if len(variables) == 1:
                    var = variables[0]
                    lines.extend(
                        self.print_lines(
                            Variable(
                                var_name(var.name), "", var.type, var.format_style
                            ),
                            var_name(var.type.size),
                            1,
                        )
                    )
                else:

                    def get_print_format(printed: Variable) -> str:
                        if printed.type.main == TypeEnum.FLOAT:
                            return "${" + var_name(printed.name) + ".iorgenFormat() }"
                        return "${" + var_name(printed.name) + "}"

                    lines.append(
                        INDENTATION
                        + 'println("{}")'.format(
                            " ".join(get_print_format(i) for i in variables)
                        )
                    )
        else:
            lines.extend(
                [
                    INDENTATION + i
                    for i in textwrap.wrap(
                        "/* TODO: " + self.input.output + " */",
                    )
                ]
            )
        return lines + ["}"]

    def print_line(self, name: str, type_: Type) -> str:
        """Print the content of a var that holds in one line"""
        assert type_.fits_in_one_line(self.input.structs)
        if type_.main == TypeEnum.FLOAT:
            return f"println({name}.iorgenFormat())"
        if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
            return f"println({name})"
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return f'println({name}.joinToString(""))'
            assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
            if type_.encapsulated.main == TypeEnum.FLOAT:
                return f'println({name}.joinToString(" ") {{ it.iorgenFormat() }})'
            return f'println({name}.joinToString(" "))'
        assert type_.main == TypeEnum.STRUCT
        fields = self.input.get_struct(type_.struct_name).fields
        struct_print_patterns = []
        for field in fields:
            if field.type.main == TypeEnum.FLOAT:
                struct_print_patterns.append(
                    f"${{{name}.{var_name(field.name)}.iorgenFormat()}}"
                )
            else:
                struct_print_patterns.append(f"${{{name}.{var_name(field.name)}}}")
        return f'println("{" ".join(struct_print_patterns)}")'

    def print_lines(
        self,
        var: Variable,
        _: str,
        indent_lvl: int,
    ) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        if var.fits_in_one_line(self.input.structs):
            return [INDENTATION * indent_lvl + self.print_line(var.name, var.type)]
        if var.type.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(var.type.struct_name)
            lines = []
            for f_name, f_type, f_size in struct.fields_name_type_size(
                f"{var.name}.{{}}", var_name
            ):
                lines.extend(
                    self.print_lines(Variable(f_name, "", f_type), f_size, indent_lvl)
                )
            return lines
        assert var.type.main == TypeEnum.LIST
        assert var.type.encapsulated is not None

        inline_variable_name = self.words.next_name()

        lines = [
            f"{INDENTATION * indent_lvl}{var.name}.forEach {{ {inline_variable_name} -> "
        ]

        lines.extend(
            self.print_lines(
                Variable(inline_variable_name, "", var.type.encapsulated),
                var_name(var.type.encapsulated.size),
                indent_lvl + 1,
            )
        )
        lines.append(INDENTATION * indent_lvl + "}")
        return lines

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        output = ""
        for struct in self.input.structs:
            output += f"/**\n * {struct.comment}\n\n"
            for field in struct.fields:
                output += f" * @property {var_name(field.name)} {field.comment}\n"
            output += " */\n"
            output += f"data class {class_name(struct.name)}("
            output += ", ".join(
                f"val {var_name(field.name)}: {type_str(field.type)}"
                for field in struct.fields
            )
            output += ")\n\n"

        output += "\n".join(self.call(reprint)) + "\n\n"

        if reprint and self.input.contains_float():
            output += """
    fun Double.iorgenFormat(): String {
        val symbols =
            java.text.DecimalFormatSymbols(java.util.Locale.ROOT)
        val a = java.text.DecimalFormat("#.###############E00", symbols)
            .format(this).replace("E-", "e-").replace("E", "e+")
        val b = java.text.DecimalFormat("###############.###############",
            symbols).format(this)
        return if (a.length < b.length) a else b
    }
            """

        output += "fun main() {\n"
        output += INDENTATION + (
            "val reader = " "BufferedReader(InputStreamReader(System.`in`))\n"
        )

        for variables in self.input.get_all_vars():
            if len(variables) == 1:
                var = variables[0]
                for line in self.read_lines(
                    True,
                    Variable(var_name(var.name), "", var.type, var.format_style),
                    var_name(var.type.size),
                    1,
                ):
                    output += line + "\n"
            else:
                words = self.words.next_name()
                output += (
                    f"{INDENTATION}val {words}" + ' = reader.readLine().split(" ")\n'
                )
                for i, var in enumerate(variables):
                    assert var.type.main == TypeEnum.INT
                    output += (
                        INDENTATION
                        + f"val {var_name(var.name)} = {words}[{i}].toInt()\n"
                    )

        args = (var_name(var.name) for var in self.input.input)
        output += "\n{}{}({})\n".format(
            INDENTATION, var_name(self.input.name), ", ".join(args)
        )
        output += "}\n\n"
        output += "main()\n"
        return "".join(f"import {i}\n" for i in sorted(self.imports)) + "\n" + output


def gen_kotlin(input_data: Input, reprint: bool = False) -> str:
    """Generate a Kotlin code to parse input"""
    return ParserKotlin(input_data).content(reprint)


BUILT_IN_CLASSNAMES = [
    "Any",
    "Nothing",
    "Unit",
    "Boolean",
    "Byte",
    "Short",
    "Int",
    "Long",
    "Float",
    "Double",
    "Char",
    "String",
    "Array",
    "List",
    "System",
    "Nothing",
    "Object",
    "Exception",
    "BufferedReader",
    "InputStreamReader",
]

KEYWORDS = [
    "abstract",
    "as",
    "actual",
    "expected",
    "when",
    "annotation",
    "class",
    "fun",
    "var",
    "val",
    "const",
    "for",
    "object",
    "if",
    "else",
    "do",
    "while",
    "break",
    "return",
    "continue",
    "throw",
    "try",
    "catch",
    "finally",
    "this",
    "super",
    "is",
    "in",
    "package",
    "interface",
    "import",
    "typealias",
    "constructor",
    "init",
    "as?",
    "!in",
    "!is",
    "null",
    "typeof",
    "by",
    "delegate",
    "dynamic",
    "inline",
    "crossinline",
    "infix",
    "operator",
    "external",
    "inner",
    "internal",
    "private",
    "public",
    "protected",
    "lateinit",
    "out",
    "override",
    "reified",
    "sealed",
    "suspend",
    "tailrec",
    "vararg",
    "open",
    "it",
    "data",
    "value",
    "main",
]
