# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2025 Sacha Delanoue
"""Generate a C# parser"""

import textwrap
from iorgen.types import FormatStyle, Input, Type, TypeEnum
from iorgen.utils import camel_case, pascal_case, IteratorName, WordsName

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for C#"""
    candidate = camel_case(name)
    if candidate in KEYWORDS:
        return "@" + candidate
    return candidate


def pascal_name(name: str) -> str:
    """Transform a method, or class name into a valid one for C#"""
    candidate = pascal_case(name)
    if candidate in (
        "Array",
        "Console",
        "CultureInfo",
        "Main",
        "Program",
        "String",
        "System",
    ):
        return candidate + "_"
    return candidate


def type_str(type_: Type) -> str:
    """Return the C# name for a type"""
    if type_.main == TypeEnum.INT:
        return "int"
    if type_.main == TypeEnum.FLOAT:
        return "double"
    if type_.main == TypeEnum.CHAR:
        return "char"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.STRUCT:
        return pascal_name(type_.struct_name)
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated
    return type_str(type_.encapsulated) + "[]"


def parse_type(type_: Type, name: str) -> str:
    """Return C# code to parse a type from a string"""
    return {
        TypeEnum.INT: "int.Parse({})",
        TypeEnum.FLOAT: "double.Parse({}, CultureInfo.InvariantCulture)",
        TypeEnum.CHAR: "{}[0]",
        TypeEnum.STR: "{}",
    }[type_.main].format(name)


class ParserCS:
    """Create the C# code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        existing_names = [var.name for var in input_data.input]
        self.iterator = IteratorName(existing_names)
        self.words = WordsName(existing_names, cs_mode=True)

    def read_line(
        self, decl: bool, name: str, type_: Type, indent_lvl: int
    ) -> list[str]:
        """Read an entire line and store it into the right place(s)"""
        assert type_.fits_in_one_line(self.input.structs)
        indent = INDENTATION * indent_lvl
        type_decl = (type_str(type_) + " ") if decl else ""
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            s_name = pascal_name(struct.name)
            words = self.words.next_name()
            fields = ", ".join(
                f"{var_name(f.name)} = {parse_type(f.type, f'{words}[{i}]')}"
                for i, f in enumerate(struct.fields)
            )
            return [
                f"{indent}string[] {words} = Console.ReadLine().Split(' ');",
                f"{indent}{type_decl}{name} = new {s_name} {{{fields}}};",
            ]
        command = ""
        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
            command = parse_type(type_, "Console.ReadLine()")
        else:
            assert type_.main == TypeEnum.LIST
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                command = "Console.ReadLine().ToCharArray()"
            else:
                assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
                split_option = (
                    "new char[] {' '}, StringSplitOptions.RemoveEmptyEntries"
                    if type_.can_be_empty
                    else "' '"
                )
                parse = (
                    "int.Parse"
                    if type_.encapsulated.main == TypeEnum.INT
                    else "x => double.Parse(x, CultureInfo.InvariantCulture)"
                )
                command = (
                    "Array.ConvertAll(Console.ReadLine()"
                    f".Split({split_option}), {parse})"
                )

        assert command
        return [f"{indent}{type_decl}{name} = {command};"]

    def read_lines(
        self,
        decl: bool,
        name: str,
        type_: Type,
        size: str,
        indent_lvl: int,
        style: FormatStyle = FormatStyle.DEFAULT,
    ) -> list[str]:
        # pylint: disable=too-many-arguments
        # pylint: disable=too-many-positional-arguments
        # pylint: disable=too-many-locals
        """Read one or several lines and store them into the right place(s)"""
        if type_.fits_in_one_line(self.input.structs, style):
            return self.read_line(decl, name, type_, indent_lvl)
        indent = INDENTATION * indent_lvl
        if type_.main == TypeEnum.STRUCT:
            lines = []
            if decl:
                lines.append(f"{indent}{pascal_name(type_.struct_name)} {name};")
            struct = self.input.get_struct(type_.struct_name)
            for f_name, f_type, f_size in struct.fields_name_type_size(
                f"{name}.{{}}", var_name
            ):
                lines.extend(self.read_lines(False, f_name, f_type, f_size, indent_lvl))
            return lines
        assert type_.main == TypeEnum.LIST
        assert type_.encapsulated is not None
        far_inner_type = type_.encapsulated
        list_suffix = ""
        while far_inner_type.main == TypeEnum.LIST:
            assert far_inner_type.encapsulated is not None
            far_inner_type = far_inner_type.encapsulated
            list_suffix += "[]"
        lines = [
            "{}{}{} = new {}[{}]{};".format(
                indent,
                (type_str(type_) + " ") if decl else "",
                name,
                type_str(far_inner_type),
                size,
                list_suffix,
            )
        ]
        index = self.iterator.new_it()
        self.words.push_scope()
        lines.append(
            "{0}for (int {1} = 0; {1} < {2}; ++{1})".format(indent, index, size)
        )
        lines.append(indent + "{")
        lines.extend(
            self.read_lines(
                False,
                f"{name}[{index}]",
                type_.encapsulated,
                var_name(type_.encapsulated.size),
                indent_lvl + 1,
            )
        )
        self.words.pop_scope()
        self.iterator.pop_it()
        return lines + [indent + "}"]

    def call(self, reprint: bool) -> list[str]:
        """Declare and call the function take all inputs in arguments"""
        lines = []
        arguments = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            lines.append(INDENTATION + f"/// \\param {arg_name} {arg.comment}")
            arguments.append(f"{type_str(arg.type)} {arg_name}")
        lines.append(
            "{0}static void {1}({2})\n{0}{{".format(
                INDENTATION, pascal_name(self.input.name), ", ".join(arguments)
            )
        )
        if reprint:
            for variables in self.input.get_all_vars():
                if len(variables) == 1:
                    var = variables[0]
                    lines.extend(
                        self.print_lines(
                            var_name(var.name), var.type, 2, var.format_style
                        )
                    )
                else:
                    fmt = " ".join(f"{{{i}}}" for i in range(len(variables)))
                    lines.append(
                        INDENTATION * 2
                        + f'Console.WriteLine("{fmt}", '
                        + f"{', '.join(var_name(i.name) for i in variables)});"
                    )
        else:
            lines.extend(
                [
                    2 * INDENTATION + i
                    for i in textwrap.wrap(
                        "/* TODO " + self.input.output + " */",
                        79 - 2 * len(INDENTATION),
                    )
                ]
            )
        return lines + [INDENTATION + "}"]

    def print_line(self, name: str, type_: Type) -> str:
        """Print the content of a var that holds in one line"""
        assert type_.fits_in_one_line(self.input.structs)

        def print_type(name: str, type_: Type) -> str:
            if type_.main == TypeEnum.FLOAT:
                return f'String.Format(CultureInfo.InvariantCulture, "{{0:g}}", {name})'
            return name

        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
            return f"Console.WriteLine({print_type(name, type_)});"
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return f"Console.WriteLine(new string({name}));"
            if type_.encapsulated.main == TypeEnum.INT:
                return f'Console.WriteLine(String.Join(" ", {name}));'
            assert type_.encapsulated.main == TypeEnum.FLOAT
            return (
                'Console.WriteLine(String.Join(" ", Array.ConvertAll('
                + f'{name}, x => {print_type("x", type_.encapsulated)})));'
            )
        assert type_.main == TypeEnum.STRUCT
        fields = self.input.get_struct(type_.struct_name).fields
        return 'Console.WriteLine("{}", {});'.format(
            " ".join(f"{{{i}}}" for i in range(len(fields))),
            ", ".join(print_type(f"{name}.{var_name(f.name)}", f.type) for f in fields),
        )

    def print_lines(
        self,
        name: str,
        type_: Type,
        indent_lvl: int,
        style: FormatStyle = FormatStyle.DEFAULT,
    ) -> list[str]:
        """Print the content of a var that holds in one or more lines"""
        if type_.fits_in_one_line(self.input.structs, style):
            return [INDENTATION * indent_lvl + self.print_line(name, type_)]
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            lines = []
            for field in struct.fields:
                lines.extend(
                    self.print_lines(
                        f"{name}.{var_name(field.name)}",
                        field.type,
                        indent_lvl,
                    )
                )
            return lines
        assert type_.main == TypeEnum.LIST
        assert type_.encapsulated is not None
        index = self.iterator.new_it()
        self.words.push_scope()
        lines = [
            "{}foreach ({} {} in {})".format(
                INDENTATION * indent_lvl, type_str(type_.encapsulated), index, name
            )
        ]
        lines.append(INDENTATION * indent_lvl + "{")
        lines.extend(self.print_lines(index, type_.encapsulated, indent_lvl + 1))
        lines.append(INDENTATION * indent_lvl + "}")
        self.words.pop_scope()
        self.iterator.pop_it()
        return lines

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        output = "using System;\n\n"
        if self.input.contains_float():
            output = "using System;\nusing System.Globalization;\n\n"
        for struct in self.input.structs:
            output += f"/// {struct.comment}\n"
            output += f"struct {pascal_name(struct.name)}\n{{\n"
            for field in struct.fields:
                output += INDENTATION + "public {} {}; //!< {}\n".format(
                    type_str(field.type), var_name(field.name), field.comment
                )
            output += "}\n\n"
        output += "class Program\n{\n"
        output += "\n".join(self.call(reprint)) + "\n"
        output += "\n{0}static void Main()\n{0}{{\n".format(INDENTATION)
        for variables in self.input.get_all_vars():
            if len(variables) == 1:
                var = variables[0]
                for line in self.read_lines(
                    True,
                    var_name(var.name),
                    var.type,
                    var_name(var.type.size),
                    2,
                    var.format_style,
                ):
                    output += line + "\n"
            else:
                words = self.words.next_name()
                output += (
                    f"{INDENTATION * 2}string[] {words}"
                    " = Console.ReadLine().Split(' ');\n"
                )
                for i, var in enumerate(variables):
                    assert var.type.main == TypeEnum.INT
                    output += (
                        INDENTATION * 2
                        + f"int {var_name(var.name)} = int.Parse({words}[{i}]);\n"
                    )
        args = (var_name(var.name) for var in self.input.input)
        output += "\n{}{}({});\n".format(
            INDENTATION * 2, pascal_name(self.input.name), ", ".join(args)
        )
        output += INDENTATION + "}\n}\n"
        return output


def gen_csharp(input_data: Input, reprint: bool = False) -> str:
    """Generate a C# code to parse input"""
    return ParserCS(input_data).content(reprint)


KEYWORDS = [
    "abstract",
    "as",
    "base",
    "bool",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "checked",
    "class",
    "const",
    "continue",
    "decimal",
    "default",
    "delegate",
    "do",
    "double",
    "else",
    "enum",
    "event",
    "explicit",
    "extern",
    "false",
    "finally",
    "fixed",
    "float",
    "for",
    "foreach",
    "goto",
    "if",
    "implicit",
    "in",
    "int",
    "interface",
    "internal",
    "is",
    "lock",
    "long",
    "namespace",
    "new",
    "null",
    "object",
    "operator",
    "out",
    "override",
    "params",
    "private",
    "protected",
    "public",
    "readonly",
    "ref",
    "return",
    "sbyte",
    "sealed",
    "short",
    "sizeof",
    "stackalloc",
    "static",
    "string",
    "struct",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "uint",
    "ulong",
    "unchecked",
    "unsafe",
    "ushort",
    "using",
    "using static",
    "virtual",
    "void",
    "volatile",
    "while",
]
