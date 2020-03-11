# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2020 Sacha Delanoue
"""Generate a C# parser"""

import textwrap
from typing import List
from iorgen.types import Input, Type, TypeEnum
from iorgen.utils import camel_case, pascal_case, IteratorName, WordsName

KEYWORDS = [
    "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char",
    "checked", "class", "const", "continue", "decimal", "default", "delegate",
    "do", "double", "else", "enum", "event", "explicit", "extern", "false",
    "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit",
    "in", "int", "interface", "internal", "is", "lock", "long", "namespace",
    "new", "null", "object", "operator", "out", "override", "params",
    "private", "protected", "public", "readonly", "ref", "return", "sbyte",
    "sealed", "short", "sizeof", "stackalloc", "static", "string", "struct",
    "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong",
    "unchecked", "unsafe", "ushort", "using", "using static", "virtual",
    "void", "volatile", "while"
]

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
    if candidate in ("Main", "Program", "System", "Console", "Array",
                     "String"):
        return candidate + "_"
    return candidate


def type_str(type_: Type) -> str:
    """Return the C# name for a type"""
    if type_.main == TypeEnum.INT:
        return "int"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.CHAR:
        return "char"
    if type_.main == TypeEnum.STRUCT:
        return pascal_name(type_.struct_name)
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated
    return type_str(type_.encapsulated) + "[]"


class ParserCS():
    """Create the C# code to parse an input"""
    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        existing_names = [var.name for var in input_data.input]
        self.iterator = IteratorName(existing_names)
        self.words = WordsName(existing_names, cs_mode=True)

    def read_line(self, decl: bool, name: str, type_: Type,
                  indent_lvl: int) -> List[str]:
        """Read an entire line and store it into the right place(s)"""
        assert type_.fits_in_one_line(self.input.structs)
        indent = INDENTATION * indent_lvl
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            s_name = pascal_name(struct.name) + " "
            words = self.words.next_name()
            lines = [
                indent +
                "string[] {} = Console.ReadLine().Split(' ');".format(words)
            ]
            return lines + [
                "{}{}{} = new {}{{{}}};".format(
                    indent, s_name if decl else "", name, s_name,
                    ", ".join("{} = {}".format(
                        var_name(f.name),
                        "int.Parse({}[{}])".format(words, i) if f.type.main ==
                        TypeEnum.INT else "{}[{}][0]".format(words, i))
                              for i, f in enumerate(struct.fields)))
            ]
        type_decl = (type_str(type_) + " ") if decl else ""
        command = ""
        if type_.main == TypeEnum.INT:
            command = "int.Parse(Console.ReadLine())"
        elif type_.main == TypeEnum.CHAR:
            command = "Console.ReadLine()[0]"
        elif type_.main == TypeEnum.STR:
            command = "Console.ReadLine()"
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                command = "Console.ReadLine().ToCharArray()"
            elif type_.encapsulated.main == TypeEnum.INT:
                command = (
                    "Array.ConvertAll(Console.ReadLine().Split({}), "
                    "int.Parse)"
                ).format(
                    "new char[] {' '}, StringSplitOptions.RemoveEmptyEntries"
                    if type_.can_be_empty else "' '")

        assert command
        return ["{}{}{} = {};".format(indent, type_decl, name, command)]

    def read_lines(self, decl: bool, name: str, type_: Type, size: str,
                   indent_lvl: int) -> List[str]:
        # pylint: disable=too-many-arguments
        # pylint: disable=too-many-locals
        """Read one or several lines and store them into the right place(s)"""
        if type_.fits_in_one_line(self.input.structs):
            return self.read_line(decl, name, type_, indent_lvl)
        indent = INDENTATION * indent_lvl
        if type_.main == TypeEnum.STRUCT:
            lines = []
            if decl:
                lines.append("{}{} {};".format(indent,
                                               pascal_name(type_.struct_name),
                                               name))
            struct = self.input.get_struct(type_.struct_name)
            for f_name, f_type, f_size in struct.fields_name_type_size(
                    "{}.{{}}".format(name), var_name):
                lines.extend(
                    self.read_lines(False, f_name, f_type, f_size, indent_lvl))
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
                indent, (type_str(type_) + " ") if decl else "", name,
                type_str(far_inner_type), size, list_suffix)
        ]
        index = self.iterator.new_it()
        self.words.push_scope()
        lines.append("{0}for (int {1} = 0; {1} < {2}; ++{1})".format(
            indent, index, size))
        lines.append(indent + "{")
        lines.extend(
            self.read_lines(False, "{}[{}]".format(name, index),
                            type_.encapsulated,
                            var_name(type_.encapsulated.size), indent_lvl + 1))
        self.words.pop_scope()
        self.iterator.pop_it()
        return lines + [indent + "}"]

    def call(self, reprint: bool) -> List[str]:
        """Declare and call the function take all inputs in arguments"""
        lines = []
        arguments = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            lines.append(INDENTATION +
                         "/// \\param {} {}".format(arg_name, arg.comment))
            arguments.append("{} {}".format(type_str(arg.type), arg_name))
        lines.append("{0}static void {1}({2})\n{0}{{".format(
            INDENTATION, pascal_name(self.input.name), ", ".join(arguments)))
        if reprint:
            for var in self.input.input:
                lines.extend(self.print_lines(var_name(var.name), var.type, 2))
        else:
            lines.extend([
                2 * INDENTATION + i
                for i in textwrap.wrap("/* TODO " + self.input.output +
                                       " */", 79 - 2 * len(INDENTATION))
            ])
        return lines + [INDENTATION + "}"]

    def print_line(self, name: str, type_: Type) -> str:
        """Print the content of a var that holds in one line"""
        assert type_.fits_in_one_line(self.input.structs)
        if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
            return 'Console.WriteLine({});'.format(name)
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return 'Console.WriteLine(new string({}));'.format(name)
            assert type_.encapsulated.main == TypeEnum.INT
            return 'Console.WriteLine(String.Join(" ", {}));'.format(name)
        assert type_.main == TypeEnum.STRUCT
        fields = self.input.get_struct(type_.struct_name).fields
        return 'Console.WriteLine("{}", {});'.format(
            " ".join("{{{}}}".format(i) for i in range(len(fields))),
            ", ".join("{}.{}".format(name, var_name(f.name)) for f in fields))

    def print_lines(self, name: str, type_: Type,
                    indent_lvl: int) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        if type_.fits_in_one_line(self.input.structs):
            return [INDENTATION * indent_lvl + self.print_line(name, type_)]
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            lines = []
            for field in struct.fields:
                lines.extend(
                    self.print_lines(
                        "{}.{}".format(name, var_name(field.name)), field.type,
                        indent_lvl))
            return lines
        assert type_.main == TypeEnum.LIST
        assert type_.encapsulated is not None
        index = self.iterator.new_it()
        self.words.push_scope()
        lines = [
            "{}foreach ({} {} in {})".format(INDENTATION * indent_lvl,
                                             type_str(type_.encapsulated),
                                             index, name)
        ]
        lines.append(INDENTATION * indent_lvl + "{")
        lines.extend(
            self.print_lines(index, type_.encapsulated, indent_lvl + 1))
        lines.append(INDENTATION * indent_lvl + "}")
        self.words.pop_scope()
        self.iterator.pop_it()
        return lines

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        output = "using System;\n\n"
        for struct in self.input.structs:
            output += "/// {}\n".format(struct.comment)
            output += "struct {}\n{{\n".format(pascal_name(struct.name))
            for field in struct.fields:
                output += INDENTATION + "public {} {}; //!< {}\n".format(
                    type_str(field.type), var_name(field.name), field.comment)
            output += "}\n\n"
        output += "class Program\n{\n"
        output += "\n".join(self.call(reprint)) + "\n"
        output += "\n{0}static void Main()\n{0}{{\n".format(INDENTATION)
        for var in self.input.input:
            output += "\n".join(
                self.read_lines(True, var_name(var.name), var.type,
                                var_name(var.type.size), 2)) + "\n"
        args = (var_name(var.name) for var in self.input.input)
        output += "\n{}{}({});\n".format(INDENTATION * 2,
                                         pascal_name(self.input.name),
                                         ", ".join(args))
        output += INDENTATION + "}\n}\n"
        return output


def gen_csharp(input_data: Input, reprint: bool = False) -> str:
    """Generate a C# code to parse input"""
    return ParserCS(input_data).content(reprint)
