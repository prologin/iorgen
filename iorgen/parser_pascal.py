# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
"""Generate a Pascal parser"""

import textwrap
from typing import List, Set
from iorgen.types import FormatStyle, Input, Type, TypeEnum, Variable
from iorgen.utils import pascal_case, IteratorName


INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Pascal"""
    candidate = pascal_case(name)
    if candidate.lower() in KEYWORDS or candidate.lower() == "sysutils":
        return candidate + "_"
    return candidate


def type_str(var: Variable, decl: bool = False) -> str:
    """Return the Pascal name for a type"""
    # pylint: disable=too-many-return-statements
    if var.type.main == TypeEnum.INT:
        return "longint"
    if var.type.main == TypeEnum.FLOAT:
        return "double"
    if var.type.main == TypeEnum.CHAR:
        return "char"
    if var.type.main == TypeEnum.STR:
        return "AnsiString"
    if var.type.main == TypeEnum.STRUCT:
        return var_name(var.type.struct_name)
    assert var.type.main == TypeEnum.LIST
    assert var.type.encapsulated
    if var.type.encapsulated.main == TypeEnum.CHAR:
        return "AnsiString"
    if decl:
        return "array of " + type_str(Variable("", "", var.type.encapsulated), True)
    return "T_" + var_name(var.name)


def init_list(name: str, type_: Type, size: str = "") -> List[str]:
    """Initialize a (multidimentional) array by calling setLength"""
    rec = type_
    sizes = []
    while rec.main == TypeEnum.LIST:
        inner = rec.encapsulated
        assert inner is not None
        if inner.main == TypeEnum.CHAR:
            break
        sizes.append(var_name(rec.size))
        rec = inner
    if not sizes:
        return []
    if size:
        sizes[0] = size
    return ["setLength({}, {});".format(name, ", ".join(sizes))]


def decl_types(variables: List[Variable]) -> List[str]:
    """Declare the types of arary"""
    out = []
    for var in variables:
        if var.type.main == TypeEnum.LIST:
            assert var.type.encapsulated
            if var.type.encapsulated.main != TypeEnum.CHAR:
                out.append(
                    INDENTATION
                    + "T_{} = {};".format(var_name(var.name), type_str(var, True))
                )
    return out


class ParserPascal:
    """Create the Pascal code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.includes = set()  # type: Set[str]
        self.main = []  # type: List[str]
        self.iterator = IteratorName([var.name for var in input_data.input])
        self.local_integers = set()  # type: Set[str]
        self.local_char = False
        self.indent_lvl = 0

    def read_line(self, name: str, type_: Type, size: str) -> None:
        """Read an entire line and store it into the right place(s)"""
        assert type_.fits_in_one_line(self.input.structs)
        indent = INDENTATION * self.indent_lvl
        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
            self.main.append(f"{indent}readln({name});")
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                self.main.append(indent + "readln({});".format(name))
            else:
                assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
                index = self.iterator.new_it()
                self.local_integers.add(index)
                self.main.append(
                    indent + "for {} := 0 to {} - 1 do".format(index, size)
                )
                self.main.append(
                    INDENTATION + indent + "read({}[{}]);".format(name, index)
                )
                self.main.append(indent + "readln();")
                self.iterator.pop_it()
        else:
            assert type_.main == TypeEnum.STRUCT
            struct = self.input.get_struct(type_.struct_name)
            args = []
            for i, field in enumerate(struct.fields):
                if i != 0 and field.type.main == TypeEnum.CHAR:
                    args.append("_")
                    self.local_char = True
                args.append(name + "." + var_name(field.name))
            self.main.append(indent + "readln({});".format(", ".join(args)))

    def read_lines(
        self,
        name: str,
        type_: Type,
        size: str,
        style: FormatStyle = FormatStyle.DEFAULT,
    ) -> None:
        """Read one or several lines and store them into the right place(s)"""
        if type_.fits_in_one_line(self.input.structs, style):
            self.read_line(name, type_, size)
        else:
            if type_.main == TypeEnum.STRUCT:
                struct = self.input.get_struct(type_.struct_name)
                for f_name, f_type, f_size in struct.fields_name_type_size(
                    "{}.{{}}".format(name), var_name
                ):
                    self.main.extend(
                        [
                            INDENTATION * self.indent_lvl + i
                            for i in init_list(f_name, f_type, f_size)
                        ]
                    )
                    self.read_lines(f_name, f_type, f_size)
            else:
                assert type_.main == TypeEnum.LIST
                assert type_.encapsulated is not None
                index = self.iterator.new_it()
                self.local_integers.add(index)
                self.main.append(
                    f"{INDENTATION * self.indent_lvl}for {index} := 0 to {size} - 1 do"
                )
                self.main.append(INDENTATION * self.indent_lvl + "begin")
                self.indent_lvl += 1
                self.read_lines(
                    f"{name}[{index}]",
                    type_.encapsulated,
                    var_name(type_.encapsulated.size),
                )
                self.indent_lvl -= 1
                self.main.append(INDENTATION * self.indent_lvl + "end;")
                self.iterator.pop_it()

    def call(self, reprint: bool) -> List[str]:
        """Declare and call the function take all inputs in arguments"""
        name = var_name(self.input.name)
        arguments = []
        method = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            method.append("{{ @param {} {} }}".format(arg_name, arg.comment))
            const = (
                ""
                if arg.type.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR)
                else "const "
            )
            arguments.append("{}{}: {}".format(const, arg_name, type_str(arg)))
        method.append("procedure {}({});".format(name, "; ".join(arguments)))
        if reprint and self.local_integers:
            method.append("var")
            method.append(
                INDENTATION
                + "{}: longint;".format(", ".join(sorted(self.local_integers)))
            )
        method.append("begin")
        if reprint:
            self.indent_lvl += 1
            for var in self.input.input:
                method.extend(
                    self.print_lines(
                        var_name(var.name),
                        var.type,
                        var_name(var.type.size),
                        var.format_style,
                    )
                )
            self.indent_lvl -= 1
        else:
            method.extend(
                textwrap.wrap(
                    self.input.output + " *}",
                    79,
                    initial_indent=INDENTATION + "{* " + "TODO ",
                    subsequent_indent=INDENTATION,
                )
            )
        method.append("end;")
        return method

    def print_line(
        self, name: str, type_: Type, size: str, style: FormatStyle
    ) -> List[str]:
        """Print the content of a var that holds in one line"""

        def print_type(name: str, type_: Type) -> str:
            """Print the content of a variable (with a special case for float)"""
            if type_.main != TypeEnum.FLOAT:
                return name
            return (
                # This is an ungly one. It's the best I found to convert e-X to e-0X
                # as a one-liner. The good thing is reprint mode is not meant to be seen
                "LowerCase(StringReplace("
                f"sysutils.FloatToStr({name}),"
                f"copy(sysutils.FloatToStrF(Abs({name}), ffExponent, 0, 1), 4, 99),"
                f"copy(sysutils.FloatToStrF(Abs({name}), ffExponent, 0, 2), 4, 99),"
                "[]))"
            )

        assert type_.fits_in_one_line(self.input.structs, style)
        indent = INDENTATION * self.indent_lvl
        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
            return [
                indent
                + (
                    f"write({print_type(name, type_)}, ' ');"
                    if style == FormatStyle.NO_ENDLINE
                    else f"writeln({print_type(name, type_)});"
                )
            ]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return [indent + f"writeln({name});"]
            lines = []
            index = self.iterator.new_it()
            lines.append(indent + f"for {index} := 0 to {size} - 2 do")
            print_idx = print_type(f"{name}[{{0}}]", type_.encapsulated).format
            lines.append(f"{indent}{INDENTATION}write({print_idx(index)}, ' ');")
            try:
                size_int = int(size)
                if size_int > 0:
                    lines.append(f"{indent}write({print_idx(size_int - 1)});")
            except ValueError:
                lines.append(
                    f"{indent}if ({size} > 0) then write({print_idx(f'{size} - 1')});"
                )
            lines.append(indent + "writeln();")
            self.iterator.pop_it()
            return lines
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        args = ["' '"] * (2 * len(struct.fields) - 1)
        args[::2] = [
            print_type(f"{name}.{var_name(i.name)}", i.type) for i in struct.fields
        ]
        return [indent + f"writeln({', '.join(args)});"]

    def print_lines(
        self,
        name: str,
        type_: Type,
        size: str,
        style: FormatStyle = FormatStyle.DEFAULT,
    ) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        if type_.fits_in_one_line(self.input.structs, style):
            return self.print_line(name, type_, size, style)
        lines = []
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            struct = self.input.get_struct(type_.struct_name)
            for f_name, f_type, f_size in struct.fields_name_type_size(
                f"{name}.{{}}", var_name
            ):
                lines.extend(self.print_lines(f_name, f_type, f_size))
        else:
            assert type_.main == TypeEnum.LIST
            assert type_.encapsulated is not None
            index = self.iterator.new_it()
            lines = [
                f"{INDENTATION * self.indent_lvl}for {index} := 0 to {size} - 1 do",
                INDENTATION * self.indent_lvl + "begin",
            ]
            self.indent_lvl += 1
            lines.extend(
                self.print_lines(
                    f"{name}[{index}]",
                    type_.encapsulated,
                    var_name(type_.encapsulated.size),
                )
            )
            self.indent_lvl -= 1
            lines.append(INDENTATION * self.indent_lvl + "end;")
            self.iterator.pop_it()
        return lines

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        for variables in self.input.get_all_vars():
            if len(variables) == 1:
                var = variables[0]
                self.main.extend(init_list(var_name(var.name), var.type))
                self.read_lines(
                    var_name(var.name),
                    var.type,
                    var_name(var.type.size),
                    var.format_style,
                )
            else:
                assert all(var.type.main == TypeEnum.INT for var in variables)
                self.main.append(
                    f"readln({', '.join(var_name(i.name) for i in variables)});"
                )
        method = self.call(reprint)
        output = "program {};\n\n".format(var_name(self.input.name))
        if reprint and self.input.contains_float():
            output += "uses sysutils;\n"
        types = decl_types(self.input.input)
        if self.input.structs or types:
            output += "type\n"
        for struct in self.input.structs:
            output += INDENTATION + "{{ {} }}\n".format(struct.comment)
            output += INDENTATION + "{} = record\n".format(var_name(struct.name))
            for field in struct.fields:
                output += 2 * INDENTATION + "{}: {}; {{ {} }}\n".format(
                    var_name(field.name), type_str(field, True), field.comment
                )
            output += INDENTATION + "end;\n\n"
        if types:
            output += "\n".join(types) + "\n\n"
        for line in method:
            output += line + "\n"
        output += "\nvar\n"
        for var in self.input.input:
            output += INDENTATION + "{}: {}; {{ {} }}\n".format(
                var_name(var.name), type_str(var), var.comment
            )
        if self.local_integers:
            output += INDENTATION + "{}: longint;\n".format(
                ", ".join(sorted(self.local_integers))
            )
        if self.local_char:
            output += INDENTATION + "_: char;\n"

        output += "begin\n"
        output += "\n".join(INDENTATION + line for line in self.main) + "\n"
        output += INDENTATION + "{}({});\n".format(
            var_name(self.input.name),
            ", ".join([var_name(i.name) for i in self.input.input]),
        )
        output += "end.\n"
        return output


def gen_pascal(input_data: Input, reprint: bool = False) -> str:
    """Generate a Pascal code to parse input"""
    return ParserPascal(input_data).content(reprint)


KEYWORDS = [
    "absolute",
    "abstract",
    "alias",
    "and",
    "ansistring",
    "array",
    "asm",
    "assembler",
    "begin",
    "break",
    "case",
    "cdecl",
    "char",
    "const",
    "constructor",
    "continue",
    "default",
    "destructor",
    "dispose",
    "div",
    "do",
    "downto",
    "else",
    "end",
    "exit",
    "export",
    "external",
    "false",
    "far",
    "file",
    "for",
    "forward",
    "function",
    "goto",
    "if",
    "implementation",
    "in",
    "index",
    "inherited",
    "inline",
    "integer",
    "interface",
    "label",
    "mod",
    "name",
    "near",
    "new",
    "nil",
    "not",
    "object",
    "of",
    "on",
    "operator",
    "or",
    "override",
    "packed",
    "pascal",
    "popstack",
    "private",
    "procedure",
    "program",
    "protected",
    "public",
    "published",
    "read",
    "readln",
    "record",
    "register",
    "repeat",
    "saveregisters",
    "self",
    "set",
    "setlength",
    "shl",
    "shr",
    "stdcall",
    "string",
    "system",
    "then",
    "to",
    "true",
    "type",
    "unit",
    "until",
    "uses",
    "var",
    "virtual",
    "while",
    "with",
    "write",
    "writeln",
    "xor",
]
