# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a Pascal parser"""

import textwrap
from typing import List, Set
from iorgen.types import Input, Type, TypeEnum, Variable
from iorgen.utils import pascal_case, IteratorName

KEYWORDS = [
    "absolute", "abstract", "alias", "and", "array", "asm", "assembler",
    "begin", "break", "case", "cdecl", "char", "const", "constructor",
    "continue", "default", "destructor", "dispose", "div", "do", "downto",
    "else", "end", "exit", "export", "external", "false", "far", "file", "for",
    "forward", "function", "goto", "if", "implementation", "in", "index",
    "inherited", "inline", "integer", "interface", "label", "mod", "name",
    "near", "new", "nil", "not", "object", "of", "on", "operator", "or",
    "override", "packed", "pascal", "popstack", "private", "procedure",
    "program", "protected", "public", "published", "read", "readln", "record",
    "register", "repeat", "saveregisters", "self", "set", "setlength", "shl",
    "shr", "stdcall", "string", "system", "then", "to", "true", "type", "unit",
    "until", "uses", "var", "virtual", "while", "with", "write", "writeln",
    "xor"
]

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Pascal"""
    candidate = pascal_case(name)
    if candidate.lower() in KEYWORDS:
        return candidate + "_"
    return candidate


def type_str(var: Variable, decl: bool = False) -> str:
    """Return the Pascal name for a type"""
    # pylint: disable=too-many-return-statements
    if var.type.main == TypeEnum.INT:
        return "longint"
    if var.type.main == TypeEnum.CHAR:
        return "char"
    if var.type.main == TypeEnum.STR:
        return "string"
    if var.type.main == TypeEnum.STRUCT:
        return var_name(var.type.struct_name)
    if var.type.main == TypeEnum.LIST:
        assert var.type.encapsulated
        if var.type.encapsulated.main == TypeEnum.CHAR:
            return "string"
        if decl:
            return "array of " + type_str(
                Variable("", "", var.type.encapsulated), True)
        return "T_" + var_name(var.name)
    assert False
    return ""


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
                out.append(INDENTATION + "T_{} = {};".format(
                    var_name(var.name), type_str(var, True)))
    return out


class ParserPascal():
    """Create the Pascal code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.includes = set()  # type: Set[str]
        self.main = []  # type: List[str]
        self.method = []  # type: List[str]
        self.iterator = IteratorName([var.name for var in input_data.input])
        self.local_integers = set()  # type: Set[str]
        self.local_char = False

    def read_line(self, name: str, type_: Type, size: str,
                  indent_lvl: int) -> None:
        """Read an entire line and store it into the right place(s)"""
        assert type_.fits_in_one_line(self.input.structs)
        indent = INDENTATION * indent_lvl
        if type_.main == TypeEnum.INT:
            self.main.append(indent + "readln({});".format(name))
        elif type_.main == TypeEnum.CHAR:
            self.main.append(indent + "readln({});".format(name))
        elif type_.main == TypeEnum.STR:
            self.main.append(indent + "readln({});".format(name))
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                self.main.append(indent + "readln({});".format(name))
            elif type_.encapsulated.main == TypeEnum.INT:
                index = self.iterator.new_it()
                self.local_integers.add(index)
                self.main.append(
                    indent + "for {} := 0 to {} - 1 do".format(index, size))
                self.main.append(INDENTATION + indent +
                                 'read({}[{}]);'.format(name, index))
                self.main.append(indent + "readln();")
                self.iterator.pop_it()
            else:
                assert False
        elif type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            args = []
            for i, field in enumerate(struct.fields):
                if i != 0 and field.type.main == TypeEnum.CHAR:
                    args.append("_")
                    self.local_char = True
                args.append(name + "." + var_name(field.name))
            self.main.append(indent + 'readln({});'.format(", ".join(args)))
        else:
            assert False

    def read_lines(self,
                   name: str,
                   type_: Type,
                   size: str,
                   indent_lvl: int = 0) -> None:
        """Read one or several lines and store them into the right place(s)"""
        if type_.fits_in_one_line(self.input.structs):
            self.read_line(name, type_, size, indent_lvl)
        else:
            if type_.main == TypeEnum.STRUCT:
                struct = self.input.get_struct(type_.struct_name)
                for f_name, f_type, f_size in struct.fields_name_type_size(
                        "{}.{{}}".format(name), var_name):
                    self.main.extend([
                        INDENTATION * indent_lvl + i
                        for i in init_list(f_name, f_type, f_size)
                    ])
                    self.read_lines(f_name, f_type, f_size, indent_lvl)
            elif type_.main == TypeEnum.LIST:
                assert type_.encapsulated is not None
                index = self.iterator.new_it()
                self.local_integers.add(index)
                self.main.append("{}for {} := 0 to {} - 1 do".format(
                    INDENTATION * indent_lvl, index, size))
                self.main.append(INDENTATION * indent_lvl + "begin")
                self.read_lines(
                    "{}[{}]".format(name, index), type_.encapsulated,
                    var_name(type_.encapsulated.size), indent_lvl + 1)
                self.main.append(INDENTATION * indent_lvl + "end;")
                self.iterator.pop_it()
            else:
                assert False

    def call(self, reprint: bool) -> None:
        """Declare and call the function take all inputs in arguments"""
        name = var_name(self.input.name)
        arguments = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            self.method.append("{{ @param {} {} }}".format(
                arg_name, arg.comment))
            const = "" if arg.type.main in (TypeEnum.INT,
                                            TypeEnum.CHAR) else "const "
            arguments.append("{}{}: {}".format(const, arg_name, type_str(arg)))
        self.method.append("procedure {}({});".format(name,
                                                      "; ".join(arguments)))
        if reprint and self.local_integers:
            self.method.append("var")
            self.method.append(INDENTATION + "{}: longint;".format(", ".join(
                sorted(self.local_integers))))
        self.method.append("begin")
        if reprint:
            for var in self.input.input:
                self.print_lines(
                    var_name(var.name), var.type, var_name(var.type.size), 1)
        else:
            self.method.extend(
                textwrap.wrap(
                    self.input.output + " *}",
                    79,
                    initial_indent=INDENTATION + "{* " + "TODO ",
                    subsequent_indent=INDENTATION))
        self.method.append("end;")

    def print_line(self, name: str, type_: Type, size: str,
                   indent_lvl: int) -> None:
        """Print the content of a var that holds in one line"""
        assert type_.fits_in_one_line(self.input.structs)
        indent = INDENTATION * indent_lvl
        if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
            self.method.append(indent + 'writeln({});'.format(name))
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                self.method.append(indent + 'writeln({});'.format(name))
            else:
                index = self.iterator.new_it()
                self.method.append(
                    indent + "for {} := 0 to {} - 2 do".format(index, size))
                self.method.append(indent + INDENTATION +
                                   "write({}[{}], ' ');".format(name, index))
                try:
                    size_int = int(size)
                    if size_int > 0:
                        self.method.append(indent + "write({}[{}]);".format(
                            name, size_int - 1))
                except ValueError:
                    self.method.append(indent +
                                       "if ({0} > 0) then write({1}[{0} - 1]);"
                                       .format(size, name))
                self.method.append(indent + "writeln();")
                self.iterator.pop_it()
        elif type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            args = ["' '"] * (2 * len(struct.fields) - 1)
            args[::2] = [name + "." + var_name(i.name) for i in struct.fields]
            self.method.append(indent + 'writeln({});'.format(", ".join(args)))
        else:
            assert False

    def print_lines(self,
                    name: str,
                    type_: Type,
                    size: str,
                    indent_lvl: int = 0) -> None:
        """Print the content of a var that holds in one or more lines"""
        if type_.fits_in_one_line(self.input.structs):
            self.print_line(name, type_, size, indent_lvl)
        else:
            if type_.main == TypeEnum.STRUCT:
                struct = self.input.get_struct(type_.struct_name)
                struct = self.input.get_struct(type_.struct_name)
                for f_name, f_type, f_size in struct.fields_name_type_size(
                        "{}.{{}}".format(name), var_name):
                    self.print_lines(f_name, f_type, f_size, indent_lvl)
            elif type_.main == TypeEnum.LIST:
                assert type_.encapsulated is not None
                index = self.iterator.new_it()
                self.method.append("{}for {} := 0 to {} - 1 do".format(
                    INDENTATION * indent_lvl, index, size))
                self.method.append(INDENTATION * indent_lvl + "begin")
                self.print_lines(
                    "{}[{}]".format(name, index), type_.encapsulated,
                    var_name(type_.encapsulated.size), indent_lvl + 1)
                self.method.append(INDENTATION * indent_lvl + "end;")
                self.iterator.pop_it()
            else:
                assert False

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        for var in self.input.input:
            self.main.extend(init_list(var_name(var.name), var.type))
            self.read_lines(
                var_name(var.name), var.type, var_name(var.type.size))
        self.call(reprint)
        output = "program {};\n\n".format(var_name(self.input.name))
        types = decl_types(self.input.input)
        if self.input.structs or types:
            output += "type\n"
        for struct in self.input.structs:
            output += INDENTATION + "{{ {} }}\n".format(struct.comment)
            output += INDENTATION + "{} = record\n".format(
                var_name(struct.name))
            for field in struct.fields:
                output += 2 * INDENTATION + "{}: {}; {{ {} }}\n".format(
                    var_name(field.name), type_str(field, True), field.comment)
            output += INDENTATION + "end;\n\n"
        if types:
            output += "\n".join(types) + "\n\n"
        for line in self.method:
            output += line + "\n"
        if self.method:
            output += "\n"
        output += "var\n"
        for var in self.input.input:
            output += INDENTATION + "{}: {}; {{ {} }}\n".format(
                var_name(var.name), type_str(var), var.comment)
        if self.local_integers:
            output += INDENTATION + "{}: longint;\n".format(", ".join(
                sorted(self.local_integers)))
        if self.local_char:
            output += INDENTATION + "_: char;\n"

        output += "begin\n"
        for line in self.main:
            output += INDENTATION + line + "\n"
        output += INDENTATION + "{}({});\n".format(
            var_name(self.input.name), ", ".join(
                [var_name(i.name) for i in self.input.input]))
        output += "end.\n"
        return output


def gen_pascal(input_data: Input, reprint: bool = False) -> str:
    """Generate a Pascal code to parse input"""
    return ParserPascal(input_data).content(reprint)
