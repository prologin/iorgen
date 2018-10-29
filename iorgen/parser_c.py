# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a C++ parser"""

import textwrap
from typing import List, Set  # pylint: disable=unused-import
from iorgen.types import Input, Type, TypeEnum, Variable
from iorgen.utils import snake_case

KEYWORDS = [
    "auto", "break", "case", "char", "const", "continue", "default", "do",
    "double", "else", "enum", "extern", "float", "for", "goto", "if", "int",
    "long", "register", "return", "short", "signed", "sizeof", "static",
    "struct", "switch", "typedef", "union", "unsigned", "void", "volatile",
    "while"
]


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for C"""
    candidate = snake_case(name)
    if candidate in KEYWORDS:
        return candidate + "_"
    if candidate.endswith("_index"):  # could cause conflict for iterations
        return candidate + "_"
    return candidate


def struct_name(name: str) -> str:
    """Transform a struct name into a valid one for C"""
    candidate = snake_case(name)  # We make want an other style
    if candidate in KEYWORDS:
        return candidate + "_"
    return candidate


def index_name(name: str) -> str:
    """Return a name for an index for a given variable name"""
    if '[' in name:
        return name[name.find('[') + 1:-1] + "_index"
    return name + "_index"


class ParserC():
    """Create the C code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.includes = set()  # type: Set[str]
        self.main = []  # type: List[str]
        self.method = []  # type: List[str]
        self.call_site = []  # type: List[str]
        self.garbage_ws = False

        self.indentation = 4

    def type_str(self, type_: Type) -> str:
        """Return the C name for a type"""
        if type_.main == TypeEnum.INT:
            return "int"
        if type_.main == TypeEnum.STR:
            return "char*"
        if type_.main == TypeEnum.CHAR:
            return "char"
        if type_.main == TypeEnum.STRUCT:
            return "struct " + struct_name(type_.struct_name)
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated
            return self.type_str(type_.encapsulated) + "*"
        assert False
        return ""

    def read_line(self, name: str, type_: Type, indent_lvl: int) -> None:
        """Read an entire line and store it into the right place(s)"""
        assert type_.fits_it_one_line(self.input.structs)
        indent = ' ' * (self.indentation * indent_lvl)
        self.includes.add("stdio.h")
        if type_.main == TypeEnum.INT:
            self.main.append(indent + 'scanf("%d\\n", &{});'.format(name))
        elif type_.main == TypeEnum.CHAR:
            if indent_lvl != 0:
                self.main.append(indent + "{} = getchar();".format(name))
            self.main.append(indent + "getchar(); // \\n")
        elif type_.main == TypeEnum.STR:
            self.main.append(indent + "fgets({}, {} + 1, stdin);".format(
                name, var_name(type_.size)))
            self.main.append(indent + "getchar(); // \\n")
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                self.main.append(indent + "fgets({}, {} + 1, stdin);".format(
                    name, var_name(type_.size)))
                self.main.append(indent + "getchar(); // \\n")
            elif type_.encapsulated.main == TypeEnum.INT:
                index = index_name(name)
                self.main.append(indent + "for (int {0} = 0; {0} < {1}; ++{0})"
                                 .format(index, var_name(type_.size)))
                self.main.append(' ' * self.indentation + indent +
                                 'scanf("%d ", &{}[{}]);'.format(name, index))
            else:
                assert False
        elif type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            self.main.append(indent + 'scanf("{}\\n", {});'.format(
                " ".join("%c" if i[1].main == TypeEnum.CHAR else "%d"
                         for i in struct.fields), ", ".join(
                             "&" + name + "." + var_name(i[0])
                             for i in struct.fields)))
        else:
            assert False
        self.garbage_ws = type_.main != TypeEnum.STR

    def read_lines(self, name: str, type_: Type, indent_lvl: int = 0) -> None:
        """Read one or several lines and store them into the right place(s)"""
        if type_.main == TypeEnum.LIST and indent_lvl != 0:
            self.includes.add("stdlib.h")
            self.main.append("{}{} = calloc({}{}, sizeof({}));".format(
                " " * self.indentation * indent_lvl, name, type_.size,
                " + 1" if type_.main == TypeEnum.CHAR else "",
                self.type_str(type_)))
        elif type_.main == TypeEnum.STR and indent_lvl != 0:
            self.includes.add("stdlib.h")
            self.main.append("{}{} = calloc({} + 1, sizeof(char));".format(
                " " * self.indentation * indent_lvl, name, type_.size))
        if type_.fits_it_one_line(self.input.structs):
            self.read_line(name, type_, indent_lvl)
        else:
            if type_.main == TypeEnum.STRUCT:
                for field in self.input.get_struct(type_.struct_name).fields:
                    self.read_lines("{}.{}".format(name, field[0]), field[1],
                                    indent_lvl)
            elif type_.main == TypeEnum.LIST:
                assert type_.encapsulated is not None
                index = index_name(name)
                self.main.append(
                    "{0}for (int {1} = 0; {1} < {2}; ++{1}) {{".format(
                        " " * self.indentation * indent_lvl, index,
                        var_name(type_.size)))
                self.read_lines("{}[{}]".format(name, index),
                                type_.encapsulated, indent_lvl + 1)
                self.main.append(" " * self.indentation * indent_lvl + "}")
            else:
                assert False

    def read_var(self, var: Variable) -> None:
        """Read a variable"""
        init = ""
        if var.type.main == TypeEnum.CHAR:
            self.includes.add("stdio.h")
            init = " = getchar()"
        elif var.type.main == TypeEnum.LIST:
            assert var.type.encapsulated is not None
            self.includes.add("stdlib.h")
            init = " = calloc({}{}, sizeof({}))".format(
                var_name(var.type.size),
                " + 1" if var.type.encapsulated.main == TypeEnum.CHAR else "",
                self.type_str(var.type.encapsulated))
        self.main.append("{} {}{}; ///< {}".format(
            self.type_str(var.type), var_name(var.name), init, var.comment))
        self.read_lines(var_name(var.name), var.type)

    def call(self, reprint: bool) -> None:
        """Declare and call the function take all inputs in arguments"""
        name = var_name(self.input.name)
        arguments = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            self.method.append("/// \\param {} {}".format(
                arg_name, arg.comment))
            arguments.append("{} {}".format(self.type_str(arg.type), arg_name))
        self.method.append("void {}({}) {{".format(name, ", ".join(arguments)))
        if reprint:
            for var in self.input.input:
                self.print_lines(var_name(var.name), var.type, 1)
        else:
            self.method.extend([
                " " * self.indentation + i
                for i in textwrap.wrap("/* TODO " + self.input.output +
                                       " */", 79 - self.indentation)
            ])
        self.method.append("}")
        self.call_site.append("{}({});".format(
            name, ", ".join([var_name(i.name) for i in self.input.input])))

    def print_line(self, name: str, type_: Type, indent_lvl: int) -> None:
        """Print the content of a var that holds in one line"""
        assert type_.fits_it_one_line(self.input.structs)
        indent = ' ' * (self.indentation * indent_lvl)
        if type_.main == TypeEnum.INT:
            self.method.append(indent + 'printf("%d\\n", {});'.format(name))
        elif type_.main == TypeEnum.CHAR:
            self.method.append(indent + 'printf("%c\\n", {});'.format(name))
        elif type_.main == TypeEnum.STR:
            self.method.append(indent + 'printf("%s\\n", {});'.format(name))
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                self.method.append(indent +
                                   'printf("%s\\n", {});'.format(name))
            else:
                index = index_name(name)
                self.method.append(indent +
                                   "for (int {0} = 0; {0} < {1}; ++{0})".
                                   format(index, var_name(type_.size)))
                self.method.append(
                    ' ' * self.indentation + indent +
                    'printf("%d%c", {0}[{1}], {1} < {2} - 1 ? \' \' : \'\\n\');'
                    .format(name, index, var_name(type_.size)))
        elif type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            self.method.append(indent + 'printf("{}\\n", {});'.format(
                " ".join("%c" if i[1].main == TypeEnum.CHAR else "%d"
                         for i in struct.fields), ", ".join(
                             name + "." + var_name(i[0])
                             for i in struct.fields)))
        else:
            assert False

    def print_lines(self, name: str, type_: Type, indent_lvl: int = 0) -> None:
        """Print the content of a var that holds in one or more lines"""
        if type_.fits_it_one_line(self.input.structs):
            self.print_line(name, type_, indent_lvl)
        else:
            if type_.main == TypeEnum.STRUCT:
                for field in self.input.get_struct(type_.struct_name).fields:
                    self.print_lines("{}.{}".format(name, field[0]), field[1],
                                     indent_lvl)
            elif type_.main == TypeEnum.LIST:
                assert type_.encapsulated is not None
                index = index_name(name)
                self.method.append(
                    "{0}for (int {1} = 0; {1} < {2}; ++{1}) {{".format(
                        " " * self.indentation * indent_lvl, index,
                        var_name(type_.size)))
                self.print_lines("{}[{}]".format(name, index),
                                 type_.encapsulated, indent_lvl + 1)
                self.method.append(" " * self.indentation * indent_lvl + "}")
            else:
                assert False

    def content(self) -> str:
        """Return the parser content"""
        output = ""
        for include in sorted(self.includes):
            output += "#include <{}>\n".format(include)
        if self.includes:
            output += "\n"
        for struct in self.input.structs:
            output += "/// {}\n".format(struct.comment)
            output += "struct {} {{\n".format(struct_name(struct.name))
            for field in struct.fields:
                output += " " * self.indentation + "{} {}; ///< {}\n".format(
                    self.type_str(field[1]), var_name(field[0]), field[2])
            output += "};\n\n"
        for line in self.method:
            output += line + "\n"
        if self.method:
            output += "\n"
        output += "int main() {\n"
        for line in self.main:
            output += ' ' * self.indentation + line + "\n"
        for line in self.call_site:
            output += ' ' * self.indentation + line + "\n"
        output += "\n" + ' ' * self.indentation + "return 0;\n}\n"
        return output


def gen_c(input_data: Input, reprint: bool = False) -> str:
    """Generate a C code to parse input"""
    parser = ParserC(input_data)
    for var in input_data.input:
        parser.read_var(var)
    parser.call(reprint)
    return parser.content()
