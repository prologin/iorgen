# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a C++ parser"""

import textwrap
from typing import List, Set  # pylint: disable=unused-import
from iorgen.types import Input, Type, TypeEnum, Variable
from iorgen.utils import snake_case

# keywords taken from cppreference on 2018-10-25
KEYWORDS = [
    "alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel",
    "atomic_commit", "atomic_noexcept", "auto", "bitand", "bitor", "bool",
    "break", "case", "catch", "char", "char16_t", "char32_t", "class", "compl",
    "concept", "const", "constexpr", "const_cast", "continue", "co_await",
    "co_return", "co_yield", "decltype", "default", "delete", "do", "double",
    "dynamic_cast", "else", "enum", "explicit", "export", "extern(", "false",
    "float", "for", "friend", "goto", "if", "import", "inline", "int", "long",
    "module", "mutable", "namespace", "new", "noexcept", "not", "not_eq",
    "nullptr", "operator", "or", "or_eq", "private", "protected", "public",
    "reflexpr", "register", "reinterpret_cast", "requires", "return", "short",
    "signed", "sizeof", "static", "static_assert", "static_cast", "struct",
    "switch", "synchronized", "template", "this", "thread_local", "throw",
    "true", "try", "typedef", "typeid", "typename", "union", "unsigned",
    "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"
]


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for C++"""
    candidate = snake_case(name)
    if candidate in KEYWORDS:
        return candidate + "_"
    if candidate.endswith("_elem"):  # could cause conflict in range-base loop
        return candidate + "_"
    return candidate


def struct_name(name: str) -> str:
    """Transform a struct name into a valid one for C++"""
    candidate = snake_case(name)  # We make want an other style
    if candidate in KEYWORDS:
        return candidate + "_"
    return candidate


class ParserCpp():
    """Create the C++ code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.includes = set()  # type: Set[str]
        self.main = []  # type: List[str]
        self.method = []  # type: List[str]
        self.call_site = []  # type: List[str]
        self.garbage_ws = False

        self.indentation = 4

    def type_str(self, type_: Type) -> str:
        """Return the C++ name for a type"""
        if type_.main == TypeEnum.INT:
            return "int"
        if type_.main == TypeEnum.STR:
            self.includes.add("string")
            return "std::string"
        if type_.main == TypeEnum.CHAR:
            return "char"
        if type_.main == TypeEnum.STRUCT:
            return struct_name(type_.struct_name)
        assert type_.encapsulated
        if type_.main == TypeEnum.LIST:
            self.includes.add("vector")
            return "std::vector<{}>".format(self.type_str(type_.encapsulated))
        assert False
        return ""

    def read_line(self, name: str, type_: Type, indent_lvl: int) -> None:
        """Read an entire line and store it into the right place(s)"""
        assert type_.fits_it_one_line(self.input.structs)
        indent = ' ' * (self.indentation * indent_lvl)
        self.includes.add("iostream")
        if type_.main in (TypeEnum.INT, TypeEnum.CHAR):
            self.main.append(indent + "std::cin >> {};".format(name))
        elif type_.main == TypeEnum.STR:
            self.includes.add("string")
            cin = "std::cin"
            if self.garbage_ws:
                self.includes.add("istream")
                cin = "std::cin >> std::ws"
            self.main.append(indent +
                             "std::getline({}, {});".format(cin, name))
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            inner_name = "{}_elem".format(name)
            self.main.append(indent + "for ({}& {} : {})".format(
                self.type_str(type_.encapsulated), inner_name, name))
            self.main.append(' ' * self.indentation + indent +
                             "std::cin >> {};".format(inner_name))
        elif type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            self.main.append(indent + "std::cin >> {};".format(" >> ".join(
                "{}.{}".format(name, x[0]) for x in struct.fields)))
        else:
            assert False
        self.garbage_ws = type_.main != TypeEnum.STR

    def read_lines(self, name: str, type_: Type, indent_lvl: int = 0) -> None:
        """Read one or several lines and store them into the right place(s)"""
        if type_.main == TypeEnum.LIST and indent_lvl != 0:
            self.main.append("{}{}.resize({});".format(
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
                inner_name = "{}_elem".format(name)
                self.main.append("{}for ({}& {} : {}) {{".format(
                    " " * self.indentation * indent_lvl,
                    self.type_str(type_.encapsulated), inner_name, name))
                self.read_lines(inner_name, type_.encapsulated, indent_lvl + 1)
                self.main.append(" " * self.indentation * indent_lvl + "}")
            else:
                assert False

    def read_var(self, var: Variable) -> None:
        """Read a variable"""
        size = ""
        if var.type.main == TypeEnum.LIST:
            try:
                size = "({})".format(int(var.type.size))
            except ValueError:
                size = "({})".format(var_name(var.type.size))
        self.main.append("{} {}{}; ///< {}".format(
            self.type_str(var.type), var_name(var.name), size, var.comment))
        self.read_lines(var_name(var.name), var.type)

    def call(self, reprint: bool) -> None:
        """Declare and call the function take all inputs in arguments"""
        name = var_name(self.input.name)
        arguments = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            self.method.append("/// \\param {} {}".format(
                arg_name, arg.comment))
            if arg.has_size():
                arguments.append("const {}& {}".format(
                    self.type_str(arg.type), arg_name))
            else:
                arguments.append("{} {}".format(
                    self.type_str(arg.type), arg_name))
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
        if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
            self.method.append(indent +
                               "std::cout << {} << std::endl;".format(name))
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            inner_name = "{}_elem".format(name)
            self.method.append(indent +
                               "for (size_t {0} = 0; {0} < {1}.size(); ++{0})".
                               format(inner_name, name))
            self.method.append(
                ' ' * self.indentation + indent + "std::cout << " +
                '{0}[{1}] << ({1} < {0}.size() - 1 ? "{2}" : "\\n");'.format(
                    name, inner_name, "" if type_.encapsulated.main ==
                    TypeEnum.CHAR else " "))
        elif type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            self.method.append(indent + "std::cout << {} << std::endl;".format(
                " << ' ' << ".join("{}.{}".format(name, x[0])
                                   for x in struct.fields)))
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
                inner_name = "{}_elem".format(name)
                self.method.append("{}for (const {}& {} : {}) {{".format(
                    " " * self.indentation * indent_lvl,
                    self.type_str(type_.encapsulated), inner_name, name))
                self.print_lines(inner_name, type_.encapsulated,
                                 indent_lvl + 1)
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
        output += "}\n"
        return output


def gen_cpp(input_data: Input, reprint: bool = False) -> str:
    """Generate a C++ code to parse input"""
    parser = ParserCpp(input_data)
    for var in input_data.input:
        parser.read_var(var)
    parser.call(reprint)
    return parser.content()
