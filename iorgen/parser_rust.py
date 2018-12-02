# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a Rust parser"""

import textwrap
from typing import List
from iorgen.types import Input, Struct, Type, TypeEnum, Variable
from iorgen.utils import pascal_case, snake_case

KEYWORDS = [
    "abstract", "alignof", "as", "become", "box", "break", "const", "continue",
    "crate", "do", "else", "enum", "extern", "false", "final", "fn", "for",
    "if", "impl", "in", "let", "loop", "macro", "match", "mod", "move", "mut",
    "offsetof", "override", "priv", "proc", "pub", "pure", "ref", "return",
    "Self", "self", "sizeof", "static", "struct", "super", "trait", "true",
    "type", "typeof", "unsafe", "unsized", "use", "virtual", "where", "while",
    "yield"
]

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Rust"""
    candidate = snake_case(name)
    if candidate in KEYWORDS:
        return candidate + "_"
    if candidate in ("read_line", "read_vec_int"):
        return candidate + "_"
    if candidate.startswith("read_struct_") or candidate.endswith("_elem"):
        return candidate + "_"
    return candidate


def struct_name(name: str) -> str:
    """Transform a struct name into a valid one for Rust"""
    candidate = pascal_case(name)
    if candidate in KEYWORDS:
        return candidate + "_"
    if candidate in ("Vec", "String"):
        return candidate + "_"
    return candidate


def type_str(type_: Type) -> str:
    """Return the Rust name for a type"""
    if type_.main == TypeEnum.INT:
        return "i32"
    if type_.main == TypeEnum.CHAR:
        return "char"
    if type_.main == TypeEnum.STR:
        return "String"
    if type_.main == TypeEnum.STRUCT:
        return struct_name(type_.struct_name)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated
        return "Vec<{}>".format(type_str(type_.encapsulated))
    assert False
    return ""


def decl_struct(struct: Struct) -> List[str]:
    """Return the Rust code for declaring a struct"""
    lines = [
        "/// " + struct.comment, "struct {} {{".format(
            struct_name(struct.name))
    ]
    for field in struct.fields:
        lines.extend([
            INDENTATION + "/// " + field.comment, "{}{}: {},".format(
                INDENTATION, var_name(field.name), type_str(field.type))
        ])
    return lines + ["}", ""]


class ParserRust():
    """Create the Rust code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.call_site = []  # type: List[str]
        self.read_vec_int = False

    def def_read_struct(self, struct: Struct) -> List[str]:
        """Return a rust function reading a parsing a struct"""
        lines = [
            "fn read_struct_{}() -> {} {{".format(
                snake_case(struct.name), struct_name(struct.name))
        ]
        if Type(
                TypeEnum.STRUCT,
                struct_name=struct.name).fits_in_one_line(self.input.structs):
            lines.extend([
                INDENTATION + "let line = read_line();", INDENTATION +
                "let words: Vec<&str> = line.split_whitespace().collect();",
                INDENTATION + "{} {{".format(struct_name(struct.name))
            ])
            lines.extend("{}{}: words[{}].parse().unwrap(),".format(
                2 * INDENTATION, var_name(field.name), i)
                         for i, field in enumerate(struct.fields))
            lines.append(INDENTATION + "}")
        else:
            for field in struct.fields:
                lines.extend(
                    self.read_var(
                        Variable(field.name, field.comment, field.type)))
            lines.append("{}{} {{ {} }}".format(
                INDENTATION, struct_name(struct.name), ", ".join(
                    var_name(f.name) for f in struct.fields)))
        return lines + ["}"]

    def read_line(self, type_: Type) -> str:
        """Return a Rust command for parsing a line into a given type"""
        assert type_.fits_in_one_line(self.input.structs)
        if type_.main in (TypeEnum.INT, TypeEnum.CHAR):
            return "read_line().parse().unwrap()"
        if type_.main == TypeEnum.STR:
            return "read_line()"
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.INT:
                self.read_vec_int = True
                return "read_vec_int()"
            if type_.encapsulated.main == TypeEnum.CHAR:
                return "read_line().chars().collect()"
        if type_.main == TypeEnum.STRUCT:
            return "read_struct_{}()".format(snake_case(type_.struct_name))
        assert False
        return ""

    def read_lines(self, name: str, type_: Type, indent_lvl: int) -> List[str]:
        """Return a Rust command for parsing some lines into a given type"""
        assert not type_.fits_in_one_line(self.input.structs)
        if type_.main == TypeEnum.STRUCT:
            return [
                "{}let {}: {} = read_struct_{}();".format(
                    INDENTATION * indent_lvl, name, type_str(type_),
                    snake_case(type_.struct_name))
            ]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            lines = [
                INDENTATION * indent_lvl +
                "let mut {}: {} = Vec::with_capacity({} as usize);".format(
                    name, type_str(type_), var_name(type_.size)),
                INDENTATION * indent_lvl + "for _ in 0..{} {{".format(
                    var_name(type_.size))
            ]
            if type_.encapsulated.fits_in_one_line(self.input.structs):
                lines.append(INDENTATION * (indent_lvl + 1) + "{}.push({});".
                             format(name, self.read_line(type_.encapsulated)))
            else:
                lines.extend(
                    self.read_lines("{}_elem".format(name.replace(".", "_")),
                                    type_.encapsulated, indent_lvl + 1))
                lines.append(INDENTATION * (indent_lvl + 1) +
                             "{0}.push({0}_elem);".format(name))
            return lines + [INDENTATION * indent_lvl + "}"]
        assert False
        return []

    def read_var(self, var: Variable) -> List[str]:
        """Return a Rust command for parsing a variable"""
        if var.type.fits_in_one_line(self.input.structs):
            return [
                INDENTATION + "let {}: {} = {};".format(
                    var_name(var.name), type_str(var.type),
                    self.read_line(var.type))
            ]
        return self.read_lines(var_name(var.name), var.type, 1)

    def method(self, reprint: bool) -> List[str]:
        """Declare and call the function take all inputs in arguments"""
        lines = [
            "/// * `{}` - {}".format(var_name(var.name), var.comment)
            for var in self.input.input
        ]
        lines.append("fn {}({}) {{".format(
            var_name(self.input.name),
            ", ".join("{}: {}".format(var_name(var.name), type_str(var.type))
                      for var in self.input.input)))
        if reprint:
            for var in self.input.input:
                lines.extend(self.print_lines(var_name(var.name), var.type, 1))
        else:
            lines.extend(
                INDENTATION + i
                for i in textwrap.wrap("/* TODO " + self.input.output +
                                       " */", 79 - len(INDENTATION)))
        return lines + ["}"]

    def print_line(self, name: str, type_: Type) -> str:
        """Print the content of a var that holds in one line"""
        assert type_.fits_in_one_line(self.input.structs)
        if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
            return 'print!("{{}}\\n", {});'.format(name)
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            to_string = ""
            if type_.encapsulated.main == TypeEnum.INT:
                to_string = 'iter().map(|x| x.to_string())' + \
                            '.collect::<Vec<String>>().join(" ")'
            elif type_.encapsulated.main == TypeEnum.CHAR:
                to_string = 'into_iter().collect::<String>()'
            else:
                assert False
            return 'print!("{{}}\\n", {}.{});'.format(name, to_string)
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            return 'print!("{}\\n", {});'.format(
                " ".join(["{}"] * len(struct.fields)), ", ".join(
                    name + "." + var_name(f.name) for f in struct.fields))
        assert False
        return ""

    def print_lines(self, name: str, type_: Type,
                    indent_lvl: int) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        indent = INDENTATION * indent_lvl
        if type_.fits_in_one_line(self.input.structs):
            return [indent + self.print_line(name, type_)]
        if type_.main == TypeEnum.STRUCT:
            lines = []
            for field in self.input.get_struct(type_.struct_name).fields:
                lines.extend(
                    self.print_lines(name + "." + var_name(field.name),
                                     field.type, indent_lvl))
            return lines
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            inner_name = "{}_elem".format(name.replace(".", "_"))
            lines = ["{}for {} in &{} {{".format(indent, inner_name, name)]
            lines.extend(
                self.print_lines(inner_name, type_.encapsulated,
                                 indent_lvl + 1))
            return lines + [indent + "}"]
        assert False
        return []

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        read_vars = []
        for var in self.input.input:
            read_vars.extend(self.read_var(var))
        structs = [
            self.def_read_struct(struct) for struct in self.input.structs
        ]

        output = ""
        if self.input.input:  # There are stuff to parse
            output += "use std::io;\n\n"
        for struct in self.input.structs:
            output += "\n".join(decl_struct(struct)) + "\n"
        output += "\n".join(self.method(reprint)) + "\n\n"
        output += "fn main() {\n"
        output += "\n".join(read_vars) + "\n"
        args = (var_name(var.name) for var in self.input.input)
        output += "\n{}{}({});\n".format(INDENTATION, var_name(
            self.input.name), ", ".join(args))
        output += "}\n"
        if self.input.input:  # There are stuff to parse
            output += "\nfn read_line() -> String {\n"
            output += INDENTATION + "let mut line = String::new();\n"
            output += INDENTATION + "io::stdin()\n"
            output += 2 * INDENTATION + ".read_line(&mut line)\n"
            output += 2 * INDENTATION + '.expect("Failed to read line");\n'
            output += INDENTATION + "line.trim().to_string()\n"
            output += "}\n"
        if self.read_vec_int:
            output += "\nfn read_vec_int() -> Vec<i32> {\n"
            output += INDENTATION + "read_line()\n"
            output += 2 * INDENTATION + ".split_whitespace()\n"
            output += 2 * INDENTATION + ".collect::<Vec<&str>>()\n"
            output += 2 * INDENTATION + ".iter()\n"
            output += 2 * INDENTATION + ".map(|x| x.parse().unwrap())\n"
            output += 2 * INDENTATION + ".collect()\n"
            output += "}\n"
        for struct_def in structs:
            output += "\n" + "\n".join(struct_def) + "\n"
        return output


def gen_rust(input_data: Input, reprint: bool = False) -> str:
    """Generate a Rust code to parse input"""
    return ParserRust(input_data).content(reprint)
