# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
"""Generate a Go parser"""

import textwrap
from typing import List, Optional
from iorgen.types import Constraints, FormatStyle, Input, Type, TypeEnum, Variable
from iorgen.utils import pascal_case, camel_case, IteratorName


INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Go"""
    candidate = camel_case(name)
    if candidate in KEYWORDS:
        return candidate + "_"
    if candidate in (
        "bufio",
        "byte",
        "fmt",
        "int",
        "len",
        "main",
        "make",
        "os",
        "scanner",
        "strconv",
        "string",
        "strings",
    ):
        return candidate + "_"
    return candidate


def struct_name(name: str) -> str:
    """Transform a struct name into a valid one for Go"""
    candidate = pascal_case(name)
    if candidate in KEYWORDS:
        return candidate + "_"
    return candidate


def type_str(type_: Type) -> str:
    """Return the Go name for a type"""
    if type_.main == TypeEnum.INT:
        return "int"
    if type_.main == TypeEnum.FLOAT:
        return "float32"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.CHAR:
        return "byte"
    if type_.main == TypeEnum.STRUCT:
        return struct_name(type_.struct_name)
    assert type_.encapsulated
    assert type_.main == TypeEnum.LIST
    return "[]{}".format(type_str(type_.encapsulated))


def max_size(
    type_: Type,
    constraints: Optional[Constraints],
    input_data: Input,
    style: FormatStyle = FormatStyle.DEFAULT,
) -> int:
    """Computes the maximum number of bytes the type can take on stdin"""
    if type_.main in (TypeEnum.INT, TypeEnum.FLOAT):
        assert constraints
        size = max(
            len(str(constraints.min_possible())),
            len(str(constraints.max_possible())),
        )
        if type_.main == TypeEnum.FLOAT:
            size += 1
        return size
    if type_.main == TypeEnum.CHAR:
        return 1
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        sizes = [max_size(i.type, i.constraints, input_data) for i in struct.fields]
        if type_.fits_in_one_line(input_data.structs, style):
            return sum(sizes) + len(struct.fields) - 1
        return max(sizes)
    size = -1
    size_vars = [x for x in input_data.input if x.name == type_.size]
    if not size_vars:
        size_vars = [
            x for s in input_data.structs for x in s.fields if x.name == type_.size
        ]
    if size_vars:
        varconstraints = size_vars[0].constraints
        assert varconstraints
        size = varconstraints.max_possible()
    else:
        size = int(type_.size)
    if type_.main == TypeEnum.STR:
        return size
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated
    value = max_size(type_.encapsulated, constraints, input_data)
    return (
        value * size + max(0, size - 1)
        if type_.fits_in_one_line(input_data.structs, style)
        else value
    )


class ParserGo:
    """Create the Go code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data
        self.imports = set(["bufio", "os"])

        self.iterator = IteratorName([var.name for var in input_data.input])

    def read_line(self, name: str, size: str, type_: Type) -> List[str]:
        """Read an entire line and store it into the right place(s)"""

        # pylint: disable=too-many-return-statements

        assert type_.fits_in_one_line(self.input.structs)
        if type_.main == TypeEnum.INT:
            self.imports.add("strconv")
            return [
                "scanner.Scan()",
                f"{name}, _ = strconv.Atoi(scanner.Text())",
            ]
        if type_.main == TypeEnum.FLOAT:
            self.imports.add("strconv")
            return [
                "scanner.Scan()",
                f"{name}, _ = strconv.ParseFloat(scanner.Text(), 64)",
            ]
        if type_.main == TypeEnum.CHAR:
            return ["scanner.Scan()", f"{name} = scanner.Text()[0]"]
        if type_.main == TypeEnum.STR:
            return [
                "scanner.Scan()",
                f"{name} = scanner.Text()",
            ]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return [
                    "scanner.Scan()",
                    f"{name} = scanner.Bytes()",
                ]
            inner_name = self.iterator.new_it()
            self.imports.add("strings")
            self.imports.add("strconv")
            lines = [
                "scanner.Scan()",
                f"for {inner_name}, {inner_name}Value "
                + f':= range strings.SplitN(scanner.Text(), " ", {size}) {{',
            ]
            lines.append(
                INDENTATION
                + f"{name}[{inner_name}], _ = strconv.Atoi({inner_name}Value)"
            )
            self.iterator.pop_it()
            return lines + ["}"]
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        self.imports.add("fmt")
        return [
            "scanner.Scan()",
            'fmt.Sscanf(scanner.Text(), "{}", {})'.format(
                " ".join(
                    "%d"
                    if f.type.main == TypeEnum.INT
                    else "%g"
                    if f.type.main == TypeEnum.FLOAT
                    else "%c"
                    for f in struct.fields
                ),
                ", ".join(
                    "&{}.{}".format(name, var_name(f.name)) for f in struct.fields
                ),
            ),
        ]

    def read_lines(
        self,
        var: Variable,
        size: str,
        already_allocated: bool = False,
    ) -> List[str]:
        """Read one or several lines and store them into the right place(s)"""
        lines = []
        if var.type.main == TypeEnum.LIST and not already_allocated:
            lines.append(f"{var.name} = make({type_str(var.type)}, {size})")
        if var.fits_in_one_line(self.input.structs):
            return lines + self.read_line(var.name, size, var.type)
        if var.type.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(var.type.struct_name)
            for f_name, f_type, f_size in struct.fields_name_type_size(
                var.name + ".{}", var_name
            ):
                lines.extend(self.read_lines(Variable(f_name, "", f_type), f_size))
            return lines
        assert var.type.main == TypeEnum.LIST
        assert var.type.encapsulated is not None
        inner_name = self.iterator.new_it()
        lines.append(f"for {inner_name} := range {var.name} {{")
        lines.extend(
            INDENTATION + i
            for i in self.read_lines(
                Variable(f"{var.name}[{inner_name}]", "", var.type.encapsulated),
                var_name(var.type.encapsulated.size),
            )
        )
        lines.append("}")
        self.iterator.pop_it()
        return lines

    def read_var(self, var: Variable) -> List[str]:
        """Read a variable"""
        make = False
        if var.type.main == TypeEnum.LIST:
            assert var.type.encapsulated is not None
            if var.type.encapsulated.main != TypeEnum.CHAR:
                make = True
        lines = []
        if make:
            lines.append(
                "{} := make({}, {})".format(
                    var_name(var.name), type_str(var.type), var_name(var.type.size)
                )
            )
        else:
            lines.append("var {} {}".format(var_name(var.name), type_str(var.type)))
        lines.extend(
            self.read_lines(
                Variable(var_name(var.name), "", var.type, var.format_style),
                var_name(var.type.size),
                already_allocated=True,
            )
        )
        return lines

    def call(self, reprint: bool) -> List[str]:
        """Declare and call the function take all inputs in arguments"""
        lines = []
        name = var_name(self.input.name)
        arguments = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            lines.append("// {}: {}".format(arg_name, arg.comment))
            arguments.append("{} {}".format(arg_name, type_str(arg.type)))
        lines.append("func {}({}) {{".format(name, ", ".join(arguments)))
        if reprint:
            for var in self.input.input:
                lines.extend(
                    self.print_lines(var_name(var.name), var.type, 1, var.format_style)
                )
        else:
            lines.extend(
                [
                    INDENTATION + i
                    for i in textwrap.wrap(
                        "/* TODO " + self.input.output + " */", 79 - len(INDENTATION)
                    )
                ]
            )
        return lines + ["}"]

    def print_line(
        self, name: str, type_: Type, indent_lvl: int, style: FormatStyle
    ) -> List[str]:
        """Print the content of a var that holds in one line"""
        assert type_.fits_in_one_line(self.input.structs, style)
        indent = INDENTATION * indent_lvl
        self.imports.add("fmt")
        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.STR):
            return [
                indent
                + (
                    f'fmt.Print({name}, " ");'
                    if style == FormatStyle.NO_ENDLINE
                    else f"fmt.Println({name});"
                )
            ]
        if type_.main == TypeEnum.CHAR:
            return [indent + 'fmt.Printf("%c\\n", {});'.format(name)]
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return [indent + "fmt.Println(string({}));".format(name)]
            index = self.iterator.new_it()
            lines = [
                indent + "for {} := range {} {{".format(index, name),
                indent + INDENTATION + "fmt.Print({}[{}])".format(name, index),
            ]
            lines.extend(
                [
                    indent + INDENTATION + "if {} < len({}) - 1 {{".format(index, name),
                    indent + 2 * INDENTATION + 'fmt.Print(" ")',
                    indent + INDENTATION + "}",
                ]
            )
            self.iterator.pop_it()
            return lines + [indent + "}", indent + "fmt.Println()"]
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        return [
            indent
            + 'fmt.Printf("{}\\n", {})'.format(
                " ".join(
                    "%d"
                    if x.type.main == TypeEnum.INT
                    else "%g"
                    if x.type.main == TypeEnum.FLOAT
                    else "%c"
                    for x in struct.fields
                ),
                ", ".join(
                    "{}.{}".format(name, var_name(x.name)) for x in struct.fields
                ),
            )
        ]

    def print_lines(
        self,
        name: str,
        type_: Type,
        indent_lvl: int,
        style: FormatStyle = FormatStyle.DEFAULT,
    ) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        if type_.fits_in_one_line(self.input.structs, style):
            return self.print_line(name, type_, indent_lvl, style)
        if type_.main == TypeEnum.STRUCT:
            lines = []
            for field in self.input.get_struct(type_.struct_name).fields:
                lines.extend(
                    self.print_lines(
                        "{}.{}".format(name, var_name(field.name)),
                        field.type,
                        indent_lvl,
                    )
                )
            return lines
        assert type_.main == TypeEnum.LIST
        assert type_.encapsulated is not None
        inner_name = self.iterator.new_it()
        lines = [
            "{}for _, {} := range {} {{".format(
                INDENTATION * indent_lvl, inner_name, name
            )
        ]
        lines.extend(self.print_lines(inner_name, type_.encapsulated, indent_lvl + 1))
        lines.append(INDENTATION * indent_lvl + "}")
        self.iterator.pop_it()
        return lines

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        output = ""
        for struct in self.input.structs:
            output += "// {}\n".format(struct.comment)
            output += "type {} struct {{\n".format(struct_name(struct.name))
            for field in struct.fields:
                output += INDENTATION + "{} {} // {}\n".format(
                    var_name(field.name), type_str(field.type), field.comment
                )
            output += "}\n\n"
        output += "\n".join(self.call(reprint)) + "\n\n"
        output += "func main() {\n"
        output += INDENTATION + "scanner := bufio.NewScanner(os.Stdin)\n"
        max_line_length = max(
            max_size(i.type, i.constraints, self.input, i.format_style)
            for i in self.input.input
        )
        if max_line_length > 64 * 1024:  # bufio.MaxScanTokenSize
            output += INDENTATION + (
                "scanner.Buffer(make([]byte, 0, " "64 * 1024), {})\n"
            ).format(max_line_length + 1)
        for variables in self.input.get_all_vars():
            if len(variables) == 1:
                for line in self.read_var(variables[0]):
                    output += INDENTATION + line + "\n"
            else:
                assert all(var.type.main == TypeEnum.INT for var in variables)
                output += (
                    INDENTATION
                    + f"var {', '.join(var_name(i.name) for i in variables)} int\n"
                )
                self.imports.add("fmt")
                output += INDENTATION + "scanner.Scan()\n"
                output += (
                    INDENTATION
                    + 'fmt.Sscanf(scanner.Text(), "{}", {});\n'.format(
                        " ".join(["%d"] * len(variables)),
                        ", ".join("&" + var_name(i.name) for i in variables),
                    )
                )
        output += INDENTATION + "{}({});\n".format(
            var_name(self.input.name),
            ", ".join([var_name(i.name) for i in self.input.input]),
        )
        output += "}\n"
        return (
            "package main\n\n"
            + "\n".join('import "{}"'.format(i) for i in sorted(self.imports))
            + "\n\n"
            + output
        )


def gen_go(input_data: Input, reprint: bool = False) -> str:
    """Generate a Go code to parse input"""
    return ParserGo(input_data).content(reprint)


KEYWORDS = [
    "break",
    "case",
    "chan",
    "const",
    "continue",
    "default",
    "defer",
    "else",
    "fallthrough",
    "for",
    "func",
    "go",
    "goto",
    "if",
    "import",
    "interface",
    "map",
    "package",
    "range",
    "return",
    "select",
    "struct",
    "switch",
    "type",
    "var",
]
