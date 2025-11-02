# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2025 Sacha Delanoue
# Copyright 2022 Marc Schmitt
"""Generate a Go parser"""

import textwrap
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
        return "float64"
    if type_.main == TypeEnum.CHAR:
        return "byte"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.STRUCT:
        return struct_name(type_.struct_name)
    assert type_.encapsulated
    assert type_.main == TypeEnum.LIST
    return f"[]{type_str(type_.encapsulated)}"


def max_size(
    type_: Type,
    constraints: Constraints | None,
    input_data: Input,
    style: FormatStyle = FormatStyle.DEFAULT,
) -> int:
    """Computes the maximum number of bytes the type can take on stdin"""
    # pylint: disable=too-many-return-statements
    if type_.main == TypeEnum.INT:
        assert constraints
        return max(
            len(str(constraints.min_possible())), len(str(constraints.max_possible()))
        )
    if type_.main == TypeEnum.FLOAT:
        # The constrains on iorgen float system is to have only 15 digits on a float.
        # To that we add the eventual "-" and "." signs, for a total of 17
        # We can have exponential notation, but it can by design, only lower the size.
        # Constrains don't help much: they do not involve any precision limitiation
        assert constraints
        return 17 if constraints.min_possible() < 0 else 16  # presence of "-"
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
        max_possible = varconstraints.max_possible()
        assert isinstance(max_possible, int)
        size = max_possible
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
        self.imports = {"bufio", "os"}

        self.iterator = IteratorName([var.name for var in input_data.input])

    def read_line(self, name: str, size: str, type_: Type) -> list[str]:
        """Read an entire line and store it into the right place(s)"""

        # pylint: disable=too-many-return-statements

        assert type_.fits_in_one_line(self.input.structs)

        def parse_type(name: str, type_: Type) -> str:
            if type_.main == TypeEnum.INT:
                self.imports.add("strconv")
                return f", _ = strconv.Atoi({name})"
            if type_.main == TypeEnum.FLOAT:
                self.imports.add("strconv")
                return f", _ = strconv.ParseFloat({name}, 64)"
            if type_.main == TypeEnum.CHAR:
                return f" = {name}[0]"
            assert type_.main == TypeEnum.STR
            return f" = {name}"

        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
            return [
                "scanner.Scan()",
                f"{name}{parse_type('scanner.Text()', type_)}",
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
            lines = [
                "scanner.Scan()",
                f"for {inner_name}, {inner_name}Value "
                + f':= range strings.SplitN(scanner.Text(), " ", {size}) {{',
            ]
            lines.append(
                INDENTATION
                + f"{name}[{inner_name}]"
                + parse_type(inner_name + "Value", type_.encapsulated)
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
                    {TypeEnum.INT: "%d", TypeEnum.FLOAT: "%g", TypeEnum.CHAR: "%c"}[
                        f.type.main
                    ]
                    for f in struct.fields
                ),
                ", ".join(f"&{name}.{var_name(f.name)}" for f in struct.fields),
            ),
        ]

    def read_lines(
        self,
        var: Variable,
        size: str,
        already_allocated: bool = False,
    ) -> list[str]:
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

    def read_var(self, var: Variable) -> list[str]:
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
            lines.append(f"var {var_name(var.name)} {type_str(var.type)}")
        lines.extend(
            self.read_lines(
                Variable(var_name(var.name), "", var.type, var.format_style),
                var_name(var.type.size),
                already_allocated=True,
            )
        )
        return lines

    def call(self, reprint: bool) -> list[str]:
        """Declare and call the function take all inputs in arguments"""
        lines = []
        name = var_name(self.input.name)
        arguments = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            lines.append(f"// {arg_name}: {arg.comment}")
            arguments.append(f"{arg_name} {type_str(arg.type)}")
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
    ) -> list[str]:
        """Print the content of a var that holds in one line"""
        assert type_.fits_in_one_line(self.input.structs, style)
        indent = INDENTATION * indent_lvl
        self.imports.add("fmt")
        if type_.main in (TypeEnum.INT, TypeEnum.STR):
            return [
                indent
                + (
                    f'fmt.Print({name}, " ");'
                    if style == FormatStyle.NO_ENDLINE
                    else f"fmt.Println({name});"
                )
            ]
        if type_.main == TypeEnum.FLOAT:
            return [indent + f'fmt.Printf("%.15g\\n", {name});']
        if type_.main == TypeEnum.CHAR:
            return [indent + f'fmt.Printf("%c\\n", {name});']
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return [indent + f"fmt.Println(string({name}));"]
            index = self.iterator.new_it()
            specifier = "%d" if type_.encapsulated.main == TypeEnum.INT else "%.15g"
            lines = [
                indent + f"for {index} := range {name} {{",
                indent + INDENTATION + f'fmt.Printf("{specifier}", {name}[{index}])',
            ]
            lines.extend(
                [
                    indent + INDENTATION + f"if {index} < len({name}) - 1 {{",
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
                    {TypeEnum.INT: "%d", TypeEnum.FLOAT: "%.15g", TypeEnum.CHAR: "%c"}[
                        x.type.main
                    ]
                    for x in struct.fields
                ),
                ", ".join(f"{name}.{var_name(x.name)}" for x in struct.fields),
            )
        ]

    def print_lines(
        self,
        name: str,
        type_: Type,
        indent_lvl: int,
        style: FormatStyle = FormatStyle.DEFAULT,
    ) -> list[str]:
        """Print the content of a var that holds in one or more lines"""
        if type_.fits_in_one_line(self.input.structs, style):
            return self.print_line(name, type_, indent_lvl, style)
        if type_.main == TypeEnum.STRUCT:
            lines = []
            for field in self.input.get_struct(type_.struct_name).fields:
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
            output += f"// {struct.comment}\n"
            output += f"type {struct_name(struct.name)} struct {{\n"
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
                f"scanner.Buffer(make([]byte, 0, 64 * 1024), {max_line_length + 1})\n"
            )
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
            + "\n".join(f'import "{i}"' for i in sorted(self.imports))
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
