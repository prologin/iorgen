# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
"""Generate a Java parser"""

import textwrap
from typing import List
from iorgen.types import Input, Type, TypeEnum, Variable
from iorgen.utils import camel_case, pascal_case, IteratorName, WordsName

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Java"""
    candidate = camel_case(name)
    if candidate == "reader":
        return candidate + "_"
    return candidate + "_" if candidate in KEYWORDS else candidate


def class_name(name: str) -> str:
    """Transform a class name into a valid one for Java"""
    candidate = pascal_case(name)
    if candidate in ("Main", "Integer", "String", "System"):
        return candidate + "_"
    return candidate


def type_str(type_: Type) -> str:
    """Return the Java name for a type"""
    if type_.main == TypeEnum.INT:
        return "int"
    if type_.main == TypeEnum.FLOAT:
        return "double"
    if type_.main == TypeEnum.CHAR:
        return "char"
    if type_.main == TypeEnum.STR:
        return "String"
    if type_.main == TypeEnum.STRUCT:
        return class_name(type_.struct_name)
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated
    return type_str(type_.encapsulated) + "[]"


def parse_type(type_: Type, name: str) -> str:
    """Return java code to parse a type."""
    return {
        TypeEnum.INT: f"Integer.parseInt({name})",
        TypeEnum.FLOAT: f"Double.parseDouble({name})",
        TypeEnum.CHAR: f"{name}.charAt(0)",
        TypeEnum.STR: name,
    }[type_.main]


class ParserJava:
    """Create the Java code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.imports = set(["java.io.BufferedReader", "java.io.InputStreamReader"])
        existing_names = [var.name for var in input_data.input] + [
            var_name(input_data.name)
        ]
        self.iterator = IteratorName(existing_names)
        self.words = WordsName(existing_names)

    def read_line(
        self, decl: bool, name: str, type_: Type, indent_lvl: int
    ) -> List[str]:
        """Read an entire line and store it into the right place(s)"""
        assert type_.fits_in_one_line(self.input.structs)
        indent = INDENTATION * indent_lvl
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            words = self.words.next_name()
            lines = [indent + f'String[] {words} = reader.readLine().split(" ");']
            lines.append(
                indent
                + "{}{} = new {}();".format(
                    class_name(type_.struct_name) + " " if decl else "",
                    name,
                    class_name(type_.struct_name),
                )
            )
            lines.extend(
                f"{indent}{name}.{var_name(f.name)} = "
                f"{parse_type(f.type, f'{words}[{i}]')};"
                for i, f in enumerate(struct.fields)
            )
            return lines
        type_decl = (type_str(type_) + " ") if decl else ""
        command = ""
        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
            command = parse_type(type_, "reader.readLine()")
        else:
            assert type_.main == TypeEnum.LIST
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                command = "reader.readLine().toCharArray()"
            else:
                assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
                self.imports.add("java.util.Arrays")
                command = (
                    'Arrays.stream(reader.readLine().split(" ")).{}{}.toArray()'
                ).format(
                    "filter(x -> !x.isEmpty())." if type_.can_be_empty else "",
                    "mapToInt(Integer::parseInt)"
                    if type_.encapsulated.main == TypeEnum.INT
                    else "mapToDouble(Double::parseDouble)",
                )
        assert command
        return [f"{indent}{type_decl}{name} = {command};"]

    def read_lines(
        self, decl: bool, var: Variable, size: str, indent_lvl: int
    ) -> List[str]:
        # pylint: disable=too-many-arguments
        # pylint: disable=too-many-locals
        """Read one or several lines and store them into the right place(s)"""
        if var.fits_in_one_line(self.input.structs):
            return self.read_line(decl, var.name, var.type, indent_lvl)
        indent = INDENTATION * indent_lvl
        if var.type.main == TypeEnum.STRUCT:
            lines = [
                indent
                + "{}{} = new {}();".format(
                    class_name(var.type.struct_name) + " " if decl else "",
                    var.name,
                    class_name(var.type.struct_name),
                )
            ]
            struct = self.input.get_struct(var.type.struct_name)
            for f_name, f_type, f_size in struct.fields_name_type_size(
                f"{var.name}.{{}}", var_name
            ):
                lines.extend(
                    self.read_lines(
                        False, Variable(f_name, "", f_type), f_size, indent_lvl
                    )
                )
            return lines
        assert var.type.main == TypeEnum.LIST
        assert var.type.encapsulated is not None
        far_inner_type = var.type.encapsulated
        list_suffix = ""
        while far_inner_type.main == TypeEnum.LIST:
            assert far_inner_type.encapsulated is not None
            far_inner_type = far_inner_type.encapsulated
            list_suffix += "[]"
        lines = [
            "{}{}{} = new {}[{}]{};".format(
                indent,
                (type_str(var.type) + " ") if decl else "",
                var.name,
                type_str(far_inner_type),
                size,
                list_suffix,
            )
        ]
        index = self.iterator.new_it()
        self.words.push_scope()
        lines.append(
            "{0}for (int {1} = 0; {1} < {2}; ++{1}) {{".format(indent, index, size)
        )
        lines.extend(
            self.read_lines(
                False,
                Variable(f"{var.name}[{index}]", "", var.type.encapsulated),
                var_name(var.type.encapsulated.size),
                indent_lvl + 1,
            )
        )
        self.words.pop_scope()
        self.iterator.pop_it()
        return lines + [indent + "}"]

    def call(self, reprint: bool) -> List[str]:
        """Declare and call the function take all inputs in arguments"""
        lines = [INDENTATION + "/**"]
        arguments = []
        for arg in self.input.input:
            arg_name = var_name(arg.name)
            lines.append(INDENTATION + " * @param {} {}".format(arg_name, arg.comment))
            arguments.append("{} {}".format(type_str(arg.type), arg_name))
        lines.append(INDENTATION + " */")
        lines.append(
            "{}static void {}({}) {{".format(
                INDENTATION, var_name(self.input.name), ", ".join(arguments)
            )
        )
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
                            2,
                        )
                    )
                else:
                    lines.append(
                        INDENTATION * 2
                        + 'System.out.printf("{}\\n", {});'.format(
                            " ".join(["%d"] * len(variables)),
                            ", ".join(var_name(i.name) for i in variables),
                        )
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
        if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
            return f"System.out.println({name});"
        if type_.main == TypeEnum.FLOAT:
            return f"System.out.println(iorgen_float({name}));"
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR:
                return f"System.out.println(new String({name}));"
            assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
            self.imports.add("java.util.Arrays")
            self.imports.add("java.util.stream.Collectors")
            conv = (
                "String::valueOf"
                if type_.encapsulated.main == TypeEnum.INT
                else "Main::iorgen_float"
            )
            return (
                f"System.out.println(Arrays.stream({name}).mapToObj({conv})"
                '.collect(Collectors.joining(" ")));'
            )
        assert type_.main == TypeEnum.STRUCT
        fields = self.input.get_struct(type_.struct_name).fields
        return 'System.out.printf("{}\\n", {});'.format(
            " ".join("%s" for _ in fields),
            ", ".join(
                ("iorgen_float" if f.type.main == TypeEnum.FLOAT else "String.valueOf")
                + f"({name}.{var_name(f.name)})"
                for f in fields
            ),
        )

    def print_lines(
        self,
        var: Variable,
        size: str,
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
        index = self.iterator.new_it()
        self.words.push_scope()
        lines = [
            "{0}for (int {1} = 0; {1} < {2}; ++{1}) {{".format(
                INDENTATION * indent_lvl, index, size
            )
        ]
        lines.extend(
            self.print_lines(
                Variable(f"{var.name}[{index}]", "", var.type.encapsulated),
                var_name(var.type.encapsulated.size),
                indent_lvl + 1,
            )
        )
        lines.append(INDENTATION * indent_lvl + "}")
        self.words.pop_scope()
        self.iterator.pop_it()
        return lines

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        output = ""
        for struct in self.input.structs:
            output += f"/**\n * {struct.comment}\n */\n"
            output += "class {}\n{{\n".format(class_name(struct.name))
            for field in struct.fields:
                decl_field = "{0}/**\n{0} * {1}\n{0} */\n{0}public {2} {3};\n"
                output += decl_field.format(
                    INDENTATION,
                    field.comment,
                    type_str(field.type),
                    var_name(field.name),
                )
            output += "}\n\n"
        output += "class Main {\n"
        if reprint and self.input.contains_float():
            # Since Java does not conform to the C-like behavior of format("%.15g"),
            # we need to reimplement this behavior.
            output += """
    public static String iorgen_float(double x) {
        java.text.DecimalFormatSymbols symbols =
            new java.text.DecimalFormatSymbols(java.util.Locale.ROOT);
        String a = new java.text.DecimalFormat("#.###############E00", symbols)
            .format(x).replace("E-", "e-").replace("E", "e+");
        String b = new java.text.DecimalFormat("###############.###############",
            symbols).format(x);
        return a.length() < b.length() ? a : b;
    }
            """
        output += "\n".join(self.call(reprint)) + "\n\n"
        output += (
            INDENTATION
            + "public static void main(String[] args) throws java.io.IOException {\n"
        )
        output += 2 * INDENTATION + (
            "BufferedReader reader = "
            "new BufferedReader(new InputStreamReader(System.in));\n"
        )
        for variables in self.input.get_all_vars():
            if len(variables) == 1:
                var = variables[0]
                for line in self.read_lines(
                    True,
                    Variable(var_name(var.name), "", var.type, var.format_style),
                    var_name(var.type.size),
                    2,
                ):
                    output += line + "\n"
            else:
                words = self.words.next_name()
                output += (
                    f"{INDENTATION * 2}String[] {words}"
                    ' = reader.readLine().split(" ");\n'
                )
                for i, var in enumerate(variables):
                    assert var.type.main == TypeEnum.INT
                    output += (
                        INDENTATION * 2
                        + f"int {var_name(var.name)} = Integer.parseInt({words}[{i}]);\n"
                    )
        args = (var_name(var.name) for var in self.input.input)
        output += "\n{}{}({});\n".format(
            INDENTATION * 2, var_name(self.input.name), ", ".join(args)
        )
        output += INDENTATION + "}\n}\n"
        return "".join(f"import {i};\n" for i in sorted(self.imports)) + "\n" + output


def gen_java(input_data: Input, reprint: bool = False) -> str:
    """Generate a Java code to parse input"""
    return ParserJava(input_data).content(reprint)


KEYWORDS = [
    "abstract",
    "assert",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extends",
    "final",
    "finally",
    "float",
    "for",
    "goto",
    "if",
    "implements",
    "import",
    "instanceof",
    "int",
    "interface",
    "long",
    "native",
    "new",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "static",
    "strictfp",
    "super",
    "switch",
    "synchronized",
    "this",
    "throw",
    "throws",
    "transient",
    "try",
    "void",
    "volatile",
    "while",
]
