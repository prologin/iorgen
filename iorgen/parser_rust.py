# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2021 Sacha Delanoue
# Copyright 2020 Rémi Dupré
# Copyright 2021 Grégoire Geis
"""Generate a Rust parser"""

import textwrap
from typing import List, Optional
from iorgen.types import Input, Struct, Type, TypeEnum, Variable
from iorgen.utils import pascal_case, snake_case


# Conventional indentation token
INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Rust"""
    candidate = snake_case(name)
    if candidate in EXTENDED_KEYWORDS:
        return candidate + "_"
    if candidate.startswith("read_struct_") or candidate.endswith("_elem"):
        return candidate + "_"
    return candidate


def struct_name(name: str) -> str:
    """Transform a struct name into a valid one for Rust"""
    candidate = pascal_case(name)
    if candidate in EXTENDED_KEYWORDS:
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
        return f"Vec<{type_str(type_.encapsulated)}>"
    raise Exception


def parse_without_error(type_: Type) -> bool:
    """Check if a type can be parsed without being wrapped in a result"""
    if type_.main == TypeEnum.STR:
        return True

    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        return type_.encapsulated.main == TypeEnum.CHAR or parse_without_error(
            type_.encapsulated
        )

    return False


class ParserRust:
    """Create the Rust code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.call_site = []  # type: List[str]
        self.read_vec_int = False

    def struct_is_copy(self, struct: Struct) -> bool:
        """Check if the generated Rust structure can be marked as Copy"""
        return all(self.is_copy(t) for t in struct.fields)

    def is_copy(self, var: Variable) -> bool:
        """
        Check if the generated Rust type for a variable can be marked as Copy
        """
        type_ = var.type.main

        if type_ in [TypeEnum.CHAR, TypeEnum.INT]:
            return True

        if type_ in [TypeEnum.LIST, TypeEnum.STR]:
            return False

        if type_ == TypeEnum.STRUCT:
            return self.struct_is_copy(self.input.get_struct(var.type.struct_name))

        raise Exception

    def decl_struct(self, struct: Struct) -> List[str]:
        """Return the Rust code for declaring a struct"""
        copy = " Copy," if self.struct_is_copy(struct) else ""

        lines = [
            f"/// {struct.comment}",
            f"#[derive(Clone,{copy} Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]",
            f"struct {struct_name(struct.name)} {{",
        ]

        for field in struct.fields:
            lines += [
                f"    /// {field.comment}",
                f"    {var_name(field.name)}: {type_str(field.type)},",
            ]

        return lines + ["}", ""]

    def def_read_struct(self, struct: Struct) -> List[str]:
        """Return a rust function reading a parsing a struct"""
        s_name = struct_name(struct.name)

        if Type(TypeEnum.STRUCT, struct_name=struct.name).fits_in_one_line(
            self.input.structs
        ):
            lines = [
                f"impl std::str::FromStr for {s_name} {{",
                "    type Err = Box<dyn std::error::Error>;",
                "",
                "    fn from_str(line: &str) -> Result<Self, Self::Err> {",
                "        let mut line = line.split_whitespace();",
                "        Ok(Self {",
            ]

            for field in struct.fields:
                name = var_name(field.name)
                lines.append(
                    "            "
                    f'{name}: line.next().ok_or("missing `{name}`")?.parse()?,',
                )

            return lines + [
                "        })",
                "    }",
                "}",
            ]

        fields = ", ".join(var_name(field.name) for field in struct.fields)
        lines = []
        lines.append(
            f"fn read_struct_{snake_case(struct.name)}(mut buffer: &mut String)"
            f" -> Result<{s_name}, Box<dyn std::error::Error>> {{"
        )

        for field in struct.fields:
            lines.append(self.read_var(field, in_error_context=True))

        return lines + [
            f"    Ok({s_name} {{ {fields} }})",
            "}",
        ]

    def read_line(
        self,
        type_: Type,
        indent_lvl: int,
        name: Optional[str] = None,
        unwrap_method: Optional[str] = None,
    ) -> str:
        """Return a Rust command for parsing a line into a given type"""
        assert type_.fits_in_one_line(self.input.structs)

        if unwrap_method is None:
            if name:
                unwrap_method = f'.expect("invalid `{name}` parameter")'
            else:
                unwrap_method = ".unwrap()"

        if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STRUCT):
            code = [
                "read_line(&mut buffer)",
                ".parse()",
                unwrap_method,
            ]

            if len(unwrap_method) < 10:
                separator = ""
            else:
                separator = "\n" + (indent_lvl + 1) * INDENTATION

            return separator.join(code)

        if type_.main == TypeEnum.STR:
            return "read_line(&mut buffer).to_string()"

        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None

            if type_.encapsulated.main == TypeEnum.INT:
                self.read_vec_int = True
                idt = (indent_lvl + 1) * INDENTATION
                lines = [
                    "read_line(&mut buffer)",
                    f"{idt}.split_whitespace()",
                    f"{idt}.map(str::parse)",
                    f"{idt}.collect::<Result<_, _>>()",
                ]

                if unwrap_method == "?":
                    lines[-1] += "?"
                elif unwrap_method:
                    lines.append(f"{idt}{unwrap_method}")

                return "\n".join(lines)

            if type_.encapsulated.main == TypeEnum.CHAR:
                return "read_line(&mut buffer).chars().collect()"

        raise Exception

    def read_lines(
        self,
        name: str,
        type_: Type,
        indent_lvl: int,
        unwrap_method: Optional[str] = None,
    ) -> List[str]:
        """Return a Rust command for parsing some lines into a given type"""
        assert not type_.fits_in_one_line(self.input.structs)

        if unwrap_method is None:
            unwrap_method = f'.expect("invalid `{name}` parameter")'

        if type_.main == TypeEnum.STRUCT:
            return [
                f"read_struct_{snake_case(type_.struct_name)}(&mut buffer){unwrap_method}"
            ]

        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None

            if type_.encapsulated.fits_in_one_line(self.input.structs):
                line_read = self.read_line(
                    type_.encapsulated,
                    indent_lvl + 2,
                    name.replace(".", "_") + "_elem",
                    unwrap_method="",
                )
            else:
                line_read = "\n".join(
                    self.read_lines(
                        name.replace(".", "_") + "_elem",
                        type_.encapsulated,
                        indent_lvl + 2,
                        unwrap_method="",
                    )
                )

            idt = (indent_lvl + 1) * INDENTATION
            lines = [f"(0..{var_name(type_.size)})"]

            if line_read.count("\n") == 0:
                lines.append(f"{idt}.map(|_| {line_read})")
            else:
                lines += [
                    f"{idt}.map(|_| {{",
                    f"{idt}    {line_read}",
                    f"{idt}}})",
                ]

            if parse_without_error(type_.encapsulated):
                # read_lines doesn't return a Result
                lines.append(f"{idt}.collect()")
            else:
                lines.append(f"{idt}.collect::<Result<_, _>>()")

                if unwrap_method == "?":
                    lines[-1] += "?"
                elif unwrap_method:
                    lines.append(f"{idt}{unwrap_method}")

            return lines

        raise Exception

    def read_var(self, var: Variable, in_error_context: bool = False) -> str:
        """Return a Rust command for parsing a variable"""
        name = var_name(var.name)
        unwrap_method = "?" if in_error_context else None

        if var.type.fits_in_one_line(self.input.structs):
            read_method = self.read_line(var.type, 1, var.name, unwrap_method)
        else:
            read_method = "\n".join(self.read_lines(name, var.type, 1, unwrap_method))

        return f"    let {name} = {read_method};"

    def method(self, reprint: bool) -> List[str]:
        """Declare and call the function take all inputs in arguments"""
        args = [
            (var_name(var.name), type_str(var.type), var.comment)
            for var in self.input.input
        ]
        lines = [f"/// * `{arg}` - {comment}" for arg, _, comment in args]

        args_decl = ", ".join(f"{arg}: {atype}" for arg, atype, _ in args)
        lines.append(f"fn {var_name(self.input.name)}({args_decl}) {{")

        if reprint:
            for var in self.input.input:
                lines.extend(self.print_lines(var_name(var.name), var.type, 1))
        else:
            lines.extend(
                INDENTATION + i
                for i in textwrap.wrap(
                    "/* TODO " + self.input.output + " */", 79 - len(INDENTATION)
                )
            )

        return lines + ["}"]

    def print_line(self, name: str, type_: Type) -> str:
        """Print the content of a var that holds in one line"""
        assert type_.fits_in_one_line(self.input.structs)

        if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
            return f'print!("{{}}\\n", {name});'

        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            to_string = ""

            if type_.encapsulated.main == TypeEnum.INT:
                to_string = (
                    'iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" ")'
                )
            elif type_.encapsulated.main == TypeEnum.CHAR:
                to_string = "into_iter().collect::<String>()"
            else:
                raise Exception

            return f'print!("{{}}\\n", {name}.{to_string});'

        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            template = " ".join(["{}"] * len(struct.fields))
            fields = ", ".join(f"{name}.{var_name(f.name)}" for f in struct.fields)
            return f'print!("{template}\\n", {fields});'

        raise Exception

    def print_lines(self, name: str, type_: Type, indent_lvl: int) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        idt = INDENTATION * indent_lvl

        if type_.fits_in_one_line(self.input.structs):
            return [idt + self.print_line(name, type_)]

        if type_.main == TypeEnum.STRUCT:
            return sum(
                (
                    self.print_lines(
                        f"{name}.{var_name(field.name)}", field.type, indent_lvl
                    )
                    for field in self.input.get_struct(type_.struct_name).fields
                ),
                [],
            )

        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            inner_name = name.replace(".", "_") + "_elem"
            return [
                f"{idt}for {inner_name} in {name}.iter() {{",
                *self.print_lines(inner_name, type_.encapsulated, indent_lvl + 1),
                f"{idt}}}",
            ]

        raise Exception

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        read_vars = []

        for var in self.input.input:
            read_vars.append(self.read_var(var))
            read_vars.append("")

        structs = [self.def_read_struct(struct) for struct in self.input.structs]

        output_lines: List[str] = []

        # Structs declaration

        for struct in self.input.structs:
            output_lines += self.decl_struct(struct)

        # Function that has to be completed

        output_lines += self.method(reprint)
        output_lines.append("")

        # Main function

        output_lines.append("fn main() {")

        if self.input.input:  # There are stuff to parse
            output_lines += ["    let mut buffer = String::new();", ""]
            output_lines += read_vars

        args = ", ".join(var_name(var.name) for var in self.input.input)
        output_lines.append(f"    {var_name(self.input.name)}({args});")
        output_lines.append("}")

        # Custom parse methods

        if self.input.input:
            output_lines += [
                "",
                "fn read_line(mut buffer: &mut String) -> &str {",
                "    buffer.clear();",
                "    std::io::stdin()",
                "        .read_line(&mut buffer)",
                '        .expect("impossible to read a new line");',
                "    buffer.trim_end()",
                "}",
            ]

        for struct_def in structs:
            output_lines.append("")
            output_lines += struct_def

        output_lines.append("")
        return "\n".join(output_lines)


def gen_rust(input_data: Input, reprint: bool = False) -> str:
    """Generate a Rust code to parse input"""
    return ParserRust(input_data).content(reprint)


# Language keywords
KEYWORDS = [
    "abstract",
    "alignof",
    "as",
    "become",
    "box",
    "break",
    "const",
    "continue",
    "crate",
    "do",
    "else",
    "enum",
    "extern",
    "false",
    "final",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "macro",
    "match",
    "mod",
    "move",
    "mut",
    "offsetof",
    "override",
    "priv",
    "proc",
    "pub",
    "pure",
    "ref",
    "return",
    "Self",
    "self",
    "sizeof",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "type",
    "typeof",
    "unsafe",
    "unsized",
    "use",
    "virtual",
    "where",
    "while",
    "yield",
]

# KEYWORDS extended with the list of identifiers used by the generated code
EXTENDED_KEYWORDS = KEYWORDS + [
    "buffer",
    "read_line",
    "std",
    "Box",
    "String",
    "Vec",
    "Result",
]
