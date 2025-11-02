# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2025 Sacha Delanoue
# Copyright 2019 Fardale
"""Generate a OCaml parser"""

import textwrap

from iorgen.types import FormatStyle, Input, Struct, Type, TypeEnum
from iorgen.utils import camel_case, snake_case


INDENTATION = "  "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for OCaml"""
    candidate = camel_case(name)
    return candidate + "_" if candidate in KEYWORDS else candidate


def record_name(name: str) -> str:
    """Transform a record name into a valid one for OCaml"""
    candidate = snake_case(name)
    if candidate in ("int", "char", "string", "list"):
        return candidate + "_"
    return candidate + "_" if candidate in KEYWORDS else candidate


def type_str(type_: Type) -> str:
    """Return the OCaml name for a type"""
    if type_.main == TypeEnum.INT:
        return "int"
    if type_.main == TypeEnum.FLOAT:
        return "float"
    if type_.main == TypeEnum.CHAR:
        return "char"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.STRUCT:
        return record_name(type_.struct_name)
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated
    return f"{type_str(type_.encapsulated)} list"


def declare_record(struct: Struct) -> list[str]:
    """Declare an OCaml record"""
    out = [
        f"(** {struct.comment} *)",
        f"type {record_name(struct.name)} = {{",
    ]
    out.extend(
        INDENTATION
        + "{} : {}; (** {} *)".format(
            var_name(field.name), type_str(field.type), field.comment
        )
        for field in struct.fields
    )
    return out + ["}"]


def format_specifier(type_: Type, read: bool = True) -> str:
    """Return Ocaml print format specifier for a type"""
    return {
        TypeEnum.INT: "%d",
        TypeEnum.FLOAT: "%g" if read else "%.15g",
        TypeEnum.CHAR: "%c",
        TypeEnum.STR: "%s",
    }[type_.main]


def read_line(type_: Type, input_data: Input) -> str:
    """Read an entire line into the correct type"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT):
            # Note: here we have to use List.rev_map |> List.rev, because
            # List.map is not tail-recursive, and will trigger a stack overflow
            # if the list is big (size bigger than 1024).
            # We could check if the constraints specify a small list, and use
            # only List.map for those cases.
            cast = "int" if type_.encapsulated.main == TypeEnum.INT else "float"
            return (
                'read_line () |> fun x -> if x = "" then [] else '
                "String.split_on_char ' ' x |> "
                f"List.rev_map {cast}_of_string |> List.rev"
            )
        assert type_.encapsulated.main == TypeEnum.CHAR
        return f"List.init {var_name(type_.size)} (String.get (read_line ()))"
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        args = [var_name(field.name) for field in struct.fields]
        return 'Scanf.sscanf (read_line ()) "{}" (fun {} -> {{{}}})'.format(
            " ".join(format_specifier(f.type) for f in struct.fields),
            " ".join(args),
            "; ".join(args),
        )
    return {
        TypeEnum.INT: "read_int ()",
        TypeEnum.FLOAT: "read_float ()",
        TypeEnum.CHAR: "(read_line ()).[0]",
        TypeEnum.STR: "read_line ()",
    }[type_.main]


def read_lines(
    type_: Type, input_data: Input, style: FormatStyle = FormatStyle.DEFAULT
) -> str:
    """Read one or several lines into the correct type"""
    if type_.fits_in_one_line(input_data.structs, style):
        return read_line(type_, input_data)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        return "List.init {} (fun _ -> {})".format(
            var_name(type_.size), read_lines(type_.encapsulated, input_data)
        )
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return "{} {{{}}}".format(
        " ".join(
            "let {} = {} in".format(
                var_name(field.name), read_lines(field.type, input_data)
            )
            for field in struct.fields
        ),
        "; ".join(f"{var_name(f.name)}" for f in struct.fields),
    )


def print_line(name: str, type_: Type, input_data: Input, style: FormatStyle) -> str:
    """Print a variable on one line"""
    assert type_.fits_in_one_line(input_data.structs, style)
    newline = " " if style == FormatStyle.NO_ENDLINE else r"\n"
    if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
        return f'Printf.printf "{format_specifier(type_, read=False)}{newline}" {name}'
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        concat = ""
        if type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT):
            cast = (
                "string_of_int"
                if type_.encapsulated.main == TypeEnum.INT
                else '(Printf.sprintf "%.15g")'
            )
            concat = f'" " (List.rev (List.rev_map {cast} {name}))'
        else:
            assert type_.encapsulated.main == TypeEnum.CHAR
            concat = f'"" (List.rev (List.rev_map (String.make 1) {name}))'
        return f'Printf.printf "%s\\n" (String.concat {concat})'
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return 'Printf.printf "{}\\n" {}'.format(
        " ".join(format_specifier(f.type, read=False) for f in struct.fields),
        " ".join(f"{name}.{var_name(f.name)}" for f in struct.fields),
    )


def print_lines(
    name: str, type_: Type, input_data: Input, style: FormatStyle = FormatStyle.DEFAULT
) -> str:
    """Print a variable on several lines"""
    if type_.fits_in_one_line(input_data.structs, style):
        return print_line(name, type_, input_data, style)
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        return "{} ()".format(
            " ".join(
                "let () = {} in".format(
                    print_lines(
                        f"{name}.{var_name(field.name)}",
                        field.type,
                        input_data,
                    )
                )
                for field in struct.fields
            )
        )
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated is not None
    inner_name = name.replace(".", "_x_") + "_it"
    return "List.iter (fun {} -> {}) {}".format(
        inner_name, print_lines(inner_name, type_.encapsulated, input_data), name
    )


def method(input_data: Input, reprint: bool) -> list[str]:
    """Generate the method called with all inputs"""
    out = (
        ["(**"]
        + [f"   @param {var_name(var.name)} {var.comment}" for var in input_data.input]
        + ["*)"]
    )
    out.append(
        "let {} {} =".format(
            var_name(input_data.name),
            " ".join(var_name(i.name) for i in input_data.input),
        )
    )
    if not reprint:
        out.extend(
            INDENTATION + i
            for i in textwrap.wrap(
                "(** TODO " + input_data.output + " *)", 79 - len(INDENTATION)
            )
        )
    else:
        out.extend(
            INDENTATION
            + "let () = {} in".format(
                print_lines(var_name(var.name), var.type, input_data, var.format_style)
            )
            for var in input_data.input
        )
    out.append(INDENTATION + "()")
    return out


def gen_ocaml(input_data: Input, reprint: bool = False) -> str:
    """Generate a OCaml code to parse input"""
    output = ""
    main = ""
    for record in input_data.structs:
        main += "\n".join(declare_record(record))
        main += "\n\n"
    main += "\n".join(method(input_data, reprint))
    main += "\n\nlet () =\n"
    for variables in input_data.get_all_vars():
        if len(variables) == 1:
            var = variables[0]
            main += (
                INDENTATION
                + f"let {var_name(var.name)} = "
                + f"{read_lines(var.type, input_data, var.format_style)} in\n"
            )
        else:
            assert all(var.type.main == TypeEnum.INT for var in variables)
            main += (
                INDENTATION
                + 'let[@warning "-8"] ['
                + "; ".join(var_name(i.name) for i in variables)
                + "] = read_line () |> String.split_on_char ' '"
                '|> List.map int_of_string [@warning "+8"] in \n'
            )
    args = (var_name(i.name) for i in input_data.input)
    main += "{}{} {}\n".format(INDENTATION, var_name(input_data.name), " ".join(args))
    output += main
    return output


KEYWORDS = [
    "and",
    "as",
    "assert",
    "asr",
    "begin",
    "class",
    "constraint",
    "do",
    "done",
    "downto",
    "else",
    "end",
    "exception",
    "external",
    "false",
    "for",
    "fun",
    "function",
    "functor",
    "if",
    "in",
    "include",
    "inherit",
    "initializer",
    "land",
    "lazy",
    "let",
    "lor",
    "lsl",
    "lsr",
    "lxor",
    "match",
    "method",
    "mod",
    "module",
    "mutable",
    "new",
    "object",
    "of",
    "open",
    "or",
    "private",
    "rec",
    "sig",
    "struct",
    "then",
    "to",
    "true",
    "try",
    "type",
    "val",
    "virtual",
    "when",
    "while",
    "with",
]
