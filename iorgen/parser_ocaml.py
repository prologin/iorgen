# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2020 Sacha Delanoue
# Copyright 2019 Fardale
"""Generate a OCaml parser"""

import textwrap
from typing import List

from iorgen.types import Input, Struct, Type, TypeEnum
from iorgen.utils import camel_case, snake_case

KEYWORDS = [
    "and", "as", "assert", "asr", "begin", "class", "constraint", "do", "done",
    "downto", "else", "end", "exception", "external", "false", "for", "fun",
    "function", "functor", "if", "in", "include", "inherit", "initializer",
    "land", "lazy", "let", "lor", "lsl", "lsr", "lxor", "match", "method",
    "mod", "module", "mutable", "new", "object", "of", "open", "or", "private",
    "rec", "sig", "struct", "then", "to", "true", "try", "type", "val",
    "virtual", "when", "while", "with"
]

INDENTATION = "  "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for OCaml"""
    candidate = camel_case(name)
    return candidate + '_' if candidate in KEYWORDS else candidate


def record_name(name: str) -> str:
    """Transform a record name into a valid one for OCaml"""
    candidate = snake_case(name)
    if candidate in ("int", "char", "string", "list"):
        return candidate + '_'
    return candidate + '_' if candidate in KEYWORDS else candidate


def type_str(type_: Type) -> str:
    """Return the OCaml name for a type"""
    if type_.main == TypeEnum.INT:
        return "int"
    if type_.main == TypeEnum.CHAR:
        return "char"
    if type_.main == TypeEnum.STR:
        return "string"
    if type_.main == TypeEnum.STRUCT:
        return record_name(type_.struct_name)
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated
    return "{} list".format(type_str(type_.encapsulated))


def declare_record(struct: Struct) -> List[str]:
    """Declare an OCaml record"""
    out = [
        "(** {} *)".format(struct.comment),
        "type {} = {{".format(record_name(struct.name))
    ]
    out.extend(INDENTATION + "{} : {}; (** {} *)".format(
        var_name(field.name), type_str(field.type), field.comment)
               for field in struct.fields)
    return out + ["}"]


def read_line(type_: Type, input_data: Input) -> str:
    """Read an entire line into the correct type"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.INT:
        return 'read_int ()'
    if type_.main == TypeEnum.CHAR:
        return '(read_line ()).[0]'
    if type_.main == TypeEnum.STR:
        return 'read_line ()'
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.INT:
            return ('read_line () |> fun x -> if x = "" then [] else '
                    'String.split_on_char \' \' x |> List.map int_of_string')
        assert type_.encapsulated.main == TypeEnum.CHAR
        return 'List.init {} (String.get (read_line ()))'.format(
            var_name(type_.size))
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    args = [var_name(field.name) for field in struct.fields]
    return 'Scanf.sscanf (read_line ()) "{}" (fun {} -> {{{}}})'.format(
        " ".join("%d" if f.type.main == TypeEnum.INT else "%c"
                 for f in struct.fields), " ".join(args),
        "; ".join("{0}".format(i) for i in args))


def read_lines(type_: Type, input_data: Input) -> str:
    """Read one or several lines into the correct type"""
    if type_.fits_in_one_line(input_data.structs):
        return read_line(type_, input_data)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        return "List.init {} (fun _ -> {})".format(
            var_name(type_.size), read_lines(type_.encapsulated, input_data))
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return '{} {{{}}}'.format(
        " ".join("let {} = {} in".format(var_name(field.name),
                                         read_lines(field.type, input_data))
                 for field in struct.fields),
        "; ".join("{0}".format(var_name(f.name)) for f in struct.fields))


def print_line(name: str, type_: Type, input_data: Input) -> str:
    """Print a variable on one line"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.INT:
        return 'Printf.printf "%d\\n" ' + name
    if type_.main == TypeEnum.CHAR:
        return 'Printf.printf "%c\\n" ' + name
    if type_.main == TypeEnum.STR:
        return 'Printf.printf "%s\\n" ' + name
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        concat = ""
        if type_.encapsulated.main == TypeEnum.INT:
            concat = '" " (List.map string_of_int {})'.format(name)
        else:
            assert type_.encapsulated.main == TypeEnum.CHAR
            concat = '"" (List.map (String.make 1) {})'.format(name)
        return 'Printf.printf "%s\\n" (String.concat {})'.format(concat)
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return 'Printf.printf "{}\\n" {}'.format(
        " ".join("%d" if f.type.main == TypeEnum.INT else "%c"
                 for f in struct.fields),
        " ".join("{}.{}".format(name, var_name(f.name))
                 for f in struct.fields))


def print_lines(name: str, type_: Type, input_data: Input) -> str:
    """Print a variable on several lines"""
    if type_.fits_in_one_line(input_data.structs):
        return print_line(name, type_, input_data)
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        return "{} ()".format(" ".join("let () = {} in".format(
            print_lines("{}.{}".format(name, var_name(field.name)), field.type,
                        input_data)) for field in struct.fields))
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated is not None
    inner_name = name.replace(".", "_x_") + "_it"
    return "List.iter (fun {} -> {}) {}".format(
        inner_name, print_lines(inner_name, type_.encapsulated, input_data),
        name)


def method(input_data: Input, reprint: bool) -> List[str]:
    """Generate the method called with all inputs"""
    out = ["(**"] + [
        "   @param {} {}".format(var_name(var.name), var.comment)
        for var in input_data.input
    ] + ["*)"]
    out.append("let {} {} =".format(
        var_name(input_data.name),
        " ".join(var_name(i.name) for i in input_data.input)))
    if not reprint:
        out.extend(
            INDENTATION + i
            for i in textwrap.wrap("(** TODO " + input_data.output +
                                   " *)", 79 - len(INDENTATION)))
    else:
        out.extend(INDENTATION + "let () = {} in".format(
            print_lines(var_name(var.name), var.type, input_data))
                   for var in input_data.input)
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
    for var in input_data.input:
        main += INDENTATION + "let {} = {} in\n".format(
            var_name(var.name), read_lines(var.type, input_data))
    args = (var_name(i.name) for i in input_data.input)
    main += "{}{} {}\n".format(INDENTATION, var_name(input_data.name),
                               " ".join(args))
    if "List.init " in main:
        # This is a quick fix to avoid dependency of OCaml 4.06 for List.init
        output += "(* Emulate List.init from OCaml 4.06 *)\n"
        output += "module List = struct\n"
        output += INDENTATION + "include List\n\n"
        output += INDENTATION + "let init n f =\n"
        output += INDENTATION * 2 + "let rec aux i =\n"
        output += INDENTATION * 3 + "if i >= n then [] else\n"
        output += INDENTATION * 4 + "let r = f i in\n"
        output += INDENTATION * 4 + "r :: aux (i+1) in\n"
        output += INDENTATION * 2 + "aux 0\n"
        output += "end\n\n"
    if "String.split_on_char " in main:
        output += "(* Copy String.split_on_char from OCaml 4.04 *)\n"
        output += "module String = struct\n"
        output += INDENTATION + "include String\n\n"
        output += INDENTATION + "let split_on_char sep s =\n"
        output += INDENTATION * 2 + "let r = ref [] in\n"
        output += INDENTATION * 2 + "let j = ref (String.length s) in\n"
        output += INDENTATION * 2 + "for i = String.length s - 1 downto 0 do\n"
        output += INDENTATION * 3 + ("if String.unsafe_get s i = "
                                     "sep then begin\n")
        output += INDENTATION * 4 + ("r := String.sub s (i + 1) "
                                     "(!j - i - 1) :: !r;\n")
        output += INDENTATION * 4 + "j := i\n"
        output += INDENTATION * 3 + "end\n"
        output += INDENTATION * 2 + "done;\n"
        output += INDENTATION * 2 + "String.sub s 0 !j :: !r\n"
        output += "end\n\n"
    output += main
    return output
