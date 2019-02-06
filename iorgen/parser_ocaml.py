# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2019 Sacha Delanoue
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

USED_SYMBOLS = [
    "int", "char", "string", "list", "listinit", "stringsplit_on_char"
]

INDENTATION = "  "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for OCaml"""
    candidate = camel_case(name)
    if candidate in KEYWORDS or candidate in USED_SYMBOLS:
        return candidate + '_'
    return candidate


def record_name(name: str) -> str:
    """Transform a record name into a valid one for OCaml"""
    candidate = snake_case(name)
    if candidate in KEYWORDS or candidate in USED_SYMBOLS:
        return candidate + '_'
    return candidate


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
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated
        return "{} list".format(type_str(type_.encapsulated))
    assert False
    return ""


def declare_record(struct: Struct) -> List[str]:
    """Declare an OCaml record"""
    out = [
        "(** {} *)".format(struct.comment), "type {} = {{".format(
            record_name(struct.name))
    ]
    out.extend(INDENTATION + "{} : {}; (** {} *)".format(
        var_name(field.name), type_str(field.type), field.comment)
               for field in struct.fields)
    return out + ["}"]


def read_line(type_: Type, input_data: Input) -> str:
    """Read an entire line into the correct type"""
    # pylint: disable=too-many-return-statements
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.INT:
        return 'Scanf.scanf "%d\\n" (fun x -> x)'
    if type_.main == TypeEnum.CHAR:
        return 'Scanf.scanf "%c\\n" (fun x -> x)'
    if type_.main == TypeEnum.STR:
        return 'Scanf.scanf "%s@\\n" (fun x -> x)'
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.INT:
            return ('Scanf.scanf "%s@\\n" (fun x -> {}List.map int_of_string '
                    '(stringsplit_on_char \' \' x))'
                    ).format('if String.length x == 0 then [] else ' if type_.
                             can_be_empty else "")
        if type_.encapsulated.main == TypeEnum.CHAR:
            return '{} (fun x -> listinit {} (String.get x))'.format(
                'Scanf.scanf "%s@\\n"', var_name(type_.size))
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        args = [var_name(field.name) for field in struct.fields]
        return 'Scanf.scanf "{}\\n" (fun {} -> {{{}}})'.format(
            " ".join("%d" if f.type.main == TypeEnum.INT else "%c"
                     for f in struct.fields), " ".join(args),
            "; ".join("{0} = {0}".format(i) for i in args))
    assert False
    return ""


def read_lines(type_: Type, input_data: Input) -> str:
    """Read one or several lines into the correct type"""
    if type_.fits_in_one_line(input_data.structs):
        return read_line(type_, input_data)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        return "listinit {} (fun _ -> {})".format(
            var_name(type_.size), read_lines(type_.encapsulated, input_data))
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        return '{} {{{}}}'.format(
            " ".join("let {} = {} in".format(
                var_name(field.name), read_lines(field.type, input_data))
                     for field in struct.fields),
            "; ".join(
                "{0} = {0}".format(var_name(f.name)) for f in struct.fields))
    assert False
    return ""


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
        elif type_.encapsulated.main == TypeEnum.CHAR:
            concat = '"" (List.map (String.make 1) {})'.format(name)
        return 'Printf.printf "%s\\n" (String.concat {})'.format(concat)
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        return 'Printf.printf "{}\\n" {}'.format(
            " ".join("%d" if f.type.main == TypeEnum.INT else "%c"
                     for f in struct.fields),
            " ".join(
                "{}.{}".format(name, var_name(f.name)) for f in struct.fields))
    assert False
    return ""


def print_lines(name: str, type_: Type, input_data: Input) -> str:
    """Print a variable on several lines"""
    if type_.fits_in_one_line(input_data.structs):
        return print_line(name, type_, input_data)
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        return "{} ()".format(" ".join("let () = {} in".format(
            print_lines("{}.{}".format(name, var_name(field.name)), field.type,
                        input_data)) for field in struct.fields))
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        inner_name = name.replace(".", "_x_") + "_it"
        return "List.iter (fun {} -> {}) {}".format(
            inner_name, print_lines(inner_name, type_.encapsulated,
                                    input_data), name)
    assert False
    return ""


def method(input_data: Input, reprint: bool) -> List[str]:
    """Generate the method called with all inputs"""
    out = ["(**"] + [
        "   @param {} {}".format(var_name(var.name), var.comment)
        for var in input_data.input
    ] + ["*)"]
    out.append("let {} {} =".format(
        var_name(input_data.name), " ".join(
            var_name(i.name) for i in input_data.input)))
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
    for record in input_data.structs:
        output += "\n".join(declare_record(record))
        output += "\n\n"
    output += "\n".join(method(input_data, reprint))
    output += "\n\nlet () =\n"
    main = ""
    for var in input_data.input:
        main += INDENTATION + "let {} = {} in\n".format(
            var_name(var.name), read_lines(var.type, input_data))
    args = (var_name(i.name) for i in input_data.input)
    main += "{}{} {}\n".format(INDENTATION, var_name(input_data.name),
                               " ".join(args))
    if " listinit " in main:
        # This is a quick fix to avoid dependency of OCaml 4.06 for List.init
        output += INDENTATION + "let listinit n f =\n"
        output += INDENTATION * 2 + ("let rec aux i = if i >= n then [] else "
                                     "let r = f i in r :: aux (i+1) in\n")
        output += INDENTATION * 2 + "aux 0 in\n\n"
    if "stringsplit_on_char " in main:
        output += INDENTATION + ("let stringsplit_on_char sep s = "
                                 "(* OCaml 4.04: String.split_on_char *)\n")
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
        output += INDENTATION * 2 + "String.sub s 0 !j :: !r in\n\n"
    output += main
    return output
