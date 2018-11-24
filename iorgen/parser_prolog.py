# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a Prolog parser"""

import textwrap
from typing import List, Tuple

from iorgen.types import Input, Type, TypeEnum, Variable
from iorgen.utils import pascal_case, snake_case

# It's hard to find a list of reserved keywords in prolog. Static procedures
# can not be redefined, but there is not list of all static procedures.
# I found this list here: http://www.cse.unsw.edu.au/~billw/prologdict.html
KEYWORDS = [
    "abs", "append", "arg", "argument", "arity", "assert", "asserta",
    "assertz", "atan", "atom", "atomic", "atom_chars", "atom_codes", "bagof",
    "call", "ceiling", "compound", "consult", "dynamic", "exp", "fail",
    "findall", "floor", "functor", "halt", "integer", "is", "listing", "log",
    "member", "mod", "nl", "nonvar", "number", "once", "op", "prin", "print",
    "repeat", "retract", "retractall", "round", "see", "seeing", "seen",
    "setof", "sin", "spy", "sqrt", "static", "tab", "tan", "tell", "telling",
    "told", "trace", "true", "truncate", "var", "write"
]

# Make sure all procedures used by generated program are here
USED_PROCEDURES = [
    "atom", "atomic_list_concat", "empty_assoc", "get_assoc", "is_assoc",
    "is_list", "list_to_assoc", "maplist", "nl", "nth0", "number_string",
    "pairs_keys_values", "prompt", "put_assoc", "read_char", "read_char_list",
    "read_int", "read_int_list", "read_line", "read_line_to_codes",
    "read_list", "read_string", "split_string", "string", "string_chars",
    "string_number"
    "sub_atom", "write", "writeln"
]

INDENTATION = "    "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Prolog"""
    return pascal_case(name)


def method_name(name: str) -> str:
    """Transform a method name into a valid procedure name for Prolog"""
    candidate = snake_case(name)
    if candidate in KEYWORDS or candidate in USED_PROCEDURES:
        return candidate + "_"
    if candidate.startswith("read_assoc_"):
        return candidate + "_"
    return candidate


def call_goal(goal: str, var: str) -> str:
    """Call a Prolog goal with a new argument"""
    if goal[-1] == ")":
        goal = goal[:-1] + ", "
    else:
        goal += "("
    return "{}{}),".format(goal, var)


def print_line(name: str, type_: Type, input_data: Input) -> str:
    # pylint: disable=too-many-return-statements
    """Print a variable that fits in one line"""
    assert type_.fits_it_one_line(input_data.structs)
    if type_.main == TypeEnum.INT:
        return "integer({0}), writeln({0})".format(name)
    if type_.main == TypeEnum.CHAR:
        return "atom({0}), writeln({0})".format(name)
    if type_.main == TypeEnum.STR:
        return "string({0}), writeln({0})".format(name)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.INT:
            return ("is_list({0}), maplist(integer, {0}), atomic_list_concat("
                    "{0}, ' ', {0}_S), writeln({0}_S)").format(name)
        assert type_.encapsulated.main == TypeEnum.CHAR
        return ("is_list({0}), maplist(atom, {0}), string_chars({0}_S, {0}),"
                "writeln({0}_S)").format(name)
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        fields = []
        for i, field in enumerate(struct.fields):
            f_name = name + "_" + var_name(field.name)
            fields.append(
                'get_assoc("{0}", {1}, {2}), {3}({2}), write({2}), {4}'.format(
                    field.name, name, name + "_" + f_name,
                    "integer" if field.type.main == TypeEnum.INT else "atom",
                    'write(" ")' if i < len(struct.fields) - 1 else "nl"))
        return "is_assoc({}), {}".format(name, ", ".join(fields))
    assert False
    return ""


# I'd love to use lambda, but they come with swig 7.4, not in debian 9
def print_lines(name: str, type_: Type,
                input_data: Input) -> Tuple[List[str], str]:
    """Print a variable that fits in several lines

    Return a list of declarations to be put before, and the actual code to
    print the variable"""
    if type_.fits_it_one_line(input_data.structs):
        return ([], print_line(name, type_, input_data))
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        arg = name + "_S"
        function = "print_" + name
        (decl, code) = print_lines(arg, type_.encapsulated, input_data)
        return (decl + ["{}({}) :- {}.".format(function, arg, code)
                        ]), "is_list({1}), maplist({0}, {1})".format(
                            function, name)
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        fields = []
        decls = []
        for field in struct.fields:
            f_name = name + "_" + var_name(field.name)
            decl, print_inner = print_lines(f_name, field.type, input_data)
            fields.append('get_assoc("{}", {}, {}), {}'.format(
                field.name, name, f_name, print_inner))
            decls.extend(decl)
        return decls, "is_assoc({}), {}".format(name, ", ".join(fields))
    assert False
    return [], ""


class ParserProlog():
    """Create the Prolog code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data
        self.read = set(["str"])

    def declare_read_struct(self) -> str:
        """Declare a procedure reading a struct into a assoc"""
        output = ""
        for struct in self.input.structs:
            name = "read_assoc_{}(X) :-".format(snake_case(struct.name))
            keys = ", ".join('"{}"'.format(i.name) for i in struct.fields)
            if Type(
                    TypeEnum.STRUCT, struct_name=struct.name).fits_it_one_line(
                        self.input.structs):
                if all(i.type.main == TypeEnum.INT for i in struct.fields):
                    self.read.add("List[int]")
                    output += ('{} read_int_list(L), pairs_keys_values(P, '
                               '[{}], L), list_to_assoc(P, X).\n').format(
                                   name, keys)
                elif all(i.type.main == TypeEnum.CHAR for i in struct.fields):
                    self.read.add("List[char]")
                    output += ("{} read_string(S), atomic_list_concat(L, ' ',"
                               "S), pairs_keys_values(P, [{}], L), "
                               "list_to_assoc(P, X).\n").format(name, keys)
                else:
                    output += '{}\n'.format(name)
                    output += INDENTATION + (
                        'read_line_to_codes(user_input, C), '
                        'split_string(C, " ", "", L), empty_assoc(A0),\n')
                    for i, field in enumerate(struct.fields):
                        output += INDENTATION + (
                            'nth0({1}, L, L{1}), {3}, '
                            'put_assoc("{0}", A{1}, C{1}, A{2}),\n').format(
                                field.name, i, i + 1,
                                ("number_string(C{0}, L{0})"
                                 if field.type.main == TypeEnum.INT else
                                 "sub_atom(L{0}, 0, 1, _, C{0})").format(i))
                    output += INDENTATION + "X = A{}.\n".format(
                        len(struct.fields))
            else:
                output += "{}\n".format(name)
                names = [var_name(f.name) for f in struct.fields]
                var_x = "X"
                var_p = "P"
                while var_x in names:
                    var_x += "X"
                while var_p in names:
                    var_p += "P"
                for field in struct.fields:
                    output += INDENTATION + call_goal(
                        self.read_lines(field.type), var_name(
                            field.name)) + "\n"
                output += INDENTATION + (
                    'pairs_keys_values({0}, [{1}], [{2}]), '
                    'list_to_assoc({0}, {3}).\n').format(
                        var_p, keys, ", ".join(names), var_x)
        return output

    def read_line(self, type_: Type) -> str:
        # pylint: disable=too-many-return-statements
        """Read an entire line and parse it"""
        assert type_.fits_it_one_line(self.input.structs)
        if type_.main == TypeEnum.INT:
            self.read.add("int")
            return "read_int"
        if type_.main == TypeEnum.CHAR:
            self.read.add("char")
            return "read_char"
        if type_.main == TypeEnum.STR:
            self.read.add("str")
            return "read_line"
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.INT:
                self.read.add("List[int]")
                return "read_int_list"
            assert type_.encapsulated.main == TypeEnum.CHAR
            self.read.add("List[char]")
            return "read_char_list"
        if type_.main == TypeEnum.STRUCT:
            return "read_assoc_{}".format(snake_case(type_.struct_name))
        assert False
        return ""

    def read_lines(self, type_: Type) -> str:
        """Read one or several lines and parse them"""
        if type_.fits_it_one_line(self.input.structs):
            return self.read_line(type_)
        if type_.main == TypeEnum.STRUCT:
            return "read_assoc_{}".format(snake_case(type_.struct_name))
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            self.read.add("List")
            replicate = self.read_lines(type_.encapsulated)
            return "read_list({}, {})".format(replicate, var_name(type_.size))
        assert False
        return ""

    def read_var(self, var: Variable) -> str:
        """Read a variable"""
        goal = self.read_lines(var.type)
        return call_goal(goal, var_name(var.name))

    def method(self, reprint: bool) -> List[str]:
        """The method with all the parsed arguments"""
        lines = []
        reprint_code = []
        if reprint:
            for var in self.input.input:
                (decl, code) = print_lines(
                    var_name(var.name), var.type, self.input)
                lines.extend(decl)
                reprint_code.append(INDENTATION + code + ",")
            reprint_code[-1] = reprint_code[-1][:-1] + "."
        lines.extend([
            "% {}: {}".format(var_name(arg.name), arg.comment)
            for arg in self.input.input
        ])
        args = ", ".join([var_name(i.name) for i in self.input.input])
        lines.append("{}({}) :-".format(method_name(self.input.name), args))
        lines.extend(reprint_code)
        if not reprint:
            lines.extend(
                textwrap.wrap(
                    self.input.output,
                    79,
                    initial_indent=INDENTATION + "% " + "TODO ",
                    subsequent_indent=INDENTATION + "% "))
            lines.append(INDENTATION + "nl.")
        return lines

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        main = []
        for var in self.input.input:
            main.append(self.read_var(var))
        output = "\n".join(self.method(reprint)) + "\n\n"
        decl = self.declare_read_struct()
        if "str" in self.read:
            output += 'read_line(X) :- '
            output += 'read_string(user_input, "\\n", "\\r", _, X).\n'
        if "char" in self.read:
            output += "read_char(X) :- "
            output += "read_line(S), string_chars(S, C), nth0(0, C, X).\n"
        if "List[char]" in self.read:
            output += "read_char_list(X) :- "
            output += "read_line(S), string_chars(S, X).\n"
        if "int" in self.read:
            output += "read_int(X) :- read_line(S), number_string(X, S).\n"
        if "List[int]" in self.read:
            output += "string_number(X, Y) :- number_string(Y, X).\nread_int_"
            output += 'list(X) :- read_line_to_codes(user_input, C),\n'
            output += INDENTATION + 'split_string(C, " ", "", L),'
            output += ' maplist(string_number, L, X).\n'
        if "List" in self.read:
            output += "read_list(_, 0, []) :- !.\n"
            output += "read_list(Goal, N, [H|T]) :- "
            output += "call(Goal, H), M is N - 1, read_list(Goal, M, T).\n"
        output += decl
        output += ":-\n" + INDENTATION + "prompt(_, ''),\n"
        for line in main:
            output += INDENTATION + line + "\n"
        output += INDENTATION + "{}({}).\n".format(
            method_name(self.input.name), ", ".join(
                [var_name(i.name) for i in self.input.input]))
        return output


def gen_prolog(input_data: Input, reprint: bool = False) -> str:
    """Generate a Prolog code to parse input"""
    return ParserProlog(input_data).content(reprint)
