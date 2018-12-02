# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a Scheme parser"""

import textwrap
from typing import List

from iorgen.types import Input, Struct, Type, TypeEnum

KEYWORDS = [
    "access", "and", "begin", "bkpt", "case", "cond", "cons-stream", "declare",
    "default-object?", "define", "define-integrable", "define-macro",
    "define-structure", "define-syntax", "delay", "do", "fluid-let", "if",
    "in-package", "lambda", "let", "let*", "let-syntax", "letrec",
    "local-declare", "macro", "make-environment", "named-lambda", "or",
    "quasiquote", "quote", "scode-quote", "sequence", "set!",
    "the-environment", "unassigned?", "using-syntax"
]

# Make sure all procedures used by generated program are here
USED_PROCEDURES = [
    "assq", "car", "cdr", "cons", "display", "for-each", "list",
    "make-assoc-list", "make-assoc-list-oneline", "make-list", "map",
    "newline", "not", "parse-int-list", "read-line", "string-ref"
]

INDENTATION = "  "

PARSE_INT_LIST = """(define (parse-int-list str)
  (if (string=? "" str) '()
    (let loop ((l (string->list str)) (s 1) (i 0))
      (cond
        ((null? l) (list (* i s)))
        ((char=? #\\space (car l)) (cons (* i s) (loop (cdr l ) 1 0)))
        ((char=? #\\- (car l)) (loop (cdr l) (* -1 s) i))
        (else (loop (cdr l) s (+ (* i 10) (- (char->integer (car l)) """ + \
                "48))))))))"

MAKE_ASSOC_LIST = """(define (make-assoc-list k f)
  (if (null? k) '() (cons (cons (car k) ((car f)))
                          (make-assoc-list (cdr k) (cdr f)))))"""

MAKE_ASSOC_LIST_ONELINE = """(define (make-assoc-list-oneline k b)
  (let loop ((l (string->list (read-line))) (k k) (b b) (c '()))
    (let ((conv (lambda () (if (car b)
                             (string->number (list->string (reverse c)))
                             (car c)))))
      (cond
        ((null? l) (list (cons (car k) (conv))))
        ((char=? #\\space (car l)) (cons (cons (car k) (conv))
                                        (loop (cdr l) (cdr k) (cdr b) '())))
        (else (loop (cdr l) k b (cons (car l) c)))))))"""


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Scheme"""
    candidate = "-".join(i.lower() for i in name.split())
    if candidate in KEYWORDS or candidate in USED_PROCEDURES:
        return candidate + "_"
    return candidate


def print_var_content(name: str, type_: Type, structs: List[Struct]) -> str:
    """Return Scheme function to print a variable of given type"""
    # pylint: disable=too-many-return-statements
    if type_.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
        return '(display {}) (newline)'.format(name)
    if type_.main == TypeEnum.STRUCT:
        if type_.fits_in_one_line(structs):
            return ("(let print_OnelineAssoc ((x {})) (if (null? x) (newline) "
                    "(begin (display (cdr (car x))) (if (not (null? (cdr x))) "
                    "(display #\\space)) (print_OnelineAssoc (cdr x)))))"
                    ).format(name)
        struct = next(x for x in structs if x.name == type_.struct_name)
        return '(begin {})'.format(" ".join(
            print_var_content(
                "(cdr (assq '{} {}))".format(var_name(f.name), name), f.type,
                structs) for f in struct.fields))
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated
        if type_.fits_in_one_line(structs):
            if type_.encapsulated.main == TypeEnum.INT:
                return ("(let print_IntList ((x {})) (if (null? x) (newline) "
                        "(begin (display (car x)) (if (not (null? (cdr x))) "
                        "(display #\\space)) (print_IntList (cdr x)))))"
                        ).format(name)
            return '(display (list->string {})) (newline)'.format(name)
        inner = name.replace("(", "p").replace(")", "P").replace(
            " ", "_").replace("'", "Q") + "_L"
        return '(for-each (lambda ({}) {}) {})'.format(
            inner, print_var_content(inner, type_.encapsulated, structs), name)
    assert False
    return ''


def wrap_code(code: str, indentation: str, skip_indent: bool = False) -> str:
    """Wrap scheme code so that it does not exceed 79 characters"""
    indent = "" if skip_indent else "\n" + indentation
    if code[0] != "(" and not code.startswith("'("):
        (begin, end) = code.split(" ", 1)
        return indent + begin + wrap_code(end, indentation)
    size = 0 if not code.startswith("'(") else 1
    opened = 1
    while opened != 0:
        size += 1
        if code[size] == "(":
            opened += 1
        elif code[size] == ")":
            opened -= 1
    extra = 0
    while size + 1 < len(code) and code[size + 1] == ")":
        size += 1
        extra += 1
    if size + len(indentation) < 80:
        line = indent + code[:size + 1]
        end = code[size + 1:]
        if not end.strip():
            return indent + code
        # the new indent may be false if we increased indentation by 1
        new_indent = " " * (len(indentation) - 2 * extra)
        return line + wrap_code(end.lstrip(), new_indent)
    if code[1] == "(":
        return indent + "(" + wrap_code(code[1:], indentation + " ", True)
    (begin, end) = code.split(" ", 1)
    return indent + begin + wrap_code(end, indentation + INDENTATION)


class ParserScheme():
    """Create the Scheme code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.parse_int_list = False
        self.make_list = False
        self.make_assoc_list = False
        self.make_assoc_list_oneline = False

    def read_line(self, type_: Type) -> str:
        """Read an entire line and parse it"""
        # pylint: disable=too-many-return-statements
        assert type_.fits_in_one_line(self.input.structs)
        if type_.main == TypeEnum.INT:
            return "string->number (read-line)"
        if type_.main == TypeEnum.CHAR:
            return "string-ref (read-line) 0"
        if type_.main == TypeEnum.STR:
            return "read-line"
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.INT:
                self.parse_int_list = True
                return "parse-int-list (read-line)"
            assert type_.encapsulated.main == TypeEnum.CHAR
            return "string->list (read-line)"
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            self.parse_int_list = True
            if all(i.type.main == TypeEnum.INT for i in struct.fields):
                return "map cons '({}) (parse-int-list (read-line))".format(
                    " ".join(var_name(i.name) for i in struct.fields))
            if all(i.type.main == TypeEnum.CHAR for i in struct.fields):
                return ("map cons '({}) (let loop ((l (string->list str)) "
                        "(b #t)) (if (null? l) '() (if b (cons (car l) (loop "
                        "(cdr l) #f)) (loop (cdr l) #t))))").format(" ".join(
                            var_name(i.name) for i in struct.fields))
            self.make_assoc_list_oneline = True
            return "make-assoc-list-oneline '({}) '({})".format(
                " ".join(var_name(i.name) for i in struct.fields),
                " ".join("#t" if i.type.main == TypeEnum.INT else "#f"
                         for i in struct.fields))
        assert False
        return ""

    def read_lines(self, type_: Type, size: str) -> str:
        """Read one or several lines and parse them"""
        if type_.fits_in_one_line(self.input.structs):
            return self.read_line(type_)
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            if struct.is_sized_struct():
                size_name = var_name(struct.fields[0].name)
                return ("let (({0} (string->number (read-line)))) (list (cons "
                        "'{0} {0}) (cons '{1} ({2})))").format(
                            size_name, var_name(struct.fields[1].name),
                            self.read_lines(struct.fields[1].type, size_name))
            self.make_assoc_list = True
            return "make-assoc-list '({}) (list {})".format(
                " ".join(var_name(i.name) for i in struct.fields),
                " ".join("(lambda () ({}))".format(
                    self.read_lines(f.type, var_name(f.type.size)))
                         for f in struct.fields))
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            self.make_list = True
            replicate = self.read_lines(type_.encapsulated,
                                        var_name(type_.encapsulated.size))
            if " " in replicate:
                replicate = "(lambda () ({}))".format(replicate)
            return "make-list {} {}".format(size, replicate)
        assert False
        return ""

    def method(self, reprint: bool) -> List[str]:
        """Declare and call the function take all inputs in arguments"""
        args = " ".join([var_name(i.name) for i in self.input.input])
        lines = [
            ";;; {}: {}".format(var_name(arg.name), arg.comment)
            for arg in self.input.input
        ]
        lines.append("(define ({} {})".format(var_name(self.input.name), args))
        if reprint:
            for var in self.input.input:
                lines.append(INDENTATION + print_var_content(
                    var_name(var.name), var.type, self.input.structs))
        else:
            lines.extend(
                textwrap.wrap(
                    self.input.output,
                    79,
                    initial_indent=INDENTATION + ";;; " + "TODO ",
                    subsequent_indent=INDENTATION + ";;; "))
            lines.append(INDENTATION + "(newline)")
        lines[-1] += ")"
        return lines

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        main = [
            "({} ({}))".format(
                var_name(var.name),
                self.read_lines(var.type, var_name(var.type.size)))
            for var in self.input.input
        ]
        output = ""
        for line in self.method(reprint):
            output += line + "\n"

        if self.parse_int_list:
            output += "\n" + PARSE_INT_LIST + "\n"
        if self.make_list:
            output += ("\n(define (make-list i f) (if (= 0 i) '() (cons (f) "
                       "(make-list (- i 1) f))))\n")
        if self.make_assoc_list:
            output += "\n" + MAKE_ASSOC_LIST + "\n"
        if self.make_assoc_list_oneline:
            output += "\n" + MAKE_ASSOC_LIST_ONELINE + "\n"

        output += "\n"
        for i, line in enumerate(main):
            indent = "       "
            if i == 0:
                output += "(let* (" + wrap_code(line, indent, True)
                if len(main) == 1:
                    output += ")\n"
            elif i == len(main) - 1:
                output += wrap_code(line + ")", indent) + "\n"
            else:
                output += wrap_code(line, indent)
        output += INDENTATION + "({} {}))\n".format(
            var_name(self.input.name), " ".join(
                var_name(var.name) for var in self.input.input))
        return output


def gen_scheme(input_data: Input, reprint: bool = False) -> str:
    """Generate a Scheme code to parse input"""
    return ParserScheme(input_data).content(reprint)
