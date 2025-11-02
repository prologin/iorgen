# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2025 Sacha Delanoue
"""Generate a Scheme parser"""

import textwrap

from iorgen.types import FormatStyle, Input, Struct, Type, TypeEnum

INDENTATION = "  "

PARSE_INT_LIST = """(define (parse-int-list str)
  (if (string=? "" str) '()
    (let loop ((l (string->list str)) (s 1) (i 0))
      (cond
        ((null? l) (list (* i s)))
        ((char=? #\\space (car l)) (cons (* i s) (loop (cdr l ) 1 0)))
        ((char=? #\\- (car l)) (loop (cdr l) (* -1 s) i))
        (else (loop (cdr l) s (+ (* i 10) (- (char->integer (car l)) 48))))))))"""

MAKE_ASSOC_LIST = """(define (make-assoc-list k f)
  (if (null? k) '() (cons (cons (car k) ((car f)))
                          (make-assoc-list (cdr k) (cdr f)))))"""

MAKE_ASSOC_LIST_ONELINE = """(define (make-assoc-list-oneline k b)
  (let loop ((l (string->list (read-line))) (k k) (b b) (c '()))
    (let ((conv (lambda () (if (eq? 'char (car b))
                             (car c)
                             ((if (eq? 'int (car b)) values exact->inexact)
                              (string->number (list->string (reverse c))))))))
      (cond
        ((null? l) (list (cons (car k) (conv))))
        ((char=? #\\space (car l)) (cons (cons (car k) (conv))
                                        (loop (cdr l) (cdr k) (cdr b) '())))
        (else (loop (cdr l) k b (cons (car l) c)))))))"""

# Since scheme does not have a format function without extensions,  we need to
# reimplement the C-like behavior of format("%.15g")
# The spec is: use scietific notation if exposant is < -4 or >= 15
PRINT_FLOAT = r"""(define (iorgen--get-pos char-list char pos)
  (cond ((null? char-list) #f)
        ((char=? char (car char-list)) pos)
        (else (iorgen--get-pos (cdr char-list) char (+ 1 pos)))))
(define (iorgen--shift-dot l buffer e dot)
  (cond ((null? l)
         (if (>= 0 e) (get-output-string buffer)
           (begin (display "0" buffer)
                  (iorgen--shift-dot l buffer (- e 1) dot))))
        ((char=? #\- (car l))
         (begin (display #\- buffer) (iorgen--shift-dot (cdr l) buffer e dot)))
        ((and (not dot) (>= 0 e) (not (null? l)))
         (if (= 0 e)
           (begin (display #\. buffer) (iorgen--shift-dot l buffer e #t))
           (begin
             (display "0." buffer)
             (let loop ((n e))
               (unless (zero? n) (display #\0 buffer) (loop (+ n 1))))
             (iorgen--shift-dot l buffer e #t))))
        ((char=? #\. (car l)) (iorgen--shift-dot (cdr l) buffer e dot))
        (else (begin (display (car l) buffer)
                     (iorgen--shift-dot (cdr l) buffer (- e 1) dot)))))
(define (iorgen--float x)
  (let* ((s (number->string x))
         (e-pos (iorgen--get-pos (string->list s) #\e 0)))
    (cond
      ((char=? #\. (string-ref s (- (string-length s) 1)))
       (substring s 0 (- (string-length s) 1)))
      ((char=? #\. (string-ref s 0))
       (string-append "0" s))
      ((and (char=? #\- (string-ref s 0)) (char=? #\. (string-ref s 1)))
       (string-append "-0" (substring s 1 (string-length s))))
      (e-pos
        (let ((e (string->number (substring s (+ 1 e-pos) (string-length s)))))
          (if (or (< e (- 4)) (>= e 15))
            (if (or (< e (- 9)) (> e 0))
              s
              (string-append (substring s 0 (- (string-length s) 1))
                             "0" (number->string (- e))))
            (iorgen--shift-dot (string->list (substring s 0 e-pos))
                               (open-output-string) (+ 1 e) #f))))
      (else s))))
(define (iorgen--display x)
  (display (if (and (number? x) (inexact? x)) (iorgen--float x) x)))"""


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Scheme"""
    candidate = "-".join(i.lower() for i in name.split())
    if candidate in KEYWORDS or candidate in USED_PROCEDURES:
        return candidate + "_"
    return candidate


def print_var_content(
    name: str,
    type_: Type,
    structs: list[Struct],
    style: FormatStyle = FormatStyle.DEFAULT,
) -> str:
    """Return Scheme function to print a variable of given type"""
    if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
        endline = 'display " "' if style == FormatStyle.NO_ENDLINE else "newline"
        return f"(iorgen--display {name}) ({endline})"
    if type_.main == TypeEnum.STRUCT:
        if type_.fits_in_one_line(structs, style):
            return (
                "(let print_OnelineAssoc ((x {})) (if (null? x) (newline) "
                "(begin (iorgen--display (cdr (car x))) (if (not (null? (cdr x))) "
                "(display #\\space)) (print_OnelineAssoc (cdr x)))))"
            ).format(name)
        struct = next(x for x in structs if x.name == type_.struct_name)
        return "(begin {})".format(
            " ".join(
                print_var_content(
                    f"(cdr (assq '{var_name(f.name)} {name}))",
                    f.type,
                    structs,
                )
                for f in struct.fields
            )
        )
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated
    if type_.fits_in_one_line(structs, style):
        if type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT):
            return (
                f"(let print_NumList ((x {name})) (if (null? x) (newline) "
                "(begin (iorgen--display (car x)) (if (not (null? (cdr x))) "
                "(display #\\space)) (print_NumList (cdr x)))))"
            )
        return f"(display (list->string {name})) (newline)"
    inner = (
        name.replace("(", "p").replace(")", "P").replace(" ", "_").replace("'", "Q")
        + "_L"
    )
    return "(for-each (lambda ({}) {}) {})".format(
        inner, print_var_content(inner, type_.encapsulated, structs), name
    )


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
        line = indent + code[: size + 1]
        end = code[size + 1 :]
        if not end.strip():
            return indent + code
        # the new indent may be false if we increased indentation by 1
        new_indent = " " * (len(indentation) - 2 * extra)
        return line + wrap_code(end.lstrip(), new_indent)
    if code[1] == "(":
        return indent + "(" + wrap_code(code[1:], indentation + " ", True)
    (begin, end) = code.split(" ", 1)
    return indent + begin + wrap_code(end, indentation + INDENTATION)


class ParserScheme:
    """Create the Scheme code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.parse_int_list = False
        self.make_list = False
        self.make_assoc_list = False
        self.make_assoc_list_oneline = False

    def read_line(self, type_: Type, style: FormatStyle) -> str:
        """Read an entire line and parse it"""
        # pylint: disable=too-many-return-statements
        assert type_.fits_in_one_line(self.input.structs, style)
        if type_.main == TypeEnum.INT:
            if style == FormatStyle.NO_ENDLINE:
                return "let ((i (read))) (begin (read-char) i)"
            return "string->number (read-line)"
        if type_.main == TypeEnum.FLOAT:
            return "exact->inexact (string->number (read-line))"
        if type_.main == TypeEnum.CHAR:
            return "string-ref (read-line) 0"
        if type_.main == TypeEnum.STR:
            return "read-line"
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.INT:
                self.parse_int_list = True
                return "parse-int-list (read-line)"
            if type_.encapsulated.main == TypeEnum.FLOAT:
                return (
                    "map exact->inexact (read (open-input-string "
                    '(string-append "(" (read-line) ")")))'
                )
            assert type_.encapsulated.main == TypeEnum.CHAR
            return "string->list (read-line)"
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        self.parse_int_list = True
        if all(i.type.main == TypeEnum.INT for i in struct.fields):
            return "map cons '({}) (parse-int-list (read-line))".format(
                " ".join(var_name(i.name) for i in struct.fields)
            )
        if all(i.type.main == TypeEnum.CHAR for i in struct.fields):
            return (
                "map cons '({}) (let loop ((l (string->list (read-line"
                "))) (b #t)) (if (null? l) '() (if b (cons (car l) (lo"
                "op (cdr l) #f)) (loop (cdr l) #t))))"
            ).format(" ".join(var_name(i.name) for i in struct.fields))
        self.make_assoc_list_oneline = True
        return "make-assoc-list-oneline '({}) '({})".format(
            " ".join(var_name(i.name) for i in struct.fields),
            " ".join(
                {
                    TypeEnum.INT: "int",
                    TypeEnum.FLOAT: "float",
                    TypeEnum.CHAR: "char",
                }[i.type.main]
                for i in struct.fields
            ),
        )

    def read_lines(
        self, type_: Type, size: str, style: FormatStyle = FormatStyle.DEFAULT
    ) -> str:
        """Read one or several lines and parse them"""
        if type_.fits_in_one_line(self.input.structs, style):
            return self.read_line(type_, style)
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            if struct.is_sized_struct():
                size_name = var_name(struct.fields[0].name)
                return (
                    "let (({0} (string->number (read-line)))) (list (cons "
                    "'{0} {0}) (cons '{1} ({2})))"
                ).format(
                    size_name,
                    var_name(struct.fields[1].name),
                    self.read_lines(struct.fields[1].type, size_name),
                )
            self.make_assoc_list = True
            return "make-assoc-list '({}) (list {})".format(
                " ".join(var_name(i.name) for i in struct.fields),
                " ".join(
                    "(lambda () ({}))".format(
                        self.read_lines(f.type, var_name(f.type.size))
                    )
                    for f in struct.fields
                ),
            )
        assert type_.main == TypeEnum.LIST
        assert type_.encapsulated is not None
        self.make_list = True
        replicate = self.read_lines(
            type_.encapsulated, var_name(type_.encapsulated.size)
        )
        if " " in replicate:
            replicate = f"(lambda () ({replicate}))"
        return f"make-list {size} {replicate}"

    def method(self, reprint: bool) -> list[str]:
        """Declare and call the function take all inputs in arguments"""
        args = " ".join([var_name(i.name) for i in self.input.input])
        lines = [f";;; {var_name(arg.name)}: {arg.comment}" for arg in self.input.input]
        lines.append(f"(define ({var_name(self.input.name)} {args})")
        if reprint:
            for var in self.input.input:
                lines.append(
                    INDENTATION
                    + print_var_content(
                        var_name(var.name),
                        var.type,
                        self.input.structs,
                        var.format_style,
                    )
                )
        else:
            lines.extend(
                textwrap.wrap(
                    self.input.output,
                    79,
                    initial_indent=INDENTATION + ";;; " + "TODO ",
                    subsequent_indent=INDENTATION + ";;; ",
                )
            )
            lines.append(INDENTATION + "(newline)")
        lines[-1] += ")"
        return lines

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        main = [
            "({} ({}))".format(
                var_name(var.name),
                self.read_lines(var.type, var_name(var.type.size), var.format_style),
            )
            for var in self.input.input
        ]
        output = ""
        if reprint:
            output += PRINT_FLOAT + "\n"
        for line in self.method(reprint):
            output += line + "\n"

        if self.parse_int_list:
            output += "\n" + PARSE_INT_LIST + "\n"
        if self.make_list:
            output += (
                "\n(define (make-list i f) (if (= 0 i) '() (cons (f) "
                "(make-list (- i 1) f))))\n"
            )
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
            var_name(self.input.name),
            " ".join(var_name(var.name) for var in self.input.input),
        )
        return output


def gen_scheme(input_data: Input, reprint: bool = False) -> str:
    """Generate a Scheme code to parse input"""
    return ParserScheme(input_data).content(reprint)


KEYWORDS = [
    "access",
    "and",
    "begin",
    "bkpt",
    "case",
    "cond",
    "cons-stream",
    "declare",
    "default-object?",
    "define",
    "define-integrable",
    "define-macro",
    "define-structure",
    "define-syntax",
    "delay",
    "do",
    "fluid-let",
    "if",
    "in-package",
    "lambda",
    "let",
    "let*",
    "let-syntax",
    "letrec",
    "local-declare",
    "macro",
    "make-environment",
    "named-lambda",
    "or",
    "quasiquote",
    "quote",
    "scode-quote",
    "sequence",
    "set!",
    "the-environment",
    "unassigned?",
    "using-syntax",
]

# Make sure all procedures used by generated program are here
USED_PROCEDURES = [
    "assq",
    "car",
    "cdr",
    "cons",
    "display",
    "for-each",
    "list",
    "make-assoc-list",
    "make-assoc-list-oneline",
    "make-list",
    "map",
    "newline",
    "not",
    "parse-int-list",
    "read-line",
    "string-ref",
]
