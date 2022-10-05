# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
"""Generate a Perl parser"""

import textwrap
from typing import List

from iorgen.types import FormatStyle, Input, Type, TypeEnum, Variable
from iorgen.utils import snake_case, WordsName

INDENTATION = "    "


def var_name(var: Variable) -> str:
    """Transform a variable name into a valid one for Perl"""
    name = snake_case(var.name)
    if name in ("a", "b"):  # $a and $b are used by Perl
        name += "_"
    if var.type.main == TypeEnum.LIST:
        return "@" + name
    if var.type.main == TypeEnum.STRUCT:
        return "%" + name
    return "$" + name


def size_name(name: str) -> str:
    """Transform a variable name or an integer into a valid Perl identifier"""
    if not name:
        return ""
    snake = snake_case(name)
    try:
        int(snake)
        return snake
    except ValueError:
        if snake in ("a", "b"):  # $a and $b are used by Perl
            snake += "_"
        return "$" + snake


def sub_name(name: str) -> str:
    """Transform a subroutine name into a valid one for Perl"""
    candidate = snake_case(name)
    return candidate + "_" if candidate in KEYWORDS else candidate


def format_keep_braces(format_spec: str, arg: str) -> str:
    """Do a string formating keeping the escaped braces escaped"""
    return format_spec.replace("{{", "{{{{").replace("}}", "}}}}").format(arg)


def read_line(
    name: str, decl: str, type_: Type, input_data: Input, words: WordsName
) -> List[str]:
    """Generate the Ruby code to read a line of given type"""
    assert type_.fits_in_one_line(input_data.structs)
    if type_.main == TypeEnum.STR:
        read = "<>" if decl.startswith("my") else "scalar(<>)"
        return [decl.format(read), f"chomp {name};"]
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        decl2 = decl
        if not decl.startswith("my"):
            decl2 = format_keep_braces(decl, "\\@{{[{}]}}")
        if type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT):
            cast = "map { int } " if type_.encapsulated.main == TypeEnum.INT else ""
            return [decl2.format(f"{cast}split(/[ \\n]/, <>)")]
        assert type_.encapsulated.main == TypeEnum.CHAR
        return [decl2.format("split /\\n?/, <>")]
    if type_.main == TypeEnum.STRUCT:
        struct = input_data.get_struct(type_.struct_name)
        decl2 = decl
        if not decl.startswith("my"):
            decl2 = format_keep_braces(decl, "\\%{{{{{}}}}}")
        split = words.next_name()
        fields = (
            '"{}" => {}'.format(
                f.name,
                (
                    "int(${}[{}])"
                    if f.type.main == TypeEnum.INT
                    else "${}[{}]"
                    if f.type.main == TypeEnum.FLOAT
                    else "substr(${}[{}], 0, 1)"
                ).format(split, i),
            )
            for i, f in enumerate(struct.fields)
        )
        return [
            f"my @{split} = split /[ \\n]/, <>;",
            decl2.format(f"({', '.join(fields)})"),
        ]
    return {
        TypeEnum.INT: [decl.format("int <>")],
        # Note about the "+ 0": there is no function in perl to convert a string into
        # a float. They tell you not to care about those things, and that everything
        # will be converted automatically. Sadly here <> will return a string with the
        # endline marker (\n) still present. The simpliest removing it without having
        # to declare a intermediary variable (hello 'chomp' and its stupid signature)
        # seems to do "+ 0". Maybe there is a better solution.
        TypeEnum.FLOAT: [decl.format("<> + 0")],
        TypeEnum.CHAR: [decl.format("substr <>, 0, 1")],
    }[type_.main]


def read_lines(
    var: Variable, decl: str, size: str, input_data: Input, words: WordsName
) -> List[str]:
    """Generate the Ruby code to read the lines for a given type"""
    if var.fits_in_one_line(input_data.structs):
        return read_line(var.name, decl, var.type, input_data, words)
    if var.type.main == TypeEnum.LIST:
        assert var.type.encapsulated is not None
        lines = [
            decl.format("()" if var.name[0] == "@" else "[]"),
            "for (1..{}) {{".format(size),
        ]
        words.push_scope()
        array_name = var.name.replace("{", "{{").replace("}", "}}")
        if array_name[0] != "@":
            array_name = "@{{" + array_name + "}}"
        lines.extend(
            [
                INDENTATION + i
                for i in read_lines(
                    Variable("$" + var.name[1:] + "[-1]", "", var.type.encapsulated),
                    "push({}, {{}});".format(array_name),
                    size_name(var.type.encapsulated.size),
                    input_data,
                    words,
                )
            ]
        )
        words.pop_scope()
        return lines + ["}"]
    assert var.type.main == TypeEnum.STRUCT
    struct = input_data.get_struct(var.type.struct_name)
    sizes = [size_name(field.type.size) for field in struct.fields]
    if struct.is_sized_struct():
        sizes = ["", f"{var.name}{{'{struct.fields[0].name}'}}"]
    lines = [decl.format("()" if var.name[0] == "%" else "{}")]
    for (field, f_size) in zip(struct.fields, sizes):
        f_name = f"${var.name[1:]}{{'{field.name}'}}"
        lines.extend(
            read_lines(
                Variable(f_name, "", field.type),
                f_name.replace("{", "{{").replace("}", "}}") + " = {};",
                f_size,
                input_data,
                words,
            )
        )
    return lines


def read_vars(input_data: Input, words: WordsName) -> List[str]:
    """Read all input variables"""
    lines = []
    for variables in input_data.get_all_vars():
        if len(variables) == 1:
            var = variables[0]
            lines.extend(
                read_lines(
                    Variable(var_name(var), "", var.type, var.format_style),
                    "my {} = {{}};".format(var_name(var)),
                    size_name(var.type.size),
                    input_data,
                    words,
                )
            )
        else:
            split = words.next_name()
            lines.append(f"my @{split} = split / /, <>;")
            for i, var in enumerate(variables):
                lines.append(f"my {var_name(var)} = int(${split}[{i}]);")

    return lines


def print_line(varname: str, type_: Type, input_data: Input, style: FormatStyle) -> str:
    """Print the content of a var in one line"""
    assert type_.fits_in_one_line(input_data.structs, style)
    name = "$" + varname[1:]
    if type_.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
        endline = " " if style == FormatStyle.NO_ENDLINE else r"\n"
        return f'print "{name}{endline}";'
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        if type_.encapsulated.main == TypeEnum.CHAR:
            return f'print join("", @{{{name}}}) . "\\n";'
        assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
        return 'print "{}\\n";'.format("@{" + name + "}")
    assert type_.main == TypeEnum.STRUCT
    struct = input_data.get_struct(type_.struct_name)
    return 'print "{}\\n";'.format(
        " ".join("{}->{{'{}'}}".format(name, i.name) for i in struct.fields)
    )


def print_lines(
    name: str,
    type_: Type,
    input_data: Input,
    indent_lvl: int,
    style: FormatStyle = FormatStyle.DEFAULT,
) -> List[str]:
    """Print the content of a var that holds in one or more lines"""
    indent = INDENTATION * indent_lvl
    if type_.fits_in_one_line(input_data.structs, style):
        return [indent + print_line(name, type_, input_data, style)]
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        return (
            [indent + "map {"]
            + print_lines("$_", type_.encapsulated, input_data, indent_lvl + 1)
            + [indent + "} @{$" + name[1:] + "};"]
        )
    assert type_.main == TypeEnum.STRUCT
    lines = []
    for i in input_data.get_struct(type_.struct_name).fields:
        lines.extend(
            print_lines(
                "{}->{{'{}'}}".format(name, i.name), i.type, input_data, indent_lvl
            )
        )
    return lines


def call(input_data: Input, reprint: bool) -> List[str]:
    """Declare and call the function take all inputs in arguments"""
    name = sub_name(input_data.name)
    lines = ["# {}: {}".format(var_name(arg), arg.comment) for arg in input_data.input]
    lines.append("sub {} {{".format(name))
    lines.append(
        INDENTATION
        + "my ({}) = @_;".format(
            ", ".join("$" + var_name(i)[1:] for i in input_data.input)
        )
    )
    if reprint:
        for var in input_data.input:
            lines.extend(
                print_lines(var_name(var), var.type, input_data, 1, var.format_style)
            )
    else:
        lines.extend(
            textwrap.wrap(
                input_data.output,
                79,
                initial_indent=INDENTATION + "# " + "TODO ",
                subsequent_indent=INDENTATION + "# ",
            )
        )
    return lines + ["}"]


def gen_perl(input_data: Input, reprint: bool = False) -> str:
    """Generate a Perl code to parse input"""
    words = WordsName([var.name for var in input_data.input])
    output = "#!/usr/bin/perl\nuse strict;\nuse warnings;\n\n"
    output += "\n".join(call(input_data, reprint)) + "\n\n"
    for line in read_vars(input_data, words):
        output += line + "\n"
    args = (("\\" + var_name(i)).replace("\\$", "$") for i in input_data.input)
    output += "\n{}({});\n".format(sub_name(input_data.name), ", ".join(args))
    return output


# From https://learn.perl.org/docs/keywords.html
KEYWORDS = [
    "a",
    "abs",
    "abs2rel",
    "accept",
    "alarm",
    "and",
    "atan2",
    "b",
    "back",
    "basename",
    "begin",
    "bind",
    "binmode",
    "bless",
    "break",
    "builder",
    "caller",
    "can",
    "can_ok",
    "canonpath",
    "carp",
    "case_tolerant",
    "catdir",
    "catfile",
    "catpath",
    "chdir",
    "chmod",
    "chomp",
    "chop",
    "chown",
    "chr",
    "chroot",
    "close",
    "closedir",
    "cluck",
    "cmp",
    "cmp_ok",
    "confess",
    "connect",
    "continue",
    "copy",
    "cos",
    "cp",
    "croak",
    "crypt",
    "curdir",
    "cut",
    "dbmclose",
    "dbmopen",
    "defined",
    "delete",
    "devnull",
    "diag",
    "die",
    "dirname",
    "do",
    "dump",
    "each",
    "else",
    "elsif",
    "end",
    "endgrent",
    "endhostent",
    "endnetent",
    "endprotoent",
    "endpwent",
    "endservent",
    "eof",
    "eq",
    "eq_array",
    "eq_hash",
    "eq_set",
    "eval",
    "exec",
    "exists",
    "exit",
    "exp",
    "fail",
    "fcntl",
    "file_name_is_absolute",
    "fileno",
    "fileparse",
    "fileparse_set_fstype",
    "find",
    "finddepth",
    "flock",
    "for",
    "foreach",
    "fork",
    "format",
    "formline",
    "freeze",
    "ge",
    "getc",
    "getgrent",
    "getgrgid",
    "getgrnam",
    "gethostbyaddr",
    "gethostbyname",
    "gethostent",
    "getlogin",
    "getnetbyaddr",
    "getnetbyname",
    "getnetent",
    "getpeername",
    "getpgrp",
    "getppid",
    "getpriority",
    "getprotobyname",
    "getprotobynumber",
    "getprotoent",
    "getpwent",
    "getpwnam",
    "getpwuid",
    "getservbyname",
    "getservbyport",
    "getservent",
    "getsockname",
    "getsockopt",
    "glob",
    "gmtime",
    "goto",
    "grep",
    "gt",
    "head1",
    "head2",
    "head3",
    "head4",
    "hex",
    "if",
    "index",
    "int",
    "ioctl",
    "is",
    "is_deeply",
    "isa",
    "isa_ok",
    "isnt",
    "item",
    "join",
    "keys",
    "kill",
    "last",
    "lc",
    "lcfirst",
    "le",
    "length",
    "like",
    "link",
    "listen",
    "local",
    "localtime",
    "lock",
    "log",
    "longmess",
    "lstat",
    "lt",
    "m",
    "map",
    "mkdir",
    "mkdtemp",
    "mkpath",
    "mkstemp",
    "mkstemps",
    "mktemp",
    "move",
    "msgctl",
    "msgget",
    "msgrcv",
    "msgsnd",
    "mv",
    "my",
    "ne",
    "next",
    "no",
    "no_upwards",
    "not",
    "oct",
    "ok",
    "open",
    "opendir",
    "or",
    "ord",
    "our",
    "over",
    "pack",
    "package",
    "pass",
    "path",
    "pipe",
    "plan",
    "pod",
    "pop",
    "pos",
    "print",
    "printf",
    "prototype",
    "push",
    "q",
    "qq",
    "qr",
    "quotemeta",
    "qw",
    "qx",
    "rand",
    "read",
    "readdir",
    "readline",
    "readlink",
    "readpipe",
    "recv",
    "redo",
    "ref",
    "rel2abs",
    "rename",
    "require",
    "require_ok",
    "reset",
    "return",
    "reverse",
    "rewinddir",
    "rindex",
    "rmdir",
    "rmscopy",
    "rmtree",
    "rootdir",
    "s",
    "safe_level",
    "say",
    "scalar",
    "seek",
    "seekdir",
    "select",
    "semctl",
    "semget",
    "semop",
    "send",
    "setgrent",
    "sethostent",
    "setnetent",
    "setpgrp",
    "setpriority",
    "setprotoent",
    "setpwent",
    "setservent",
    "setsockopt",
    "shift",
    "shmctl",
    "shmget",
    "shmread",
    "shmwrite",
    "shortmess",
    "shutdown",
    "sin",
    "skip",
    "sleep",
    "socket",
    "socketpair",
    "sort",
    "splice",
    "split",
    "splitdir",
    "splitpath",
    "sprintf",
    "sqrt",
    "srand",
    "stat",
    "state",
    "study",
    "sub",
    "substr",
    "symlink",
    "syscall",
    "sysopen",
    "sysread",
    "sysseek",
    "system",
    "syswrite",
    "tell",
    "telldir",
    "tempdir",
    "tempfile",
    "thaw",
    "tie",
    "tied",
    "time",
    "times",
    "tmpdir",
    "tmpfile",
    "tmpnam",
    "todo_skip",
    "tr",
    "truncate",
    "uc",
    "ucfirst",
    "umask",
    "undef",
    "unless",
    "unlike",
    "unlink",
    "unlink0",
    "unpack",
    "unshift",
    "untie",
    "until",
    "updir",
    "use",
    "use_ok",
    "utime",
    "values",
    "vec",
    "wait",
    "waitpid",
    "wantarray",
    "warn",
    "while",
    "write",
    "xor",
    "y",
]
