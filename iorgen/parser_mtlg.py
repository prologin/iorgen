# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Matthieu Moatti
"""Generate a Metalang parser"""

from typing import List, Set, Tuple  # pylint: disable=unused-import
from iorgen.types import Input, Type, TypeEnum, Variable, Struct
from iorgen.utils import snake_case, IteratorName

KEYWORDS = [
    "def", "main", "return", "end", "inline", "read", "print",
    "for", "while", "if", "else", "elsif",
    "char", "int", "void", "array", "record", "enum", "lexem", "macro",
] + [
    "abstract", "add", "alias", "as", "asm", "auto", "and", "assert",
    "base", "bool", "boolean", "break", "byte", "begin",
    "case", "catch", "char", "checked", "class", "const", "continue", "constraint",
    "decimal", "default", "delegate", "do", "double", "delete", "done", "downto", "def", "del",
    "else", "elif", "elsif", "enum", "event", "explicit", "extern", "explicit", "exp", "eval",
    "end", "exception", "external", "extends", "except", "ensure", "False", "false", "finally",
    "final", "fixed", "float", "for", "foreach", "friend", "from", "fun", "function", "functor",
    "get", "global", "goto", "go", "if", "implicit", "in", "int", "interface", "internal", "is",
    "inline", "init", "include", "inherit", "initializer", "implements", "import", "instanceof",
    "lock", "long", "lazy", "let", "lambda", "mutable", "min", "max", "match", "method", "module",
    "namespace", "new", "null", "native", "None", "nonlocal", "not", "next", "nil",
    "object", "operator", "out", "override", "or", "of", "open",
    "params", "partial", "private", "protected", "public", "package", "pass",
    "readonly", "ref", "remove", "return", "register", "rec", "raise", "redo", "rescue", "retry",
    "sig", "sbyte", "sealed", "set", "short", "sizeof", "stackalloc", "static", "string", "struct",
    "switch", "signed", "sizeof", "strictfp", "super", "synchronized",
    "this", "throw", "throws", "transient", "True", "true", "try", "typeof", "template", "typedef",
    "to", "then", "type", "uint", "ulong", "unchecked", "unsafe", "ushort", "using", "unsigned",
    "unless", "undef", "until", "value", "virtual", "void", "volatile", "val",
    "where", "while", "when", "with",
    "yield",
    "read_int", "read_char", "skip",
    "qsort"
]


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Metalang"""
    name = snake_case(name)
    if name in KEYWORDS:
        name += "_"
    return name

def record_name(name: str) -> str:
    """Transform a struct name into a valid one for Metalang"""
    name = snake_case(name)
    if name in KEYWORDS:
        name += "_"
    return "@" + name

class ParserMetalang():
    """ Metalang parser """
    def __init__(self, input_data: Input, indent: int = 0, reprint: bool = False) -> None:
        self.output = ''

        self.input_data = input_data
        self.indent = indent
        self.reprint = reprint

        self.iterator = IteratorName([var.name for var in input_data.input])

        self.struct_count = {}

    def get_type_str(self, type_: Type) -> str:
        """Return Metalang name for a type"""
        if type_.main == TypeEnum.INT:
            return 'int'
        if type_.main == TypeEnum.CHAR:
            return 'char'
        if type_.main == TypeEnum.STR:
            return 'array<char>'
        if type_.main == TypeEnum.LIST:
            return 'array<{}>'.format(self.get_type_str(type_.encapsulated))
        if type_.main == TypeEnum.STRUCT:
            return record_name(type_.struct_name)
        return ''

    def get_struct_decl(self, struct: Struct, indent: int = 0) -> str:
        """Returns a structure declaration in Metalang"""
        com = self.get_struct_comment(struct)
        if com:
            com += '\n'
        decl = '{}record {}\n'.format(com, record_name(struct.name))
        for field in struct.fields:
            decl += '{}{} : {}\n'.format(" " * indent, var_name(field.name),
                                         self.get_type_str(field.type))
        return decl + 'end'

    def get_fun_prototype(self, name: str, args: List[Variable]) -> str:
        """Returns the Metalang base function prototype"""
        name = var_name(name)
        if not args:
            return 'def void {}()'.format(name)

        arg_list = self.get_var_call(args[0])

        for arg in args[1:]:
            arg_list += ', {}'.format(self.get_var_call(arg))

        return 'def void {}({})'.format(name, arg_list)

    def get_fun_decl(self, name: str, args: List[Variable]) -> str:
        """Returns the Metalang base function declaration"""
        fun = ''
        for arg in args:
            com = self.get_param_comment(arg)
            if com:
                fun += com + '\n'
        com = ''
        if self.input_data.output:
            com = self.get_fun_comment(self.input_data.output) + '\n'
        return fun + '{}\n{}end'.format(self.get_fun_prototype(name, args), com)

    def get_fun_call(self, name: str, args: List[Variable], indent: int = 0) -> str:
        """Return the Metalang function call"""
        name = var_name(name)
        if not args:
            return '{}{}()'.format(" " * indent, name)

        arg_list = (var_name(args[0].name))

        for arg in args[1:]:
            arg_list += ', {}'.format(var_name(arg.name))

        return '{}{}({})'.format(" " * indent, name, arg_list)

    def is_native_type(self, type_: Type) -> bool:
        """Return true if type is int or char"""
        return type_.main == TypeEnum.INT or type_.main == TypeEnum.CHAR

    def get_var_call(self, var: Variable) -> str:
        """Return a Metalang variable call"""
        return '{} {}'.format(self.get_type_str(var.type), var_name(var.name))


    def is_list_in_stdlib(self, var: Variable) -> bool:
        """Tells if there is a function in stdlib to parse string"""
        return var.type.encapsulated.main == TypeEnum.LIST \
               or self.is_native_type(var.type.encapsulated)

    def get_var_decl(self, var: Variable, indent: int = 0) -> str:
        """Return declaration of a variable in Metalang"""

        str_indent = " " * indent
        # hack because parsing lists needs a loop for most cases
        if var.type.main == TypeEnum.CHAR:
            decl = self.get_char_decl(var, indent)
        elif var.type.main == TypeEnum.LIST \
             and not self.is_list_in_stdlib(var):
            decl = '{}def {}{}'.format(str_indent, self.get_var_call(var),
                                       self.get_list_parsing(var.type, indent))
        elif var.type.main == TypeEnum.STRUCT:
            decl = self.get_struct_parsing(var, indent)
        else:
            decl = '{}def {} = {}'.format(str_indent, self.get_var_call(var),
                                          self.get_type_parsing(var.type))

        com = ''
        if var.comment:
            com = ' ' + self.get_var_comment(var)
        return decl + com

    def get_char_decl(self, var: Variable, indent: int = 0) -> str:
        """ Returns parsing of a char """
        name = var_name(var.name)
        str_indent = " " * indent
        template = '{0}def array<char> {1}_tmp = read_char_line(1)\n{0}def char {1} = {1}_tmp[0]'
        return template.format(str_indent, name)

    def get_struct_details(self, var: Variable) -> Struct:
        """ Returns the Struct that represents var """
        return next(
            (s for s in self.input_data.structs \
                if record_name(s.name) == record_name(var.type.struct_name))
            , None
        )

    def get_struct_parsing(self, var: Variable, indent: int = 0) -> str:
        """ Return the parsing of a struct """
        parser = ''
        detail = self.get_struct_details(var)
        if detail.name in self.struct_count:
            self.struct_count[detail.name] += 1
        else:
            self.struct_count[detail.name] = 0

        under_count = self.struct_count[detail.name]
        for field in detail.fields:
            parser += self.get_var_decl(Variable(field.name + "_" * under_count, '',
                                                 field.type),
                                        indent) + '\n'
        parser += "{}def {} = record".format(" " * indent, self.get_var_call(var))
        for field in detail.fields:
            parser += " {} = {};".format(var_name(field.name),
                                         var_name(field.name) + "_" * under_count)
        return parser + ' end'

    def get_type_parsing(self, type_: Type) -> str:
        """Return the parsing of a type in Metalang"""
        if self.is_native_type(type_):
            return 'read_{}()'.format(self.get_type_str(type_))
        if type_.main == TypeEnum.LIST:
            return self.get_list_parsing(type_)
        if type_.main == TypeEnum.STR:
            return 'read_char_line({})'.format(type_.size)
        return ''

    def get_list_parsing(self, type_: Type, indent: int = 0) -> str:
        """Return the parsing of a list in Metalang"""
        if type_.encapsulated.main == TypeEnum.LIST:
            return self.get_matrix_parsing(type_)

        parser = ''
        if self.is_native_type(type_.encapsulated):
            parser = 'read_{}_line({})'.format(
                self.get_type_str(type_.encapsulated),
                var_name(type_.size),
            )
        else:
            iterator = self.iterator.new_it()
            parser = '[{}] with {} do\n'.format(var_name(type_.size), iterator)
            if type_.encapsulated.main == TypeEnum.STR:
                parser += '{}return read_char_line({})\n'.format(" " * (indent + self.indent),
                                                                 type_.encapsulated.size)
            elif type_.encapsulated.main == TypeEnum.STRUCT:
                iterator = var_name(type_.encapsulated.struct_name + '_i')
                parser += '{}\n{}return {}\n'.format(
                    self.get_var_decl(Variable(iterator, '', type_.encapsulated),
                                      indent + self.indent),
                    " " * (indent + self.indent),
                    iterator
                )
            parser += '{}end'.format(" " * indent)
        return parser

    def get_matrix_parsing(self, type_: Type) -> str:
        """Return the parsing of a matrix in Metalang"""
        parser = ''
        if self.is_native_type(type_.encapsulated.encapsulated):
            parser = 'read_{}_matrix({}, {})'.format(
                self.get_type_str(type_.encapsulated.encapsulated),
                var_name(type_.size),
                var_name(type_.encapsulated.size),
            )
        return parser

    def get_comment(self, comment: str) -> str:
        """ Returns a metalang comment """
        if not comment:
            return ''
        return "/* {} */".format(comment)

    def get_var_comment(self, var: Variable) -> str:
        """ Returns variable comment """
        return self.get_comment(var.comment)

    def get_param_comment(self, var: Variable) -> str:
        """ Returns a parameter comment """
        if not var.comment:
            return ''
        return '/* {}: {} */'.format(self.get_var_call(var), var.comment)

    def get_fun_comment(self, comment: str) -> str:
        """ Return the function usage comment """
        return '/* TODO {} */'.format(comment)

    def get_struct_attr_comment(self, var: Variable) -> str:
        """ Returns a struct attribut comment """
        if not var.comment:
            return ''
        return '/* {}: {} */'.format(var.name, var.comment)

    def get_struct_comment(self, struct: Struct) -> str:
        """ Return a struct comment """
        com = ''
        if struct.comment:
            com += self.get_comment(struct.comment)
        for field in struct.fields:
            if field.comment:
                if com:
                    com += '\n'
                com += self.get_comment('{}: {}'.format(field.name, field.comment))
        return com

    def get_var_print(self, var: Variable, indent: int = 0) -> str:
        """ Return the print of a var in metalang """
        if self.is_native_type(var.type):
            return '{}print {}'.format(" " * indent, var_name(var.name))
        if var.type.main == TypeEnum.LIST:
            return self.get_list_print(var, indent)
        if var.type.main == TypeEnum.STR:
            return self.get_str_print(var, indent)
        if var.type.main == TypeEnum.STRUCT:
            return self.get_struct_print(var, indent)
        return ''

    def get_list_print(self, var: Variable, indent: int = 0) -> str:
        """ Returns the printing of a list """
        printer = self.get_var_print(Variable(var.name + '[0]', '', var.type.encapsulated), indent)
        iterator = self.iterator.new_it()
        if var.type.encapsulated.main == TypeEnum.INT:
            separator = ' '
        elif var.type.encapsulated.main == TypeEnum.CHAR:
            separator = ''
        else:
            separator = '\\n'
        name = var_name(var.name) + '[{}]'.format(iterator)
        if var.type.encapsulated == TypeEnum.LIST:
            name = var_name(var.name) + '[{}][{}]'.format(iterator, iterator + '_')

        printer += '\n{}for {} = 1 to {} - 1 do\n{}print "{}" {}\n{}end'.format(
            " " * indent,
            iterator,
            var_name(var.type.size),
            " " * (indent + self.indent),
            separator,
            self.get_var_print(Variable(name, '', var.type.encapsulated)),
            " " * indent
        )

        return printer

    def get_str_print(self, var: Variable, indent: int = 0) -> str:
        """ Returns the printing of a string """
        iterator = self.iterator.new_it()

        printer = '{}for {} = 0 to {} do\n{}\n{}end'.format(
            " " * indent,
            iterator,
            var_name(str(int(var.type.size) - 1)),
            " " * (indent + self.indent) + 'print {}[{}]'.format(var_name(var.name), iterator),
            " " * indent
        )

        return printer

    def get_struct_print(self, var: Variable, indent: int = 0) -> str:
        """ Return the printing of a struct """
        struct = self.get_struct_details(var)
        printer = "{}print {}.{}".format(" " * indent, var.name, struct.fields[0].name)
        for field in struct.fields[1:]:
            printer += ' print " " print {}.{}'.format(var.name, field.name)
        return printer

    def get_main(self, fun_name: str, args: List[Variable]) -> str:
        """Return the Metalang main"""
        main = 'main\n'
        if args:
            for arg in args:
                main += self.get_var_decl(arg, self.indent) + '\n'
            if self.reprint:
                for arg in args:
                    main += self.get_var_print(arg, self.indent) + ' print "\\n"\n'
        if fun_name:
            main += self.get_fun_call(fun_name, args, self.indent) + '\n'
        return main + 'end\n'

    def generate(self) -> None:
        """Generate output for Metalang"""
        for struct in self.input_data.structs:
            self.output += self.get_struct_decl(struct, self.indent) + '\n\n'
        self.output += self.get_fun_decl(self.input_data.name, self.input_data.input) + '\n\n'
        self.output += self.get_main(self.input_data.name, self.input_data.input)

def gen_mtlg(input_data: Input, reprint: bool = False) -> str:
    """ Generate Metalang parser according to input """
    parser = ParserMetalang(input_data, 4, reprint)
    parser.generate()
    return parser.output
