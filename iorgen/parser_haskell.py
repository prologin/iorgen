# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a Haskll parser"""

import textwrap
from collections import OrderedDict
from typing import List, Set, Tuple  # pylint: disable=unused-import

from iorgen.types import Input, Struct, Type, TypeEnum, Variable


def pascal_case(name: str) -> str:
    """Format a name with pascal case style"""
    return "".join(i.lower().capitalize() for i in name.split())


def camel_case(name: str) -> str:
    """Format a name with camel case style"""
    pascal = pascal_case(name)
    return pascal[0].lower() + pascal[1:]


# keywords taken from wiki.haskell.org on 2018-10-26
KEYWORDS = [
    "as", "case", "of", "class", "data", "default", "deriving", "do", "forall",
    "foreign", "hiding", "if", "then", "else", "import", "infix", "infixl",
    "infixr", "instance", "let", "in", "mdo", "module", "newtype", "proc",
    "qualified", "rec", "type", "where"
]


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Haskell"""
    candidate = camel_case(name)
    if candidate in KEYWORDS:
        return candidate + "'"
    if candidate in ("fmap", "map", "getLine", "head", "putStrLn", "read",
                     "replicateM", "words"):
        return candidate + "'"
    if candidate.startswith("read") and candidate[4].isupper():
        return candidate + "'"
    return candidate


def data_name(name: str) -> str:
    """Transform a data name into a valid one for Haskell"""
    candidate = pascal_case(name)
    if candidate in ("Char", "Int", "String", "IO"):  # We use those types
        return candidate + "'"
    return candidate


def type_str(type_: Type) -> str:
    """Return the Haskell name for a type"""
    if type_.main == TypeEnum.INT:
        return "Int"
    if type_.main == TypeEnum.CHAR:
        return "Char"
    if type_.main == TypeEnum.STR:
        return "String"
    if type_.main == TypeEnum.STRUCT:
        return data_name(type_.struct_name)
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated
        return "[{}]".format(type_str(type_.encapsulated))
    assert False
    return ""


def print_var_content(type_: Type, structs: List[Struct]) -> str:
    """Return Haskell function to print a variable of given type"""
    # pylint: disable=too-many-return-statements
    if type_.main == TypeEnum.INT:
        return '(++ "\\n") . show'
    if type_.main == TypeEnum.CHAR:
        return '(: "\\n")'
    if type_.main == TypeEnum.STR:
        return '(++ "\\n")'
    if type_.main == TypeEnum.STRUCT:
        struct = next(x for x in structs if x.name == type_.struct_name)
        if type_.fits_it_one_line(structs):
            fields = []
            for i in struct.fields:
                if i[1].main == TypeEnum.INT:
                    fields.append("(show $ {} r)".format(var_name(i[0])))
                else:
                    assert i[1].main == TypeEnum.CHAR
                    fields.append('((: "") $ {} r)'.format(var_name(i[0])))
            return '(\\r -> {} ++ "\\n")'.format(' ++ " " ++ '.join(fields))
        return "(\\r -> {})".format(" ++ ".join("({} $ {} r)".format(
            print_var_content(i[1], structs), var_name(i[0]))
                                                for i in struct.fields))
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated
        if type_.encapsulated.main == TypeEnum.INT:
            return '(++ "\\n") . unwords . (map show)'
        if type_.encapsulated.main == TypeEnum.CHAR:
            return '(++ "\\n")'
        return 'foldr (++) "" . (map $ {})'.format(
            print_var_content(type_.encapsulated, structs))
    assert False
    return ''


class ParserHaskell():
    """Create the Haskell code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.imports = set()  # type: Set[str]
        self.main = []  # type: List[str]
        self.where = OrderedDict()  # type: OrderedDict[str, bool]
        self.method = []  # type: List[str]

        self.indentation = 2

    def declare_data(self) -> str:
        """Declare a Haskell data"""
        output = ""
        for data in self.input.structs:
            output += "-- | {}\n".format(data.comment)
            output += "data {0} = {0}\n".format(data_name(data.name))
            length_type = max(len(type_str(i[1])) for i in data.fields)
            length_name = max(len(var_name(i[0])) for i in data.fields)
            for i, field in enumerate(data.fields):
                output += "{}{} {: <{}} :: {: <{}}  -- ^ {}\n".format(
                    " " * self.indentation, '{' if i == 0 else ',',
                    var_name(field[0]), length_name, type_str(field[1]),
                    length_type, field[2])
            output += " " * self.indentation + "}\n\n"
        return output

    def read_line(self, type_: Type) -> str:
        """Read an entire line and parse it"""
        # pylint: disable=too-many-return-statements
        assert type_.fits_it_one_line(self.input.structs)
        if type_.main == TypeEnum.INT:
            return "fmap read getLine"
        if type_.main == TypeEnum.CHAR:
            return "fmap head getLine"
        if type_.main == TypeEnum.STR:
            return "getLine"
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.INT:
                return "fmap (map read . words) getLine"
            if type_.encapsulated.main == TypeEnum.CHAR:
                return "getLine"
            assert False
        if type_.main == TypeEnum.STRUCT:
            self.where[type_.struct_name] = True
            return "read{}".format(data_name(type_.struct_name))
        assert False
        return ""

    def read_lines(self, type_: Type) -> str:
        """Read one or several lines and parse them"""
        if type_.fits_it_one_line(self.input.structs):
            return self.read_line(type_)
        if type_.main == TypeEnum.STRUCT:
            self.where[type_.struct_name] = False
            return "read{}".format(data_name(type_.struct_name))
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            self.imports.add("Control.Monad (replicateM)")
            replicate = self.read_lines(type_.encapsulated)
            if len(replicate.split()) != 1:
                replicate = "$ " + replicate
            return "replicateM {} {}".format(var_name(type_.size), replicate)
        assert False
        return ""

    def read_var(self, var: Variable) -> None:
        """Read a variable"""
        self.main.append("{} <- {}".format(
            var_name(var.name), self.read_lines(var.type)))

    def call(self, reprint: bool) -> None:
        """Declare and call the function take all inputs in arguments"""
        name = var_name(self.input.name)
        length = len(name)
        if self.input.input:
            length_type = max(len(type_str(i.type)) for i in self.input.input)
            length_type = max(length_type, len("String"))
            for i, arg in enumerate(self.input.input):
                begin = "{} ::".format(
                    name) if i == 0 else " " * length + " ->"
                self.method.append("{} {: <{}}  -- ^ {}".format(
                    begin, type_str(arg.type), length_type, arg.comment))
            self.method.append(" " * length + " -> {: <{}}  -- ^ TODO".format(
                "String", length_type))
        else:
            self.method.append("{} :: String  -- ^ TODO".format(name))
        args = " ".join([var_name(i.name) for i in self.input.input])
        self.method.extend(
            ["-- " + i for i in textwrap.wrap(self.input.output, 76)])
        self.method.append("{} {} ={}".format(
            name, args, ' "TODO"' if not reprint else ""))
        self.main.append("{} $ {} {}".format(
            "putStrLn" if not reprint else "putStr", name,
            " ".join([var_name(i.name) for i in self.input.input])))
        if reprint:
            for var in self.input.input:
                self.method.append("{}({} $ {}) ++".format(
                    " " * self.indentation,
                    print_var_content(var.type, self.input.structs),
                    var_name(var.name)))
            self.method.append(" " * self.indentation + '""')

    def read_struct(self, name: str, oneline: bool) -> List[str]:
        """Generate the function to read a struct"""
        struct = self.input.get_struct(name)
        if oneline:
            args = ", ".join(chr(97 + i) for i in range(len(struct.fields)))
            parse = []
            for i, field in enumerate(struct.fields):
                if field[1].main == TypeEnum.INT:
                    parse.append("(read {})".format(chr(97 + i)))
                elif field[1].main == TypeEnum.CHAR:
                    parse.append("(head {})".format(chr(97 + i)))
                else:
                    assert False
            func = "{} {}".format(data_name(struct.name), " ".join(parse))
            return [
                "read{} = fmap ((\\[{}] -> {}) . words) getLine".format(
                    data_name(struct.name), args, func)
            ]
        self.imports.add("Control.Applicative ((<$>), (<*>))")
        args = " <*> ".join(self.read_lines(i[1]) for i in struct.fields)
        output = ["read{0} = {0} <$> {1}".format(data_name(struct.name), args)]
        return output

    def read_structs(self) -> List[str]:
        """Generate the functions to read the structs"""
        out = []
        done = 0
        while len(self.where) > done:
            last_size = len(self.where)
            for struct in list(self.where.items())[done:]:
                out.extend(self.read_struct(*struct))
            done = last_size
        return out

    def content(self) -> str:
        """Return the parser content"""
        where = self.read_structs()
        output = "".join("import {}\n".format(i) for i in sorted(self.imports))
        if self.imports:
            output += "\n"
        output += self.declare_data()
        for line in self.method:
            output += line + "\n"
        if self.method:
            output += "\n"
        output += "main :: IO ()\nmain = do"
        for line in self.main:
            output += "\n" + ' ' * self.indentation + line
        if where:
            output += "\n" + ' ' * self.indentation + "where"
            for line in where:
                output += "\n" + ' ' * self.indentation * 2 + line
        return output


def gen_haskell(input_data: Input, reprint: bool = False) -> str:
    """Generate a Haskell code to parse input"""
    parser = ParserHaskell(input_data)
    for var in input_data.input:
        parser.read_var(var)
    parser.call(reprint)
    return parser.content()
