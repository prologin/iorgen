# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate a Haskll parser"""

import textwrap
from collections import OrderedDict
from typing import List, Set, Tuple  # pylint: disable=unused-import

from iorgen.types import Input, Struct, Type, TypeEnum, Variable
from iorgen.utils import camel_case, pascal_case

# keywords taken from wiki.haskell.org on 2018-10-26
KEYWORDS = [
    "as", "case", "of", "class", "data", "default", "deriving", "do", "forall",
    "foreign", "hiding", "if", "then", "else", "import", "infix", "infixl",
    "infixr", "instance", "let", "in", "mdo", "module", "newtype", "proc",
    "qualified", "rec", "type", "where"
]

# from autocompletion in GHCi version 8.6.1
PRELUDE = [
    "Applicative", "Bool", "Bounded", "Char", "Double", "EQ", "Either", "Enum",
    "Eq", "False", "FilePath", "Float", "Floating", "Foldable", "Fractional",
    "Functor", "GT", "IO", "IOError", "Int", "Integer", "Integral", "Just",
    "LT", "Left", "Maybe", "Monad", "Monoid", "Nothing", "Num", "Ord",
    "Ordering", "Rational", "Read", "ReadS", "Real", "RealFloat", "RealFrac",
    "Right", "Semigroup", "Show", "ShowS", "String", "Traversable", "True",
    "Word", "^", "^^", "abs", "acos", "acosh", "all", "and", "any",
    "appendFile", "asTypeOf", "asin", "asinh", "atan", "atan2", "atanh",
    "break", "ceiling", "compare", "concat", "concatMap", "const", "cos",
    "cosh", "curry", "cycle", "decodeFloat", "div", "divMod", "drop",
    "dropWhile", "either", "elem", "encodeFloat", "enumFrom", "enumFromThen",
    "enumFromThenTo", "enumFromTo", "error", "errorWithoutStackTrace", "even",
    "exp", "exponent", "fail", "filter", "flip", "floatDigits", "floatRadix",
    "floatRange", "floor", "fmap", "foldMap", "foldl", "foldl1", "foldr",
    "foldr1", "fromEnum", "fromInteger", "fromIntegral", "fromRational", "fst",
    "gcd", "getChar", "getContents", "getLine", "head", "id", "init",
    "interact", "ioError", "isDenormalized", "isIEEE", "isInfinite", "isNaN",
    "isNegativeZero", "iterate", "last", "lcm", "length", "lex", "lines",
    "log", "logBase", "lookup", "map", "mapM", "mapM_", "mappend", "max",
    "maxBound", "maximum", "maybe", "mconcat", "mempty", "min", "minBound",
    "minimum", "mod", "negate", "not", "notElem", "null", "odd", "or",
    "otherwise", "pi", "pred", "print", "product", "properFraction", "pure",
    "putChar", "putStr", "putStrLn", "quot", "quotRem", "read", "readFile",
    "readIO", "readList", "readLn", "readParen", "reads", "readsPrec",
    "realToFrac", "recip", "rem", "repeat", "replicate", "return", "reverse",
    "round", "scaleFloat", "scanl", "scanl1", "scanr", "scanr1", "seq",
    "sequence", "sequenceA", "sequence_", "show", "showChar", "showList",
    "showParen", "showString", "shows", "showsPrec", "significand", "signum",
    "sin", "sinh", "snd", "span", "splitAt", "sqrt", "subtract", "succ", "sum",
    "tail", "take", "takeWhile", "tan", "tanh", "toEnum", "toInteger",
    "toRational", "traverse", "truncate", "uncurry", "undefined", "unlines",
    "until", "unwords", "unzip", "unzip3", "userError", "words", "writeFile",
    "zip", "zip3", "zipWith", "zipWith3"
]


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for Haskell"""
    candidate = camel_case(name)
    if candidate in KEYWORDS:
        return candidate + "'"
    if candidate in PRELUDE + ["replicateM", "main"]:
        return candidate + "'"
    if candidate.startswith("read") and candidate[4].isupper():
        return candidate + "'"
    return candidate


def data_name(name: str) -> str:
    """Transform a data name into a valid one for Haskell"""
    candidate = pascal_case(name)
    if candidate in PRELUDE:
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
        if type_.fits_in_one_line(structs):
            fields = []
            for i in struct.fields:
                if i.type.main == TypeEnum.INT:
                    fields.append("(show $ {} r)".format(var_name(i.name)))
                else:
                    assert i.type.main == TypeEnum.CHAR
                    fields.append('((: "") $ {} r)'.format(var_name(i.name)))
            return '(\\r -> {} ++ "\\n")'.format(' ++ " " ++ '.join(fields))
        return "(\\r -> {})".format(" ++ ".join("({} $ {} r)".format(
            print_var_content(i.type, structs), var_name(i.name))
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
            length_type = max(len(type_str(i.type)) for i in data.fields)
            length_name = max(len(var_name(i.name)) for i in data.fields)
            for i, field in enumerate(data.fields):
                output += "{}{} {: <{}} :: {: <{}}  -- ^ {}\n".format(
                    " " * self.indentation, '{' if i == 0 else ',',
                    var_name(field.name), length_name, type_str(field.type),
                    length_type, field.comment)
            output += " " * self.indentation + "}\n\n"
        return output

    def read_line(self, type_: Type) -> str:
        """Read an entire line and parse it"""
        # pylint: disable=too-many-return-statements
        assert type_.fits_in_one_line(self.input.structs)
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

    def read_lines(self, type_: Type, size: str) -> str:
        """Read one or several lines and parse them"""
        if type_.fits_in_one_line(self.input.structs):
            return self.read_line(type_)
        if type_.main == TypeEnum.STRUCT:
            self.where[type_.struct_name] = False
            return "read{}".format(data_name(type_.struct_name))
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            self.imports.add("Control.Monad (replicateM)")
            replicate = self.read_lines(type_.encapsulated,
                                        var_name(type_.encapsulated.size))
            if len(replicate.split()) != 1:
                replicate = "$ " + replicate
            return "replicateM {} {}".format(size, replicate)
        assert False
        return ""

    def read_var(self, var: Variable) -> None:
        """Read a variable"""
        self.main.append("{} <- {}".format(
            var_name(var.name),
            self.read_lines(var.type, var_name(var.type.size))))

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
            self.method.append(
                " " * length +
                " -> {: <{}}  -- ^ TODO".format("String", length_type))
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
                if field.type.main == TypeEnum.INT:
                    parse.append("(read {})".format(chr(97 + i)))
                elif field.type.main == TypeEnum.CHAR:
                    parse.append("(head {})".format(chr(97 + i)))
                else:
                    assert False
            func = "{} {}".format(data_name(struct.name), " ".join(parse))
            return [
                "read{} = fmap ((\\[{}] -> {}) . words) getLine".format(
                    data_name(struct.name), args, func)
            ]
        need_size = struct.is_sized_struct() and struct.fields[
            1].type.main == TypeEnum.LIST and not struct.fields[
                1].type.fits_in_one_line(self.input.structs)
        if not need_size:
            self.imports.add("Control.Applicative ((<$>), (<*>))")
            self.imports.discard("Control.Applicative ((<$>))")
            args = " <*> ".join(
                self.read_lines(i.type, var_name(i.type.size))
                for i in struct.fields)
            return [
                "read{0} = {0} <$> {1}".format(data_name(struct.name), args)
            ]
        if "Control.Applicative ((<$>), (<*>))" not in self.imports:
            self.imports.add("Control.Applicative ((<$>))")
        field = struct.fields[1]
        return [
            "read{0} = fmap read getLine >>= \\a -> {0} a <$> ({1})".format(
                data_name(struct.name), self.read_lines(field.type, 'a'))
        ]

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
        output += "main :: IO ()\nmain = do\n"
        for line in self.main:
            output += ' ' * self.indentation + line + "\n"
        if where:
            output += ' ' * self.indentation + "where\n"
            for line in where:
                output += ' ' * self.indentation * 2 + line + "\n"
        return output


def gen_haskell(input_data: Input, reprint: bool = False) -> str:
    """Generate a Haskell code to parse input"""
    parser = ParserHaskell(input_data)
    for var in input_data.input:
        parser.read_var(var)
    parser.call(reprint)
    return parser.content()
