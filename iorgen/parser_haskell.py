# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2025 Sacha Delanoue
"""Generate a Haskell parser"""

import textwrap
from collections import OrderedDict

from iorgen.types import FormatStyle, Input, Struct, Type, TypeEnum
from iorgen.utils import camel_case, pascal_case


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
    if type_.main == TypeEnum.FLOAT:
        return "Double"
    if type_.main == TypeEnum.CHAR:
        return "Char"
    if type_.main == TypeEnum.STR:
        return "String"
    if type_.main == TypeEnum.STRUCT:
        return data_name(type_.struct_name)
    assert type_.main == TypeEnum.LIST
    assert type_.encapsulated
    return f"[{type_str(type_.encapsulated)}]"


def print_var_content(type_: Type, structs: list[Struct], style: FormatStyle) -> str:
    """Return Haskell function to print a variable of given type"""
    if type_.main == TypeEnum.STRUCT:
        struct = next(x for x in structs if x.name == type_.struct_name)
        if type_.fits_in_one_line(structs, style):
            fields = []
            for i in struct.fields:
                if i.type.main in (TypeEnum.INT, TypeEnum.FLOAT):
                    show = "show" if i.type.main == TypeEnum.INT else "iorgen_float"
                    fields.append(f"({show} $ {var_name(i.name)} r')")
                else:
                    assert i.type.main == TypeEnum.CHAR
                    fields.append(f"((: []) $ {var_name(i.name)} r')")
            return '(\\r\' -> {} ++ "\\n")'.format(' ++ " " ++ '.join(fields))
        return "(\\r' -> {})".format(
            " ++ ".join(
                "({} $ {} r')".format(
                    print_var_content(i.type, structs, FormatStyle.DEFAULT),
                    var_name(i.name),
                )
                for i in struct.fields
            )
        )
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated
        if (
            type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT)
            and style != FormatStyle.FORCE_NEWLINES
        ):
            show = "show" if type_.encapsulated.main == TypeEnum.INT else "iorgen_float"
            return f'(++ "\\n") . unwords . (map {show})'
        if type_.encapsulated.main == TypeEnum.CHAR:
            return '(++ "\\n")'
        return 'foldr (++) "" . (map $ {})'.format(
            print_var_content(type_.encapsulated, structs, FormatStyle.DEFAULT)
        )
    newline = '" "' if style == FormatStyle.NO_ENDLINE else r'"\n"'
    return {
        TypeEnum.INT: f"(++ {newline}) . show",
        TypeEnum.FLOAT: f"(++ {newline}) . iorgen_float",
        TypeEnum.CHAR: f"(: {newline})",
        TypeEnum.STR: f"(++ {newline})",
    }[type_.main]


class ParserHaskell:
    """Create the Haskell code to parse an input"""

    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.imports = set()  # type: set[str]
        self.main = []  # type: list[str]
        self.where = OrderedDict()  # type: OrderedDict[str, bool]
        self.method = []  # type: list[str]

        self.indentation = 2

    def declare_data(self) -> str:
        """Declare a Haskell data"""
        output = ""
        for data in self.input.structs:
            output += f"-- | {data.comment}\n"
            output += "data {0} = {0}\n".format(data_name(data.name))
            length_type = max(len(type_str(i.type)) for i in data.fields)
            length_name = max(len(var_name(i.name)) for i in data.fields)
            for i, field in enumerate(data.fields):
                output += "{}{} {: <{}} :: {: <{}}  -- ^ {}\n".format(
                    " " * self.indentation,
                    "{" if i == 0 else ",",
                    var_name(field.name),
                    length_name,
                    type_str(field.type),
                    length_type,
                    field.comment,
                )
            output += " " * self.indentation + "}\n\n"
        return output

    def read_line(self, type_: Type) -> str:
        """Read an entire line and parse it"""
        assert type_.fits_in_one_line(self.input.structs)
        if type_.main in (TypeEnum.INT, TypeEnum.FLOAT):
            return "fmap read getLine"
        if type_.main == TypeEnum.CHAR:
            return "fmap head getLine"
        if type_.main == TypeEnum.STR:
            return "getLine"
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main in (TypeEnum.INT, TypeEnum.FLOAT):
                return "fmap (map read . words) getLine"
            assert type_.encapsulated.main == TypeEnum.CHAR
            return "getLine"
        assert type_.main == TypeEnum.STRUCT
        self.where[type_.struct_name] = True
        return f"read{data_name(type_.struct_name)}"

    def read_lines(
        self, type_: Type, size: str, style: FormatStyle = FormatStyle.DEFAULT
    ) -> str:
        """Read one or several lines and parse them"""
        if type_.fits_in_one_line(self.input.structs, style):
            return self.read_line(type_)
        if type_.main == TypeEnum.STRUCT:
            self.where[type_.struct_name] = False
            return f"read{data_name(type_.struct_name)}"
        assert type_.main == TypeEnum.LIST
        assert type_.encapsulated is not None
        self.imports.add("Control.Monad (replicateM)")
        replicate = self.read_lines(
            type_.encapsulated, var_name(type_.encapsulated.size)
        )
        if len(replicate.split()) != 1:
            if "$" in replicate:
                replicate = f"({replicate})"
            else:
                replicate = "$ " + replicate
        return f"replicateM {size} {replicate}"

    def read_vars(self) -> None:
        """Read all input variables"""
        for variables in self.input.get_all_vars():
            if len(variables) == 1:
                var = variables[0]
                self.main.append(
                    var_name(var.name)
                    + " <- "
                    + self.read_lines(
                        var.type, var_name(var.type.size), var.format_style
                    )
                )
            else:
                assert all(var.type.main == TypeEnum.INT for var in variables)
                self.main.append(
                    f"[{', '.join(var_name(i.name) for i in variables)}] <- "
                    "fmap (map read . words) getLine"
                )

    def call(self, reprint: bool) -> None:
        """Declare and call the function take all inputs in arguments"""
        name = var_name(self.input.name)
        length = len(name)
        if self.input.input:
            length_type = max(len(type_str(i.type)) for i in self.input.input)
            length_type = max(length_type, len("String"))
            for i, arg in enumerate(self.input.input):
                begin = f"{name} ::" if i == 0 else " " * length + " ->"
                self.method.append(
                    "{} {: <{}}  -- ^ {}".format(
                        begin, type_str(arg.type), length_type, arg.comment
                    )
                )
            self.method.append(
                " " * length + " -> {: <{}}  -- ^ TODO".format("String", length_type)
            )
        else:
            self.method.append(f"{name} :: String  -- ^ TODO")
        args = " ".join([var_name(i.name) for i in self.input.input])
        self.method.extend(["-- " + i for i in textwrap.wrap(self.input.output, 76)])
        self.method.append(
            "{} {} ={}".format(name, args, ' "TODO"' if not reprint else "")
        )
        self.main.append(
            "{} $ {} {}".format(
                "putStrLn" if not reprint else "putStr",
                name,
                " ".join([var_name(i.name) for i in self.input.input]),
            )
        )
        if reprint:
            for var in self.input.input:
                self.method.append(
                    "{}({} $ {}) ++".format(
                        " " * self.indentation,
                        print_var_content(
                            var.type, self.input.structs, var.format_style
                        ),
                        var_name(var.name),
                    )
                )
            self.method.append(" " * self.indentation + '""')

    def read_struct(self, name: str, oneline: bool) -> list[str]:
        """Generate the function to read a struct"""
        struct = self.input.get_struct(name)
        if oneline:
            args = ", ".join(chr(97 + i) for i in range(len(struct.fields)))
            parse = []
            for i, field in enumerate(struct.fields):
                if field.type.main in (TypeEnum.INT, TypeEnum.FLOAT):
                    parse.append(f"(read {chr(97 + i)})")
                else:
                    assert field.type.main == TypeEnum.CHAR
                    parse.append(f"(head {chr(97 + i)})")
            func = "{} {}".format(data_name(struct.name), " ".join(parse))
            return [
                "read{} = fmap ((\\[{}] -> {}) . words) getLine".format(
                    data_name(struct.name), args, func
                )
            ]
        need_size = (
            struct.is_sized_struct()
            and struct.fields[1].type.main == TypeEnum.LIST
            and not struct.fields[1].type.fits_in_one_line(self.input.structs)
        )
        if not need_size:
            args = " <*> ".join(
                self.read_lines(i.type, var_name(i.type.size)) for i in struct.fields
            )
            return ["read{0} = {0} <$> {1}".format(data_name(struct.name), args)]
        field = struct.fields[1]
        return [
            "read{0} = fmap read getLine >>= \\a -> {0} a <$> ({1})".format(
                data_name(struct.name), self.read_lines(field.type, "a")
            )
        ]

    def read_structs(self) -> list[str]:
        """Generate the functions to read the structs"""
        out = []
        done = 0
        while len(self.where) > done:
            last_size = len(self.where)
            for struct in list(self.where.items())[done:]:
                out.extend(self.read_struct(*struct))
            done = last_size
        return out

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        self.read_vars()
        self.call(reprint)
        where = self.read_structs()
        output = "".join(f"import {i}\n" for i in sorted(self.imports))
        if self.imports:
            output += "\n"
        if reprint and self.input.contains_float():
            # Since Haskell does not conform to the C-like behavior of format("%.15g"),
            # we need to reimplement this behavior.
            # The spec is: use scietific notation if exposant is < -4 or >= 15
            output += """import qualified Numeric as Iorgen_Num (floatToDigits)

iorgen_float :: RealFloat p => p -> String
iorgen_float x = if x == 0 then "0" else if x < 0 then '-' : (f $ -x) else f x
  where
      f x =
        let (l, e) = Iorgen_Num.floatToDigits 10 x in
        if e - 1 < (-4) || e - 1 >= 15 then
          g l 1 ++ ('e' : (if e < 0 then (if e > -9 then ("-0" ++) . tail else id)
            else ('+' :)) (show (e - 1)))
        else
          g (replicate (1 - e) 0 ++ l) (max 1 e)
      g [] e = replicate e '0'
      g (h:t) e = show h !! 0 : (if e /= 1 || null t then id else ('.' :)) (g t (e - 1))

"""

        output += self.declare_data()
        for line in self.method:
            output += line + "\n"
        if self.method:
            output += "\n"
        output += "main :: IO ()\nmain = do\n"
        for line in self.main:
            output += " " * self.indentation + line + "\n"
        if where:
            output += " " * self.indentation + "where\n"
            for line in where:
                output += " " * self.indentation * 2 + line + "\n"
        return output


def gen_haskell(input_data: Input, reprint: bool = False) -> str:
    """Generate a Haskell code to parse input"""
    return ParserHaskell(input_data).content(reprint)


# keywords taken from wiki.haskell.org on 2018-10-26
KEYWORDS = [
    "as",
    "case",
    "of",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "forall",
    "foreign",
    "hiding",
    "if",
    "then",
    "else",
    "import",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "in",
    "mdo",
    "module",
    "newtype",
    "proc",
    "qualified",
    "rec",
    "type",
    "where",
]

# from autocompletion in GHCi version 8.6.1
PRELUDE = [
    "Applicative",
    "Bool",
    "Bounded",
    "Char",
    "Double",
    "EQ",
    "Either",
    "Enum",
    "Eq",
    "False",
    "FilePath",
    "Float",
    "Floating",
    "Foldable",
    "Fractional",
    "Functor",
    "GT",
    "IO",
    "IOError",
    "Int",
    "Integer",
    "Integral",
    "Just",
    "LT",
    "Left",
    "Maybe",
    "Monad",
    "Monoid",
    "Nothing",
    "Num",
    "Ord",
    "Ordering",
    "Rational",
    "Read",
    "ReadS",
    "Real",
    "RealFloat",
    "RealFrac",
    "Right",
    "Semigroup",
    "Show",
    "ShowS",
    "String",
    "Traversable",
    "True",
    "Word",
    "^",
    "^^",
    "abs",
    "acos",
    "acosh",
    "all",
    "and",
    "any",
    "appendFile",
    "asTypeOf",
    "asin",
    "asinh",
    "atan",
    "atan2",
    "atanh",
    "break",
    "ceiling",
    "compare",
    "concat",
    "concatMap",
    "const",
    "cos",
    "cosh",
    "curry",
    "cycle",
    "decodeFloat",
    "div",
    "divMod",
    "drop",
    "dropWhile",
    "either",
    "elem",
    "encodeFloat",
    "enumFrom",
    "enumFromThen",
    "enumFromThenTo",
    "enumFromTo",
    "error",
    "errorWithoutStackTrace",
    "even",
    "exp",
    "exponent",
    "fail",
    "filter",
    "flip",
    "floatDigits",
    "floatRadix",
    "floatRange",
    "floor",
    "fmap",
    "foldMap",
    "foldl",
    "foldl1",
    "foldr",
    "foldr1",
    "fromEnum",
    "fromInteger",
    "fromIntegral",
    "fromRational",
    "fst",
    "gcd",
    "getChar",
    "getContents",
    "getLine",
    "head",
    "id",
    "init",
    "interact",
    "ioError",
    "isDenormalized",
    "isIEEE",
    "isInfinite",
    "isNaN",
    "isNegativeZero",
    "iterate",
    "last",
    "lcm",
    "length",
    "lex",
    "lines",
    "log",
    "logBase",
    "lookup",
    "map",
    "mapM",
    "mapM_",
    "mappend",
    "max",
    "maxBound",
    "maximum",
    "maybe",
    "mconcat",
    "mempty",
    "min",
    "minBound",
    "minimum",
    "mod",
    "negate",
    "not",
    "notElem",
    "null",
    "odd",
    "or",
    "otherwise",
    "pi",
    "pred",
    "print",
    "product",
    "properFraction",
    "pure",
    "putChar",
    "putStr",
    "putStrLn",
    "quot",
    "quotRem",
    "read",
    "readFile",
    "readIO",
    "readList",
    "readLn",
    "readParen",
    "reads",
    "readsPrec",
    "realToFrac",
    "recip",
    "rem",
    "repeat",
    "replicate",
    "return",
    "reverse",
    "round",
    "scaleFloat",
    "scanl",
    "scanl1",
    "scanr",
    "scanr1",
    "seq",
    "sequence",
    "sequenceA",
    "sequence_",
    "show",
    "showChar",
    "showList",
    "showParen",
    "showString",
    "shows",
    "showsPrec",
    "significand",
    "signum",
    "sin",
    "sinh",
    "snd",
    "span",
    "splitAt",
    "sqrt",
    "subtract",
    "succ",
    "sum",
    "tail",
    "take",
    "takeWhile",
    "tan",
    "tanh",
    "toEnum",
    "toInteger",
    "toRational",
    "traverse",
    "truncate",
    "uncurry",
    "undefined",
    "unlines",
    "until",
    "unwords",
    "unzip",
    "unzip3",
    "userError",
    "words",
    "writeFile",
    "zip",
    "zip3",
    "zipWith",
    "zipWith3",
]
