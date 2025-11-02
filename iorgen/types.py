# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2025 Sacha Delanoue
# Copyright 2019 Matthieu Moatti
# Copyright 2021 Kenji Gaillac
# Copyright 2022 Quentin Rataud
"""Generic types in a programming language"""

from __future__ import annotations
import re
from collections.abc import Callable, Iterator
from enum import Enum, unique
from typing import Any
from typing import Type as T

from .utils import number_int


@unique
class TypeEnum(Enum):
    """All supported variable types"""

    INT = 1
    FLOAT = 2
    CHAR = 3
    STR = 4
    LIST = 5
    STRUCT = 6


@unique
class FormatStyle(Enum):
    """Configurable formatting style option for the input"""

    DEFAULT = 0
    NO_ENDLINE = 1  # An integer that has no endline after it (an other follows)
    FORCE_NEWLINES = 2  # A integer list with each int on a new line


class Type:
    """Represents the type of a variable"""

    def __init__(
        self: Type,
        enum: TypeEnum,
        size: str = "",
        encapsulated: Type | None = None,
        struct_name: str = "",
    ) -> None:
        self.main = enum
        self.size = size
        self.can_be_empty = True
        self.encapsulated = encapsulated
        self.struct_name = struct_name

    @classmethod
    def from_string(cls: T[Type], string: str) -> Type | None:
        """Create a Type from a string"""
        if string in ("int", "float", "char"):
            return {
                "int": cls(TypeEnum.INT),
                "float": cls(TypeEnum.FLOAT),
                "char": cls(TypeEnum.CHAR),
            }[string]
        if string[0] == "@":
            return cls(TypeEnum.STRUCT, struct_name=string[1:])
        prog = re.compile(
            r"""^(str|List)
                (\[([A-Za-z@][A-Za-z0-9\[\]\(\)@ ]*)\])?
                (\(([A-Za-z0-9 ]+)\))$""",
            re.VERBOSE,
        )
        result = prog.match(string)
        if result:
            type_ = {
                "str": TypeEnum.STR,
                "List": TypeEnum.LIST,
            }[result.group(1)]

            inner = result.group(3)
            encapsulated = None if inner is None else cls.from_string(inner)

            if (encapsulated is None) == (type_ != TypeEnum.STR):
                return None

            size = result.group(5)
            return None if not size.strip() else cls(type_, size, encapsulated)
        return None

    def __str__(self: Type) -> str:
        if self.main == TypeEnum.INT:
            return "int"
        if self.main == TypeEnum.FLOAT:
            return "float"
        if self.main == TypeEnum.CHAR:
            return "char"
        if self.main == TypeEnum.STR:
            return f"str({self.size})"
        if self.main == TypeEnum.LIST:
            return f"List[{self.encapsulated}]({self.size})"
        if self.main == TypeEnum.STRUCT:
            return f"@{self.struct_name}"
        raise Exception

    def can_be_inlined(self: Type) -> bool:
        """Can we parse several of this type on a single line"""
        return self.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR)

    def fits_in_one_line(
        self: Type, structs: list[Struct], style: FormatStyle = FormatStyle.DEFAULT
    ) -> bool:
        """Return False if more than one line is needed for this type"""
        if self.main in (TypeEnum.INT, TypeEnum.FLOAT, TypeEnum.CHAR, TypeEnum.STR):
            return True
        if self.main == TypeEnum.LIST:
            assert self.encapsulated is not None
            return (
                style != FormatStyle.FORCE_NEWLINES
                and self.encapsulated.can_be_inlined()
            )
        if self.main == TypeEnum.STRUCT:
            struct = next(x for x in structs if x.name == self.struct_name)
            return all(i.type.can_be_inlined() for i in struct.fields)
        raise Exception

    def list_contained(self: Type) -> Type:
        """Return non-list type contained is list (or list of list, etc)"""
        if self.main != TypeEnum.LIST:
            return self
        inner = self.encapsulated
        assert inner is not None
        return inner.list_contained()


def get_min_value(var: int | float | Variable) -> int | float:
    """Get min value of an integer or a variable"""
    if isinstance(var, Variable):
        assert var.constraints is not None
        return var.constraints.min_possible()
    assert isinstance(var, (int, float))
    return var


def get_max_value(var: int | float | Variable) -> int | float:
    """Get min value of an integer or a variable"""
    if isinstance(var, Variable):
        assert var.constraints is not None
        return var.constraints.max_possible()
    assert isinstance(var, (int, float))
    return var


def get_number_or_var(
    var_name: str, type_: TypeEnum, variables: dict[str, Variable]
) -> int | float | Variable:
    """From a string, return either the corresponding var or number (int or float)"""
    assert isinstance(var_name, str)
    if var_name in variables:
        var = variables[var_name]
        assert var.type.main == type_
        return var
    if type_ == TypeEnum.INT:
        return int(var_name)
    if type_ == TypeEnum.FLOAT:
        return float(var_name)
    print(f"WARNING: only int or floats can have min or max values, not {type_.name}")
    return 0


def integer_bounds(
    name: str,
    min_: int | float | Variable,
    max_: int | float | Variable,
    is_size: bool,
) -> str:
    """Create a string to display an integer's bounds"""
    min_repr = ""
    if isinstance(min_, int):
        if min_ != Constraints.MIN_INT:
            min_repr = number_int(max(0, min_) if is_size else min_)
        elif is_size:
            min_repr = "0"
    elif isinstance(min_, float) and min_ != Constraints.MIN_FLOAT:
        min_repr = number_int(min_)
    elif isinstance(min_, Variable):
        assert min_.type.main in (TypeEnum.INT, TypeEnum.FLOAT)
        assert min_.constraints
        min_repr = min_.name
        if is_size and min_.constraints.min_possible() < 0:
            min_repr = "0, " + min_repr
    max_repr = ""
    if isinstance(max_, int) and max_ != Constraints.MAX_INT:
        max_repr = number_int(max_)
    elif isinstance(max_, float) and max_ != Constraints.MAX_FLOAT:
        max_repr = number_int(max_)
    elif isinstance(max_, Variable):
        max_repr = max_.name

    if not min_repr and not max_repr:
        return ""
    out = name
    if min_repr:
        out = min_repr + r" \le " + out
    if max_repr:
        out = out + r" \le " + max_repr
    return out


class Constraints:
    """Constraints values for a variable"""

    # 32 bits signed integer. This should be supported by all generators.
    MAX_INT = 2147483647
    MIN_INT = -2147483648

    # We currently limit 15 maximum digits, plus "-" and "." chars
    MAX_FLOAT = 1e15 - 1
    MIN_FLOAT = -1e15 + 1

    def __init__(
        self, type_: TypeEnum, dic: dict[str, Any], variables: dict[str, Variable]
    ) -> None:
        self.type_bounds = (
            (self.MIN_FLOAT, self.MAX_FLOAT)
            if type_ == TypeEnum.FLOAT
            else (self.MIN_INT, self.MAX_INT)
        )
        self.min = self.type_bounds[0]  # type: int | float | Variable
        self.max = self.type_bounds[1]  # type: int | float | Variable
        self.min_perf = self.type_bounds[0]  # type: int | float | Variable
        self.max_perf = self.type_bounds[1]  # type: int | float | Variable
        self.choices = set()  # type: set[int | float | str]
        self.is_size = False

        if "min" in dic:
            self.min = get_number_or_var(str(dic["min"]), type_, variables)
            self.min_perf = self.min
        if "min_perf" in dic:
            self.min_perf = get_number_or_var(str(dic["min_perf"]), type_, variables)
        if "max" in dic:
            self.max = get_number_or_var(str(dic["max"]), type_, variables)
            self.max_perf = self.max
        if "max_perf" in dic:
            self.max_perf = get_number_or_var(str(dic["max_perf"]), type_, variables)
        if "choices" in dic:
            if type_ == TypeEnum.INT:
                self.choices = set(map(int, dic["choices"]))
            elif type_ == TypeEnum.FLOAT:
                self.choices = set(map(float, dic["choices"]))
            else:
                self.choices = {i[0] for i in dic["choices"]}

    def min_possible(self) -> int | float:
        """Return the minimal possible value for an integer"""
        if self.choices:
            value = min(self.choices)
            assert isinstance(value, (int, float))
            return value
        value = self.type_bounds[1]
        for min_ in (self.min, self.min_perf):
            value = min(value, get_min_value(min_))
        return max(0, value) if self.is_size else value

    def max_possible(self) -> int | float:
        """Return the maximal possible value for an integer"""
        if self.choices:
            value = max(self.choices)
            assert isinstance(value, (int, float))
            return value
        value = self.type_bounds[0]
        for max_ in (self.max, self.max_perf):
            value = max(value, get_max_value(max_))
        return value

    def simple_repr(self, name: str) -> str:
        """Return mathjax representation of integer bounds"""
        if self.choices:
            return (
                rf"{name} \in \{{{', '.join(str(i) for i in sorted(self.choices))}\}}"
            )
        return integer_bounds(name, self.min, self.max, self.is_size)

    def perf_repr(self, name: str) -> str:
        """Return text representation of perf integer bounds"""
        if self.min == self.min_perf and self.max == self.max_perf:
            return ""
        if self.choices:
            return ""
        return integer_bounds(name, self.min_perf, self.max_perf, self.is_size)


class Variable:
    """Everything there is to know about a variable"""

    def __init__(
        self: Variable,
        name: str,
        comment: str,
        type_: Type,
        format_style: FormatStyle = FormatStyle.DEFAULT,
    ) -> None:
        self.name = name
        self.comment = comment
        self.type = type_
        self.constraints = None  # type: Constraints | None
        self.format_style = format_style

    @classmethod
    def from_dict(cls: T[Variable], dic: dict[str, str]) -> Variable | None:
        """Create a Variable from its YAML (dictionary) representation"""
        if "name" not in dic or "comment" not in dic or "type" not in dic:
            return None
        type_ = Type.from_string(dic["type"])
        if type_ is None:
            return None
        style = FormatStyle.DEFAULT
        if "format" in dic:
            if dic["format"] == "no_endline":
                if type_.main not in (TypeEnum.INT, TypeEnum.FLOAT):
                    return None
                style = FormatStyle.NO_ENDLINE
            elif dic["format"] == "force_newlines":
                if (
                    type_.main != TypeEnum.LIST
                    or type_.encapsulated is None
                    or type_.encapsulated.main not in (TypeEnum.INT, TypeEnum.FLOAT)
                ):
                    return None
                style = FormatStyle.FORCE_NEWLINES
            else:
                print(f'WARNING: unknown format "{dic["format"]}')
        return cls(dic["name"], dic["comment"], type_, format_style=style)

    def constraints_repr(self, perf: bool = False) -> str:
        """Return a string representing the variable's constraints"""
        type_ = self.type.list_contained().main
        if type_ == TypeEnum.STRUCT:
            return ""
        assert self.constraints
        name = self.name
        if type_ == TypeEnum.STR:
            type_ = TypeEnum.CHAR
        if self.type.main in (TypeEnum.LIST, TypeEnum.STR):
            if " " in name:
                name = "(" + name + ")"
            loop = self.type
            while loop.main in (TypeEnum.LIST, TypeEnum.STR):
                name += "[ ]"
                if loop.main == TypeEnum.STR:
                    break
                loop = loop.list_contained()

        if type_ in (TypeEnum.INT, TypeEnum.FLOAT):
            return (
                self.constraints.perf_repr(name)
                if perf
                else self.constraints.simple_repr(name)
            )
        if type_ == TypeEnum.CHAR:
            choices = self.constraints.choices
            if choices and not perf:
                return rf"{name} \in \{{{', '.join(str(i) for i in sorted(choices))}\}}"
            return ""
        raise Exception

    def fits_in_one_line(self, structs: list[Struct]) -> bool:
        """Return False if more than one line is needed for this variable"""
        return self.type.fits_in_one_line(structs, self.format_style)


class Struct:
    """Represent a struct (like in C)"""

    def __init__(self: Struct, name: str, comment: str, fields: list[Variable]) -> None:
        self.name = name
        self.comment = comment
        self.fields = fields

    @classmethod
    def from_dict(
        cls: T[Struct], dic: dict[str, str | list[dict[str, str]]]
    ) -> Struct | None:
        """Create a Struct from its YAML (dictionary) representation"""
        try:
            name = dic["name"]
            comment = dic["comment"]
            fields = dic["fields"]
            if (
                not isinstance(name, str)
                or not isinstance(comment, str)
                or not isinstance(fields, list)
            ):
                return None
            field_list = []  # type: list[Variable]
            for i in fields:
                var = Variable.from_dict(i)
                if var is None:
                    return None
                field_list.append(var)
            return cls(name, comment, field_list)
        except KeyError:
            return None

    def is_sized_struct(self) -> bool:
        """A special kind of struct: first field is the size of the second"""
        return (
            len(self.fields) == 2
            and self.fields[0].type.main == TypeEnum.INT
            and self.fields[1].type.main in (TypeEnum.STR, TypeEnum.LIST)
            and self.fields[0].name == self.fields[1].type.size
        )

    def fields_name_type_size(
        self, format_spec: str, var_name: Callable[[str], str]
    ) -> Iterator[tuple[str, Type, str]]:
        """Return name, type and size for each field"""
        types = [var_name(field.type.size) for field in self.fields]
        if self.is_sized_struct():
            types = ["", format_spec.format(var_name(self.fields[0].name))]
        for field in zip(self.fields, types):
            yield (format_spec.format(var_name(field[0].name)), field[0].type, field[1])


def process_sized_type(type_: Type, variables: dict[str, Variable]) -> None:
    """Set 'is_size' and 'can_be_empty' booleans"""
    if type_.main not in (TypeEnum.LIST, TypeEnum.STR):
        return
    min_ = 0
    if type_.size in variables:
        constraints = variables[type_.size].constraints
        assert constraints
        constraints.is_size = True
        min_possible = constraints.min_possible()
        assert isinstance(min_possible, int)
        min_ = min_possible
    else:
        min_ = int(type_.size)
    if min_ > 0:
        type_.can_be_empty = False
    if type_.main == TypeEnum.LIST:
        assert type_.encapsulated is not None
        process_sized_type(type_.encapsulated, variables)


def set_constraints(
    variables: dict[str, Variable], dicts: list[dict[str, Any]]
) -> None:
    """Set constraints for all variables"""
    # First create the constraints for all variables
    for dic in dicts:
        var = variables[dic["name"]]
        # Only structures do not have constraints, because their fields have
        # constraints of their own
        if var.type.list_contained().main != TypeEnum.STRUCT:
            var.constraints = Constraints(
                var.type.list_contained().main, dic, variables
            )
    # Now let the variables used as size know there are
    for var in variables.values():
        process_sized_type(var.type, variables)


class Input:
    """Represents the user input, parsed"""

    def __init__(
        self: Input,
        name: str,
        structs: list[Struct],
        inputs: list[Variable],
        subject: str,
        output: str,
    ) -> None:
        # pylint: disable=too-many-arguments
        # pylint: disable=too-many-positional-arguments
        self.name = name
        self.structs = structs
        self.input = inputs
        self.subject = subject
        self.output = output

    @classmethod
    def from_dict(cls: T[Input], dic: dict[str, Any]) -> Input | None:
        """Parse the input yaml"""
        # pylint: disable=too-many-branches
        # We'll be able to reactivate the too-many-branches check when we will
        # remove the `return None` and use raise instead
        try:
            variables_lookup = {}
            variables_dicts = []  # type: list[dict[str, Any]]
            structs = []  # type: list[Struct]
            if "structs" in dic:
                for node in dic["structs"]:
                    struct = Struct.from_dict(node)
                    if struct is None:
                        return None
                    structs.append(struct)
                    for var in struct.fields:
                        if var.name in variables_lookup:
                            raise ValueError(
                                f'Several struct fields are called "{var.name}"'
                            )
                        variables_lookup[var.name] = var
                    variables_dicts.extend(node["fields"])
            variables = []  # type: list[Variable]
            for node in dic["input"]:
                variable = Variable.from_dict(node)
                if variable is None:
                    return None
                variables.append(variable)
                if variable.name in variables_lookup:
                    raise ValueError(
                        "Several variables or struct fields "
                        f'are called "{variable.name}"'
                    )
                variables_lookup[variable.name] = variable
                variables_dicts.append(node)
            for name in variables_lookup:
                if not re.fullmatch("[a-zA-Z][a-zA-Z0-9 ]*", name):
                    raise ValueError(
                        f'Variable name "{name}" should match [a-zA-Z][a-z0-9A-Z ]*'
                    )
            set_constraints(variables_lookup, variables_dicts)
            subject = dic["subject"] if "subject" in dic else ""
            if "function_name" not in dic and "name" in dic:
                print('WARNING: "name" is deprecated, use "function_name"')
                dic["function_name"] = dic["name"]
            if not re.fullmatch("[a-z][a-z0-9 ]*", dic["function_name"]):
                raise ValueError("Field `function_name` should match [a-z][a-z0-9 ]*")
            return cls(dic["function_name"], structs, variables, subject, dic["output"])
        except KeyError:
            return None

    def get_struct(self: Input, name: str) -> Struct:
        """Get a struct by its name (or throw StopIteration)"""
        return next(x for x in self.structs if x.name == name)

    def get_var(self: Input, name: str) -> Variable:
        """Get a variable by its name."""
        return next(x for x in self.input if x.name == name)

    def get_all_vars(self: Input) -> list[list[Variable]]:
        """Return input variables. The difference from this method and simply the
        'input' field, is that this method will group integers that are read on the
        same line."""
        ret = []
        current = []
        for var in self.input:
            current.append(var)
            if var.format_style != FormatStyle.NO_ENDLINE:
                ret.append(current)
                current = []
        return ret

    def type_contains_float(self: Input, type_: Type) -> bool:
        """Does this type contains a float in any way (inside a list, a struct...)?"""
        type_ = type_.list_contained()
        if type_.main == TypeEnum.FLOAT:
            return True
        if type_.main == TypeEnum.STRUCT:
            struct = self.get_struct(type_.struct_name)
            for var in struct.fields:
                if self.type_contains_float(var.type):
                    return True
        return False

    def contains_float(self: Input) -> bool:
        """Does this input contains a float in any way (inside a list, a struct...)?"""
        for var in self.input:
            if self.type_contains_float(var.type):
                return True
        return False
