# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generic types in a programming language"""

import re
from enum import Enum, unique
from typing import Any, Dict, List, Optional, Type as T, TypeVar, Union

# This stuff is no longer necessary with python 3.7 by including:
# from __future__ import annotations
TYPE = TypeVar('TYPE', bound='Type')
STRUCT = TypeVar('STRUCT', bound='Struct')
VAR = TypeVar('VAR', bound='Variable')
INPUT = TypeVar('INPUT', bound='Input')


@unique
class TypeEnum(Enum):
    """All supported variable types"""
    INT = 1
    CHAR = 2
    STR = 3
    LIST = 4
    STRUCT = 5


class Type:
    """Represents the type of a variable"""

    def __init__(self: TYPE,
                 enum: TypeEnum,
                 size: str = "",
                 encapsulated: Optional[TYPE] = None,
                 struct_name: str = "") -> None:
        self.main = enum
        self.size = size
        self.encapsulated = encapsulated
        self.struct_name = struct_name

    @classmethod
    def from_string(cls: T[TYPE], string: str) -> Optional[TYPE]:
        """Create a Type from a string"""
        if string == "int":
            return cls(TypeEnum.INT)
        if string == "char":
            return cls(TypeEnum.CHAR)
        if string[0] == '@':
            return cls(TypeEnum.STRUCT, struct_name=string[1:])
        prog = re.compile(
            r"""^(str|List)
                (\[([A-Za-z@][A-Za-z0-9\[\]\(\)@ ]*)\])?
                (\(([A-Za-z0-9 ]+)\))$""", re.VERBOSE)
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

    def __str__(self: TYPE) -> str:
        if self.main == TypeEnum.INT:
            return "int"
        if self.main == TypeEnum.CHAR:
            return "char"
        if self.main == TypeEnum.STR:
            return "str({})".format(self.size)
        if self.main == TypeEnum.LIST:
            return "List[{}]({})".format(self.encapsulated, self.size)
        if self.main == TypeEnum.STRUCT:
            return "@{}".format(self.struct_name)
        assert False
        return "UNKNOWN_TYPE"

    def can_be_inlined(self: TYPE) -> bool:
        """Can we parse several of this type on a single line"""
        return self.main in (TypeEnum.INT, TypeEnum.CHAR)

    def fits_it_one_line(self: TYPE, structs: List[STRUCT]) -> bool:
        """Return False if more than one line is needed for this struct"""
        if self.main in (TypeEnum.INT, TypeEnum.CHAR, TypeEnum.STR):
            return True
        if self.main == TypeEnum.LIST:
            assert self.encapsulated is not None
            return self.encapsulated.can_be_inlined()
        if self.main == TypeEnum.STRUCT:
            struct = next(x for x in structs if x.name == self.struct_name)
            if all(i.type.can_be_inlined() for i in struct.fields):
                two_chars_in_a_row = False
                for i in range(len(struct.fields) - 1):
                    if struct.fields[i].type.main == TypeEnum.CHAR and \
                       struct.fields[i + 1].type.main == TypeEnum.CHAR:
                        two_chars_in_a_row = True
                        break
                if not two_chars_in_a_row:
                    return True
            return False
        assert False
        return True


class Variable:
    """Everything there is to know about a variable"""

    def __init__(self: VAR, name: str, comment: str, type_: Type) -> None:
        self.name = name
        self.comment = comment
        self.type = type_

    def has_size(self) -> bool:
        """Does the variable has a size"""
        return self.type.main in (TypeEnum.STR, TypeEnum.LIST)

    @classmethod
    def from_dict(cls: T[VAR], dic: Dict[str, str]) -> Optional[VAR]:
        """Create a Variable from its YAML (dictionary) representation"""
        if "name" not in dic or "comment" not in dic or "type" not in dic:
            return None
        type_ = Type.from_string(dic["type"])
        if type_ is None:
            return None
        return cls(dic["name"], dic["comment"], type_)


class Struct:
    """Represent a struct (like in C)"""

    # pylint: disable=too-few-public-methods

    def __init__(self: STRUCT, name: str, comment: str,
                 fields: List[Variable]) -> None:
        self.name = name
        self.comment = comment
        self.fields = fields

    @classmethod
    def from_dict(cls: T[STRUCT],
                  dic: Dict[str, Union[str, List[Dict[str, str]]]]
                  ) -> Optional[STRUCT]:
        """Create a Struct from its YAML (dictionary) representation"""
        try:
            name = dic["name"]
            comment = dic["comment"]
            fields = dic["fields"]
            if not isinstance(name, str) or not isinstance(
                    comment, str) or not isinstance(fields, list):
                return None
            field_list = []  # type: List[Variable]
            for i in fields:
                var = Variable.from_dict(i)
                if var is None:
                    return None
                field_list.append(var)
            return cls(name, comment, field_list)
        except KeyError:
            return None


class Input():
    """Represents the user input, parsed"""

    def __init__(self: INPUT, name: str, structs: List[Struct],
                 inputs: List[Variable], subject: str, output: str) -> None:
        # pylint: disable=too-many-arguments
        self.name = name
        self.structs = structs
        self.input = inputs
        self.subject = subject
        self.output = output

    @classmethod
    def from_dict(cls: T[INPUT], dic: Dict[str, Any]) -> Optional[INPUT]:
        """Parse the input yaml"""
        try:
            structs = []  # type: List[Struct]
            if "structs" in dic:
                for node in dic["structs"]:
                    struct = Struct.from_dict(node)
                    if struct is None:
                        return None
                    structs.append(struct)
            variables = []  # type: List[Variable]
            for node in dic["input"]:
                variable = Variable.from_dict(node)
                if variable is None:
                    return None
                variables.append(variable)
            subject = dic["subject"] if "subject" in dic else ""
            return cls(dic["name"], structs, variables, subject, dic["output"])
        except KeyError:
            return None

    def get_struct(self: INPUT, name: str) -> Struct:
        """Get a struct by its name (or throw StopIteration)"""
        return next(x for x in self.structs if x.name == name)

    def get_var(self: INPUT, name: str) -> Variable:
        """Get a variable by its name."""
        return next(x for x in self.input if x.name == name)
