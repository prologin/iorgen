# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2019 Sacha Delanoue
"""Check that a YAML input is well formed"""

import re
from typing import Any, Dict, List, TextIO, Union

import yaml

from iorgen.types import Input, Struct, Type, Variable


def error_parse_type(string: str) -> str:
    """Give a hint explaining why a type fails to parse"""
    # pylint: disable=too-many-return-statements
    assert Type.from_string(string) is None
    lower = string.lower()
    if string.strip() != string:
        return "leading/trailing whitespaces"
    if lower.startswith("int") or lower.startswith("uint"):
        return "the integer type is 'int'"
    if lower.startswith("char"):
        return "the char type is 'char'"
    if (lower.startswith("str")
            or lower.startswith("string")) and not string.startswith("str"):
        return "the string type is 'str(size)'"
    if string.startswith("str") and (string[:-1] != ")" or string[3] != "("):
        return "a string must specify its size with parenthesis 'str(size)'"
    if string.startswith("str"):
        return "invalid string size, can only be alphanumeric characters " + \
               "and spaces"
    if lower[0] == '[' or (lower.startswith("list")
                           and not string.startswith("List")):
        return "the list type is 'List[type](size)'"
    if string.startswith("List"):
        match = re.match(r"List\[(.*)\]\((.*)\)", string)
        if not match:
            return "a list must specify a type and a size 'List[type](size)'"
        if not re.match(r"[A-Za-z0-9 ]+",
                        match.group(2)) or not match.group(2).strip():
            return "invalid list size, can only be alphanumeric characters" + \
                   " and spaces"
        return "can not parse type list: " + error_parse_type(match.group(1))
    return "should be either 'int', 'char', 'str(size)', " + \
           "'List[type](size)' or '@struct_name'"


def error_parse_variable(dic: Dict[str, str]) -> str:
    """Explain why we a variable fails to parse"""
    assert Variable.from_dict(dic) is None
    if "name" not in dic:
        return "missing name field"
    if not isinstance(dic["name"], str):
        return "name field is not a string"
    for field in ("comment", "type"):
        if field not in dic:
            return "missing {} field for {}".format(field, dic["name"])
        if not isinstance(dic[field], str):
            return "{} field for {} is not a string".format(field, dic["name"])
    if Type.from_string(dic["type"]) is None:
        return "unable to parse type {} for {}: {}".format(
            dic["type"], dic["name"], error_parse_type(dic["type"]))
    return "unknown error"


def error_parse_struct(dic: Dict[str, Union[str, List[Dict[str, str]]]]
                       ) -> str:
    """Explain why we a struct fails to parse"""
    # pylint: disable=too-many-return-statements
    assert Struct.from_dict(dic) is None
    if "name" not in dic:
        return "missing name field"
    if not isinstance(dic["name"], str):
        return "name field is not a string"
    for field in ("comment", "fields"):
        if field not in dic:
            return "missing {} field for {}".format(field, dic["name"])
    if not isinstance(dic["comment"], str):
        return "{} field for {} is not a string".format(field, dic["name"])
    if not isinstance(dic["fields"], str):
        return "fields field for {} is not a list".format(dic["name"])
    for i in dic["fields"]:
        if not isinstance(i, dict):
            return "a field for {}.fields is not a map".format(dic["name"])
        if Variable.from_dict(i) is None:
            return "failed to parse field of {}: {}".format(
                dic["name"], error_parse_struct(i))
    return "unknown error"


def error_parse_input(dic: Dict[str, Any]) -> str:
    """Explain why we an input fails to parse"""
    # pylint: disable=too-many-return-statements
    # pylint: disable=too-many-branches
    assert Input.from_dict(dic) is None
    if "function_name" not in dic:
        if "name" in dic:
            dic["function_name"] = dic["name"]
        else:
            return "missing function_name field"
    if not isinstance(dic["function_name"], str):
        return "function_name is not a string"
    if "structs" in dic:  # non mandatory
        if not isinstance(dic["structs"], list):
            return "'structs' is not a list"
        for node in dic["structs"]:
            if Struct.from_dict(node) is None:
                return "failed to parse struct: " + error_parse_struct(node)
    if "input" not in dic:
        return "missing input field"
    if not isinstance(dic["input"], list):
        return "output is not a list"
    for node in dic["input"]:
        if Variable.from_dict(node) is None:
            return "failed to parse variable: " + error_parse_variable(node)
    if "output" not in dic:
        return "missing output field"
    if not isinstance(dic["output"], str):
        return "output is not a string"
    return "unknown error"


def input_from_dict(dic: Dict[str, Any]) -> Input:
    """Parse a input from a dict, or return a ValueError"""
    value = Input.from_dict(dic)
    if value is None:
        raise ValueError('Unable to parse input: ' + error_parse_input(dic))
    return value


def parse_input(stream: TextIO) -> Input:
    """Parse a input from a file stream, or return a ValueError"""
    return input_from_dict(yaml.safe_load(stream))
