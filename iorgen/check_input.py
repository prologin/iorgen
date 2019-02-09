# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
# Copyright 2019 Matthieu Moatti
"""Check that a YAML input is well formed"""

import re
from typing import Any, Dict, List, TextIO, Union

import yaml

import warnings

from iorgen.types import Input, Struct, Type, Variable

class Counter:
    errors = 0
    warnings = 0

def increase_error() -> None:
    """Increase error counter"""
    Counter.errors += 1

def print_error(msg: str) -> None:
    """Print error and increase error counter"""
    print('ERROR: ' + msg)
    increase_error()

def increase_warning() -> None:
    """Increase warning counter"""
    Counter.warnings += 1

def print_warning(msg: str) -> None:
    """Print warning and increase warning counter"""
    print('WARNING: ' + msg)
    increase_warning()

def check_names(name: str) -> str:
    # TODO: better message
    """Check names formatting, returns the correct name"""
    if not re.fullmatch('[a-z][a-z0-9 ]*', name):
        print_warning(name + ' should match [a-z][a-z0-9 ]*')
        name = name.lower()
        print('Using ' + name)
    return name


def check_function_name(dic: Dict[str, Any]) -> None:
    """Check that field `function_name` is good"""
    if 'function_name' not in dic:
        if 'name' in dic:
            dic['function_name'] = dic['name']
            print_warning('field `name` is deprecated, use `function_name` instead')
        else:
            print_error('field `function_name` is missing')
            return
    if not isinstance(dic['function_name'], str):
        print_error('field `function_name` is not a string')
    dic['function_name'] = check_names(dic['function_name'])


def check_input(dic: Dict[str, Any]) -> None:
    """Check the whole input dict"""
    check_function_name(dic)


def input_from_dict(dic: Dict[str, Any]) -> Input:
    """Parse a input from a dict, or return a ValueError"""
    check_input(dic)
    print('---------\nErrors: {}; Warnings: {}'.format(
        Counter.errors, Counter.warnings
    ))
    if Counter.errors > 0:
        exit(1)
    value = Input.from_dict(dic)
    if value is None:
        raise ValueError('Unable to parse input')
    return value

def parse_input(stream: TextIO) -> Input:
    """Parse a input from a file stream, or return a ValueError"""
    return input_from_dict(yaml.load(stream))