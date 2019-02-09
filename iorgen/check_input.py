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

def check_string_fields(field: str, dic: Dict[str, Any], mandatory: bool = False) -> None:
    """Check that field `field` exists and is a string"""
    if field in dic:
        if not isinstance(dic[field], str):
            print_error('field `{}` is not a string'.format(field))
    elif mandatory:
        print_error('field `{}` is missing'.format(field))


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

def check_subject(dic: Dict[str, Any]) -> None:
    """Check that field `subject` is good"""
    check_string_fields('subject', dic)

def check_type_int(dic: Dict[str, Any]) -> None:
    """Check that input int is good"""
    if not isinstance(dic['name'], str):
        print_error('field `name` is not a string')
    dic['name'] = check_names(dic['name'])
    check_string_fields('comment', dic, True)

    # Check if there are constraints
    if 'choices' in dic:
        return
    if not 'min' in dic:
        print_warning(dic['name'] + ' has no constraint min')
    if not 'max' in dic:
        print_warning(dic['name'] + ' has no constraint max')

def check_type_char(dic: Dict[str, Any]) -> None:
    """Check that input char is good"""
    if not isinstance(dic['name'], str):
        print_error('field `name` is not a string')
    dic['name'] = check_names(dic['name'])
    check_string_fields('comment', dic, True)

    # Check if there are constrachars
    if not 'choices' in dic:
        print_warning(dic['name'] + ' has no choices')

def check_input(dic: Dict[str, Any]) -> None:
    """Check that input is good"""
    if not 'input' in dic:
        return print_error('field `input` is missing')
    if not isinstance(dic['input'], list):
        return print_error('output is not a list')
    for node in dic['input']:
        if not node['type']:
            print_error('missing field type in input')
            continue
        if node['type'] == 'int':
            check_type_int(node)
        elif node['type'] == 'char':
            check_type_char(node)
        else:
            print_error('unknown type ' + node['type'])

def check_output(dic: Dict[str, Any]) -> None:
    """Check that field `output` is good"""
    check_string_fields('output', dic)

def check_input_dic(dic: Dict[str, Any]) -> None:
    """Check the whole input dict"""
    check_function_name(dic)
    check_subject(dic)
    check_input(dic)
    check_output(dic)

def input_from_dict(dic: Dict[str, Any]) -> Input:
    """Parse a input from a dict, or return a ValueError"""
    check_input_dic(dic)
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