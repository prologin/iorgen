# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
# Copyright 2019 Matthieu Moatti
"""Check that a YAML input is well formed"""

import re
from typing import Any, Dict, List, TextIO, Union

import yaml

import warnings

from iorgen.types import Input, Struct, Type, Variable

class Checker:

    def __init__(self):
        self.errors = 0
        self.warnings = 0

    def increase_error(self) -> None:
        """Increase error counter"""
        self.errors += 1

    def print_error(self, msg: str) -> None:
        """Print error and increase error counter"""
        print('ERROR: ' + msg)
        self.increase_error()

    def increase_warning(self) -> None:
        """Increase warning counter"""
        self.warnings += 1

    def print_warning(self, msg: str) -> None:
        """Print warning and increase warning counter"""
        print('WARNING: ' + msg)
        self.increase_warning()

    def check_names(self, name: str) -> str:
        # TODO: better message
        """Check names formatting, returns the correct name"""
        if not re.fullmatch('[a-z][a-z0-9 ]*', name):
            self.print_warning(name + ' should match [a-z][a-z0-9 ]*')
            name = name.lower()
            print('Using ' + name)
        return name

    def check_string_fields(self, field: str, dic: Dict[str, Any], mandatory: bool = False) -> None:
        """Check that field `field` exists and is a string"""
        if field in dic:
            if not isinstance(dic[field], str):
                self.print_error('field `{}` is not a string'.format(field))
        elif mandatory:
            self.print_error('field `{}` is missing'.format(field))

    def check_function_name(self, dic: Dict[str, Any]) -> None:
        """Check that field `function_name` is good"""
        if 'function_name' not in dic:
            if 'name' in dic:
                dic['function_name'] = dic['name']
                self.print_warning('field `name` is deprecated, use `function_name` instead')
            else:
                self.print_error('field `function_name` is missing')
                return
        if not isinstance(dic['function_name'], str):
            self.print_error('field `function_name` is not a string')
        dic['function_name'] = self.check_names(dic['function_name'])

    def check_subject(self, dic: Dict[str, Any]) -> None:
        """Check that field `subject` is good"""
        self.check_string_fields('subject', dic)

    def check_type_int(self, dic: Dict[str, Any]) -> None:
        """Check that input int is good"""
        if not isinstance(dic['name'], str):
            self.print_error('field `name` is not a string')
        dic['name'] = self.check_names(dic['name'])
        self.check_string_fields('comment', dic, True)

        # Check if there are constraints
        if 'choices' in dic:
            return
        if not 'min' in dic:
            self.print_warning(dic['name'] + ' has no constraint min')
        if not 'max' in dic:
            self.print_warning(dic['name'] + ' has no constraint max')

    def check_type_char(self, dic: Dict[str, Any]) -> None:
        """Check that input char is good"""
        if not isinstance(dic['name'], str):
            self.print_error('field `name` is not a string')
        dic['name'] = self.check_names(dic['name'])
        self.check_string_fields('comment', dic, True)

        # Check if there are constrachars
        if not 'choices' in dic:
            self.print_warning(dic['name'] + ' has no choices')

    def check_input(self, dic: Dict[str, Any]) -> None:
        """Check that input is good"""
        if not 'input' in dic:
            return self.print_error('field `input` is missing')
        if not isinstance(dic['input'], list):
            return self.print_error('output is not a list')
        for node in dic['input']:
            if not node['type']:
                self.print_error('missing field type in input')
                continue
            if node['type'] == 'int':
                self.check_type_int(node)
            elif node['type'] == 'char':
                self.check_type_char(node)
            else:
                self.print_error('unknown type ' + node['type'])

    def check_output(self, dic: Dict[str, Any]) -> None:
        """Check that field `output` is good"""
        self.check_string_fields('output', dic)

    def check_input_dic(self, dic: Dict[str, Any]) -> None:
        """Check the whole input dict"""
        self.check_function_name(dic)
        self.check_subject(dic)
        self.check_input(dic)
        self.check_output(dic)

def input_from_dict(dic: Dict[str, Any]) -> Input:
    """Parse a input from a dict, or return a ValueError"""
    checker = Checker()
    checker.check_input_dic(dic)
    print('---------\nErrors: {}; Warnings: {}'.format(
        checker.errors, checker.warnings
    ))
    if checker.errors > 0:
        exit(1)
    value = Input.from_dict(dic)
    if value is None:
        raise ValueError('Unable to parse input')
    return value

def parse_input(stream: TextIO) -> Input:
    """Parse a input from a file stream, or return a ValueError"""
    return input_from_dict(yaml.load(stream))