# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Helpers used by several modules"""


def snake_case(name: str) -> str:
    """Format a name with snake case style"""
    return "_".join(i.lower() for i in name.split())


def pascal_case(name: str) -> str:
    """Format a name with pascal case style"""
    return "".join(i.lower().capitalize() for i in name.split())


def camel_case(name: str) -> str:
    """Format a name with camel case style"""
    pascal = pascal_case(name)
    return pascal[0].lower() + pascal[1:]
