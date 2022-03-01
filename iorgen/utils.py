# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
# Copyright 2021 Kenji Gaillac
"""Helpers used by several modules"""

from typing import List


def snake_case(name: str) -> str:
    """Format a name with snake case style"""
    return "_".join(i.lower() for i in name.split())


def pascal_case(name: str) -> str:
    """Format a name with pascal case style"""
    return "".join(i.lower().capitalize() for i in name.split())


def camel_case(name: str) -> str:
    """Format a name with camel case style"""
    if not name:
        return ""
    pascal = pascal_case(name)
    return pascal[0].lower() + pascal[1:]


def int_to_iterator_name(value: int) -> str:
    """Map a integer to a iterator name (1 -> i, 18 -> z, 19 -> ii, ...)"""
    assert value > 0
    return chr((value - 1) % 18 + 105) * int((value - 1) / 18 + 1)


def str_int(value: int) -> str:
    """Return integer mathjax representation"""
    return f"{value:,}".replace(",", r"\,")


class IteratorName:
    """Give valid iterator names, like i, j, k, preventing scope conflicts"""

    def __init__(self, existing_names: List[str]) -> None:
        self.existing_names = [i.strip().lower() for i in existing_names]
        self.current = 0

    def new_it(self) -> str:
        """Return the name of the next iterator"""
        self.current += 1
        while int_to_iterator_name(self.current) in self.existing_names:
            self.current += 1
        return int_to_iterator_name(self.current)

    def pop_it(self) -> None:
        """Signal that the scope of the last iterator ended"""
        assert self.current >= 0
        self.current -= 1
        while (
            self.current > 0
            and int_to_iterator_name(self.current) in self.existing_names
        ):
            self.current -= 1


class WordsName:
    """Give valid variable names starting with 'words'"""

    def __init__(self, existing_names: List[str], cs_mode: bool = False) -> None:
        # In C# you can not name a variable if it was already declared in a
        # nested scope, it would cause error CS0136
        self.existing_names = [i.strip().lower() for i in existing_names]
        self.current = -1
        self.current_nested = -1
        self.scopes = [-1]
        self.nested_scopes = [-1]
        self.cs_mode = cs_mode

    def next_name(self) -> str:
        """Give the next variable name"""
        self.current += 1
        self.current_nested += 1
        current = self.current_nested if self.cs_mode else self.current
        self.nested_scopes = [max(i, current) for i in self.nested_scopes]
        candidate = f"words{current if current != 0 else ''}"
        if candidate in self.existing_names:
            return self.next_name()
        return candidate

    def push_scope(self) -> None:
        """Declare a new scope"""
        self.scopes.append(self.current)
        self.nested_scopes.append(self.current)
        self.current_nested = self.current

    def pop_scope(self) -> None:
        """Declare a scope's end"""
        self.current = self.scopes.pop()
        self.current_nested = self.nested_scopes.pop()
