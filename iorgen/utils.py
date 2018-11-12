# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
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
    pascal = pascal_case(name)
    return pascal[0].lower() + pascal[1:]


def int_to_iterator_name(value: int, times: int = 1) -> str:
    """Map a integer to a iterator name (1 -> i, 18 -> z, 19 -> ii, ...)"""
    if value <= 18:
        return chr(value + 104) * times
    return int_to_iterator_name(value - 18, times + 1)


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
        self.current -= 1
        while int_to_iterator_name(self.current) in self.existing_names:
            if self.current == 0:
                break
            self.current -= 1


class WordsName:
    """Give valid variable names starting with 'words'"""

    def __init__(self, existing_names: List[str]) -> None:
        self.existing_names = [i.strip().lower() for i in existing_names]
        self.current = -1
        self.scopes = [-1]

    def next_name(self) -> str:
        """Give the next variable name"""
        self.current += 1
        candidate = "words{}".format(self.current if self.current != 0 else "")
        if candidate in self.existing_names:
            return self.next_name()
        return candidate

    def push_scope(self) -> None:
        """Declare a new scope"""
        self.scopes.append(self.current)

    def pop_scope(self) -> None:
        """Declare a scope's end"""
        self.current = self.scopes.pop()
