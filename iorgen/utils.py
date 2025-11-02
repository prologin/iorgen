# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2025 Sacha Delanoue
# Copyright 2021 Kenji Gaillac
"""Helpers used by several modules"""


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


def number_int(value: int | float) -> str:
    """Return integer or float mathjax representation"""
    return f"{value:,}".replace(",", r"\,")


class IteratorName:
    """Give valid iterator names, like i, j, k, preventing scope conflicts"""

    def __init__(self, existing_names: list[str]) -> None:
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

    def __init__(self, existing_names: list[str], cs_mode: bool = False) -> None:
        # In C# you can not name a variable if it was already declared in a
        # nested scope, it would cause error CS0136
        self.existing_names = [
            "".join(i.strip().lower().split()) for i in existing_names
        ]
        self.current = -1  # number of the current 'words' variable
        self.before_scopes = [-1]  # number of the last 'words' var before each scope

        # For C# mode:
        self.cs_mode = cs_mode
        self.above_scopes = [set()]  # type: list[set[int]]
        self.other_scopes = [set()]  # type: list[set[int]]

    def next_name(self) -> str:
        """Give the next variable name"""
        self.current += 1
        current = self.current
        if self.cs_mode:
            current = 0
            while not self._is_possible_value(current):
                current += 1
        candidate = f"words{current if current != 0 else ''}"
        if candidate in self.existing_names:
            return self.next_name()
        self.above_scopes[-1].add(current)
        return candidate

    def _is_possible_value(self, value: int) -> bool:
        for scope in self.above_scopes:
            if value in scope:
                return False
        if self.cs_mode and value in self.other_scopes[-1]:
            return False
        candidate = f"words{value if value != 0 else ''}"
        return candidate not in self.existing_names

    def push_scope(self) -> None:
        """Declare a new scope"""
        self.before_scopes.append(self.current)
        self.above_scopes.append(set())
        self.other_scopes.append(set())

    def pop_scope(self) -> None:
        """Declare a scope's end"""
        self.current = self.before_scopes.pop()
        self.other_scopes[-2].update(self.other_scopes.pop())
        self.other_scopes[-1].update(self.above_scopes.pop())
