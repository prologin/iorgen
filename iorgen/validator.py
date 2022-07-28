# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
"""Check that a raw input is valid"""

from string import printable, whitespace
from typing import Dict, List, Optional, Tuple, Union
import re

from iorgen.types import Constraints, FormatStyle, Input, Type, TypeEnum, Variable

INTEGER_REGEX = re.compile("^-?[0-9]+$")
FLOAT_REGEX = re.compile("^-?[0-9]+\\.[0-9]+$")


class ValidatorException(Exception):
    """A custom exception for the Validator class"""

    def __init__(self, message: str) -> None:
        super().__init__(message)
        self.message = message


class Validator:
    """Check the format of a raw input: it should match the declaration"""

    def __init__(self, input_data: Input, lines: List[str]) -> None:
        self.input = input_data
        self.current_line = 0
        self.lines = lines
        self.integers = {}  # type: Dict[str, int]
        self.valid_for_perf_only = False

    def next_line(self) -> str:
        """Return next raw input line"""
        if self.current_line >= len(self.lines):
            raise ValidatorException("Not enough lines in input")
        line = self.lines[self.current_line]
        self.current_line += 1
        return line

    def eval_var(self, var: Union[int, Variable]) -> int:
        """Eval an integer, that can be a variable"""
        if isinstance(var, int):
            return var
        if var.name not in self.integers:
            raise ValidatorException(
                f"No variable named '{var.name}' found to be used as a constraint"
            )
        return self.integers[var.name]

    def check_integer(self, string: str, constraints: Constraints, name: str) -> None:
        """Check that the input is a correct integer"""
        if not INTEGER_REGEX.match(string):
            raise ValidatorException(
                "Line {}: '{}' is not an integer".format(self.current_line, string)
            )
        value = int(string)
        if name:
            self.integers[name] = value

        min_value = self.eval_var(constraints.min)
        if value < min_value:
            if value < self.eval_var(constraints.min_perf):
                raise ValidatorException(
                    f"Line {self.current_line}: {value} < {min_value} (the min value)"
                )
            self.valid_for_perf_only = True
        max_value = self.eval_var(constraints.max)
        if value > max_value:
            if value > self.eval_var(constraints.max_perf):
                raise ValidatorException(
                    f"Line {self.current_line}: {value} > {max_value} (the max value)"
                )
            self.valid_for_perf_only = True
        if constraints.choices and value not in constraints.choices:
            raise ValidatorException(
                "Line {}: {} not in {{{}}}".format(
                    self.current_line,
                    value,
                    ", ".join(str(i) for i in sorted(constraints.choices)),
                )
            )
        if constraints.is_size and value < 0:
            raise ValidatorException(
                f"Line {self.current_line}: {value} is negative but used as a size"
            )

    def check_float(self, string: str, constraints: Constraints) -> None:
        """Check that the input is a correct floating number"""
        if not FLOAT_REGEX.match(string):
            raise ValidatorException(
                "Line {}: '{}' is not a float".format(self.current_line, string)
            )
        value = float(string)

        min_value = self.eval_var(constraints.min)
        if value < min_value:
            if value < self.eval_var(constraints.min_perf):
                raise ValidatorException(
                    f"Line {self.current_line}: {value} < {min_value} (the min value)"
                )
            self.valid_for_perf_only = True
        max_value = self.eval_var(constraints.max)
        if value > max_value:
            if value > self.eval_var(constraints.max_perf):
                raise ValidatorException(
                    f"Line {self.current_line}: {value} > {max_value} (the max value)"
                )
            self.valid_for_perf_only = True
        if constraints.choices and value not in constraints.choices:
            raise ValidatorException(
                "Line {}: {} not in {{{}}}".format(
                    self.current_line,
                    value,
                    ", ".join(str(i) for i in sorted(constraints.choices)),
                )
            )

    def check_char(self, string: str, constraints: Constraints, use_ws: bool) -> None:
        """Check that the input is a correct string"""
        if len(string) != 1 or string not in printable:
            raise ValidatorException(
                "Line {}: '{}' is not an ASCII char".format(self.current_line, string)
            )
        if string in whitespace:
            if not use_ws:
                raise ValidatorException(
                    "Line {}: '{}' is a whitespace character".format(
                        self.current_line, string
                    )
                )
            if string != " ":
                raise ValidatorException(
                    "Line {}: '{}' is a whitespace character, but not space".format(
                        self.current_line, string
                    )
                )
        if constraints.choices and string not in constraints.choices:
            raise ValidatorException(
                "Line {}: {} not in {{{}}}".format(
                    self.current_line,
                    string,
                    ", ".join(str(i) for i in sorted(constraints.choices)),
                )
            )

    def get_size(self, size: str) -> Tuple[int, str]:
        """Get the integer size, and a string description of it"""
        if size in self.integers:
            value = self.integers[size]
            return (value, "{} ({})".format(value, size))
        return (int(size), size)

    def read_type(
        self,
        name: str,
        type_: Type,
        constraints: Optional[Constraints],
        style: FormatStyle = FormatStyle.DEFAULT,
    ) -> None:
        """Read a type, or throw an exception if incorrect"""
        # pylint: disable=too-many-branches,too-many-statements
        if type_.main == TypeEnum.INT:
            assert constraints is not None
            self.check_integer(self.next_line(), constraints, name)
        elif type_.main == TypeEnum.FLOAT:
            assert constraints is not None
            self.check_float(self.next_line(), constraints)
        elif type_.main == TypeEnum.CHAR:
            assert constraints is not None
            self.check_char(self.next_line(), constraints, use_ws=False)
        elif type_.main == TypeEnum.STR:
            assert constraints is not None
            line = self.next_line()
            (size, size_desc) = self.get_size(type_.size)
            if len(line) > size:
                raise ValidatorException(
                    "Line {}: '{}' should be of maximum size {}".format(
                        self.current_line, line, size_desc
                    )
                )
            if line.strip() != line:
                raise ValidatorException(
                    "Line {}: '{}' has leading/trailing whitespaces".format(
                        self.current_line, line
                    )
                )
            for i in line:
                self.check_char(i, constraints, use_ws=True)
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            (size, size_desc) = self.get_size(type_.size)
            if style == FormatStyle.FORCE_NEWLINES or not type_.fits_in_one_line(
                self.input.structs, style
            ):
                for _ in range(size):
                    self.read_type("", type_.encapsulated, constraints)
            elif type_.encapsulated.main == TypeEnum.CHAR:
                assert constraints is not None
                line = self.next_line()
                if len(line) != size:
                    raise ValidatorException(
                        "Line {}: '{}' should be of size {}".format(
                            self.current_line, line, size_desc
                        )
                    )
                for i in line:
                    self.check_char(i, constraints, use_ws=False)
            elif type_.encapsulated.main == TypeEnum.INT:
                assert constraints is not None
                line = self.next_line()
                words = line.split()
                if len(words) != size:
                    raise ValidatorException(
                        "Line {}: '{}' should be {} words separated by spaces".format(
                            self.current_line, line, size_desc
                        )
                    )
                if " ".join(words) != line:
                    raise ValidatorException(
                        (
                            "Line {}: '{}' should have whitespaces only "
                            "between words, and only one"
                        ).format(self.current_line, line)
                    )
                for i in words:
                    self.check_integer(i, constraints, "")
            elif type_.encapsulated.main == TypeEnum.FLOAT:
                assert constraints is not None
                line = self.next_line()
                words = line.split()
                if len(words) != size:
                    raise ValidatorException(
                        "Line {}: '{}' should be {} words separated by spaces".format(
                            self.current_line, line, size_desc
                        )
                    )
                if " ".join(words) != line:
                    raise ValidatorException(
                        (
                            "Line {}: '{}' should have whitespaces only "
                            "between words, and only one"
                        ).format(self.current_line, line)
                    )
                for i in words:
                    self.check_float(i, constraints)
        elif type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            if not type_.fits_in_one_line(self.input.structs, style):
                for var in struct.fields:
                    self.read_type(var.name, var.type, var.constraints)
            else:
                line = self.next_line()
                words = line.split()
                if len(words) != len(struct.fields):
                    raise ValidatorException(
                        "Line {}: '{}' should have {} fields".format(
                            self.current_line, line, len(struct.fields)
                        )
                    )
                if " ".join(words) != line:
                    raise ValidatorException(
                        (
                            "Line {}: '{}' should have whitespaces only "
                            "between words, and only one"
                        ).format(self.current_line, line)
                    )
                for var, word in zip(struct.fields, words):
                    assert var.constraints is not None
                    if var.type.main == TypeEnum.INT:
                        self.check_integer(word, var.constraints, var.name)
                    elif var.type.main == TypeEnum.FLOAT:
                        self.check_float(word, var.constraints)
                    else:
                        assert var.type.main == TypeEnum.CHAR
                        self.check_char(word, var.constraints, use_ws=False)

    def read_ints(self, variables: List[Variable]) -> None:
        """Read several ints on a line, or throw an exception if incorrect"""
        line = self.next_line()
        words = line.split()
        if len(words) != len(variables):
            raise ValidatorException(
                f"Line {self.current_line}: '{line}' should be {len(variables)}"
                " ints separated by spaces"
            )
        if " ".join(words) != line:
            raise ValidatorException(
                f"Line {self.current_line}: '{line}' should have whitespaces only "
                "between ints, and only one"
            )
        for i, var in zip(words, variables):
            assert var.constraints is not None
            self.check_integer(i, var.constraints, var.name)

    def read_floats(self, variables: List[Variable]) -> None:
        """Read several ints on a line, or throw an exception if incorrect"""
        line = self.next_line()
        words = line.split()
        if len(words) != len(variables):
            raise ValidatorException(
                f"Line {self.current_line}: '{line}' should be {len(variables)}"
                " floats separated by spaces"
            )
        if " ".join(words) != line:
            raise ValidatorException(
                f"Line {self.current_line}: '{line}' should have whitespaces only "
                "between floats, and only one"
            )
        for i, var in zip(words, variables):
            assert var.constraints is not None
            self.check_float(i, var.constraints)

    def read_all(self) -> str:
        """Parse the entire raw input and return an error if there was one"""
        try:
            for variables in self.input.get_all_vars():
                if len(variables) == 1:
                    var = variables[0]
                    self.read_type(
                        var.name, var.type, var.constraints, var.format_style
                    )
                else:
                    #TODO: support float / mix ?
                    if all(var.type.main == TypeEnum.INT for var in variables):
                        self.read_ints(variables)
        except ValidatorException as exception:
            return exception.message
        if self.current_line != len(self.lines):
            return "Too many lines in input"
        return ""


def input_errors(input_data: Input, filename: str, perf_mode: bool = False) -> str:
    """Return the first error found in a raw input, if any"""
    with open(filename, encoding="utf-8") as file_content:
        validator = Validator(input_data, [line.rstrip("\n") for line in file_content])
    error = validator.read_all()
    if error:
        return error
    if validator.valid_for_perf_only != perf_mode:
        if perf_mode:
            return (
                "Input meant for performance, but do not use "
                "any performance constraints"
            )
        return "Input valid only for performance"
    return ""
