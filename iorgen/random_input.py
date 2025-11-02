# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2025 Sacha Delanoue
# Copyright 2019 Matthieu Moatti
"""Generate a valid raw input"""

from typing import Callable, Optional, TypeVar, Union
import random
import string

from iorgen.types import Constraints, Input, Type, TypeEnum, Variable


def generate_char(constraints: Constraints, whitespace: bool) -> str:
    """Generate a random ASCII char, given some constraints"""
    if constraints.choices:
        return random.choice([str(i) for i in constraints.choices])
    possible = string.digits + string.ascii_letters + string.punctuation
    if whitespace:
        possible += " "
    return random.choice(possible)


class Generator:
    """Generate some random valid raw_input"""

    def __init__(
        self, input_data: Input, specs: dict[str, str], perf_mode: bool
    ) -> None:
        self.input = input_data
        self.numbers = {}  # type: dict[str, Union[int, float]]
        self.perf_mode = perf_mode
        self.specs = specs

    NUM = TypeVar("NUM", int, float)

    def eval_var(
        self,
        cast: Callable[[Union[str, int, float]], NUM],
        var: Union[int, float, str, Variable],
    ) -> NUM:
        """Eval an integer, that can be a variable, or a string"""
        if isinstance(var, (int, float)):
            cast_var = cast(var)
            assert var == cast_var
            return cast_var
        key = var if isinstance(var, str) else var.name
        if key in self.numbers:
            number = self.numbers[key]
            cast_number = cast(number)
            assert number == cast_number
            return cast_number
        return cast(key)

    def get_bounds(
        self,
        cast: Callable[[Union[str, int, float]], NUM],
        name: str,
        constraints: Constraints,
    ) -> tuple[NUM, NUM]:
        """Return min and max possible values for a number (int or float)"""
        if constraints.choices:
            random_choice = random.choice(list(constraints.choices))
            assert isinstance(random_choice, type(cast("0")))
            return (random_choice, random_choice)

        min_ = constraints.min_perf if self.perf_mode else constraints.min
        max_ = constraints.max_perf if self.perf_mode else constraints.max
        if name + "_min" in self.specs:
            assert self.eval_var(cast, min_) < cast(self.specs[name + "_min"])
            min_ = cast(self.specs[name + "_min"])
        if name + "_max" in self.specs:
            assert self.eval_var(cast, max_) > cast(self.specs[name + "_max"])
            max_ = cast(self.specs[name + "_max"])
        if name in self.specs:
            value = cast(self.specs[name])
            assert (
                self.eval_var(cast, min_) <= value <= self.eval_var(cast, max_)
                and value in constraints.choices
                if constraints.choices
                else True
            )
            return (value, value)
        return (self.eval_var(cast, min_), self.eval_var(cast, max_))

    def generate_integer(self, name: str, constraints: Constraints) -> str:
        """Generate a random integer, given some constraints"""

        min_, max_ = self.get_bounds(int, name, constraints)
        if constraints.is_size:
            min_ = max(0, min_)
        value = random.randint(min_, max_)

        if name:
            self.numbers[name] = value
        return str(value)

    def generate_float(self, name: str, constraints: Constraints) -> str:
        """Generate a random float, given some constraints"""

        min_, max_ = self.get_bounds(float, name, constraints)
        value = random.uniform(min_, max_)
        value_str = format(value, ".15g")

        if name:
            self.numbers[name] = float(value_str)
        return value_str

    def generate_line(
        self, name: str, type_: Type, constraints: Optional[Constraints]
    ) -> str:
        # pylint: disable=too-many-return-statements
        """Generate a raw input for a line"""
        assert type_.fits_in_one_line(self.input.structs)
        if type_.main == TypeEnum.INT:
            assert constraints is not None
            return self.generate_integer(name, constraints)
        if type_.main == TypeEnum.FLOAT:
            assert constraints is not None
            return self.generate_float(name, constraints)
        if type_.main == TypeEnum.CHAR:
            assert constraints is not None
            return generate_char(constraints, False)
        if type_.main == TypeEnum.STR:
            assert constraints is not None
            size = self.eval_var(int, type_.size)
            return "".join(
                generate_char(constraints, i not in (0, size - 1)) for i in range(size)
            )
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            assert constraints is not None
            size = self.eval_var(int, type_.size)
            if type_.encapsulated.main == TypeEnum.INT:
                return " ".join(
                    self.generate_integer(name, constraints) for _ in range(size)
                )
            if type_.encapsulated.main == TypeEnum.FLOAT:
                return " ".join(
                    self.generate_float(name, constraints) for _ in range(size)
                )
            assert type_.encapsulated.main == TypeEnum.CHAR
            return "".join(generate_char(constraints, False) for i in range(size))
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            fields = []
            for var in struct.fields:
                assert var.constraints
                if var.type.main == TypeEnum.INT:
                    fields.append(self.generate_integer(var.name, var.constraints))
                elif var.type.main == TypeEnum.FLOAT:
                    fields.append(self.generate_float(var.name, var.constraints))
                else:
                    assert var.type.main == TypeEnum.CHAR
                    fields.append(generate_char(var.constraints, False))
            return " ".join(fields)
        assert False
        return ""

    def generate_lines(self, var: Variable) -> list[str]:
        """Generate the raw input for a type"""
        if var.fits_in_one_line(self.input.structs):
            return [self.generate_line(var.name, var.type, var.constraints)]
        if var.type.main == TypeEnum.LIST:
            assert var.type.encapsulated
            inner = Variable("", "", var.type.encapsulated)
            inner.constraints = var.constraints
            lines = []
            size = self.eval_var(int, var.type.size)
            for _ in range(size):
                lines.extend(self.generate_lines(inner))
            return lines
        if var.type.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(var.type.struct_name)
            lines = []
            for field in struct.fields:
                lines.extend(self.generate_lines(field))
            return lines
        assert False
        return []


def generate_random_input(
    input_data: Input, specs: list[str], perf_mode: bool = False
) -> str:
    """Generate a randow raw input, as described by input_data"""
    specs_dict = {}
    for i in range(0, len(specs), 2):
        specs_dict[specs[i]] = specs[i + 1]
    generator = Generator(input_data, specs_dict, perf_mode)
    lines = []
    for variables in input_data.get_all_vars():
        if len(variables) == 1:
            lines.extend(generator.generate_lines(variables[0]))
        else:
            values = []
            for var in variables:
                assert var.type.main == TypeEnum.INT
                assert var.constraints is not None
                values.append(generator.generate_integer(var.name, var.constraints))
            lines.append(" ".join(values))
    return "\n".join(lines) + "\n"
