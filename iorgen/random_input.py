# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2022 Sacha Delanoue
# Copyright 2019 Matthieu Moatti
"""Generate a valid raw input"""

from typing import Dict, List, Optional, Union
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
        self, input_data: Input, specs: Dict[str, int], perf_mode: bool
    ) -> None:
        self.input = input_data
        self.integers = {}  # type: Dict[str, int]
        self.perf_mode = perf_mode
        self.specs = specs

    def eval_var(self, var: Union[int, str, Variable]) -> int:
        """Eval an integer, that can be a variable, or a string"""
        if isinstance(var, int):
            return var
        key = var if isinstance(var, str) else var.name
        if key in self.integers:
            return self.integers[key]
        return int(key)

    def generate_integer(self, name: str, constraints: Constraints) -> str:
        """Generate a random integer, given some constraints"""
        value = 0

        if constraints.choices:
            value = random.choice([int(i) for i in constraints.choices])

        min_ = constraints.min_perf if self.perf_mode else constraints.min
        max_ = constraints.max_perf if self.perf_mode else constraints.max
        if name + "_min" in self.specs:
            assert self.eval_var(min_) < self.specs[name + "_min"]
            min_ = self.specs[name + "_min"]
        if name + "_max" in self.specs:
            assert self.eval_var(max_) > self.specs[name + "_max"]
            max_ = self.specs[name + "_max"]

        if name in self.specs:
            assert (
                self.specs[name] >= self.eval_var(min_)
                and self.specs[name] <= self.eval_var(max_)
                and self.specs[name] in constraints.choices
                if constraints.choices
                else True
            )
            value = self.specs[name]
        else:
            min_eval = self.eval_var(min_)
            if constraints.is_size:
                min_eval = max(0, min_eval)
            value = random.randint(min_eval, self.eval_var(max_))
        if name:
            self.integers[name] = value
        return str(value)

    def generate_line(
        self, name: str, type_: Type, constraints: Optional[Constraints]
    ) -> str:
        # pylint: disable=too-many-return-statements
        """Generate a raw input for a line"""
        assert type_.fits_in_one_line(self.input.structs)
        if type_.main == TypeEnum.INT:
            assert constraints is not None
            return self.generate_integer(name, constraints)
        if type_.main == TypeEnum.CHAR:
            assert constraints is not None
            return generate_char(constraints, False)
        if type_.main == TypeEnum.STR:
            assert constraints is not None
            size = self.eval_var(type_.size)
            return "".join(
                generate_char(constraints, i not in (0, size - 1)) for i in range(size)
            )
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            assert constraints is not None
            size = self.eval_var(type_.size)
            if type_.encapsulated.main == TypeEnum.INT:
                return " ".join(
                    self.generate_integer(name, constraints) for _ in range(size)
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
                else:
                    assert var.type.main == TypeEnum.CHAR
                    fields.append(generate_char(var.constraints, False))
            return " ".join(fields)
        assert False
        return ""

    def generate_lines(self, var: Variable) -> List[str]:
        """Generate the raw input for a type"""
        if var.fits_in_one_line(self.input.structs):
            return [self.generate_line(var.name, var.type, var.constraints)]
        if var.type.main == TypeEnum.LIST:
            assert var.type.encapsulated
            inner = Variable("", "", var.type.encapsulated)
            inner.constraints = var.constraints
            lines = []
            for _ in range(self.eval_var(var.type.size)):
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
    input_data: Input, specs: List[str], perf_mode: bool = False
) -> str:
    """Generate a randow raw input, as described by input_data"""
    specs_dict = {}
    for i in range(0, len(specs), 2):
        specs_dict[specs[i]] = int(specs[i + 1])
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
