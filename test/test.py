#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate c++ parser for the given input"""

import os
import sys
from difflib import unified_diff
from pathlib import Path
from typing import Iterator, Optional

import yaml

sys.path.insert(0, "..")
from iorgen import *


def read_input(filename: str) -> Optional[Input]:
    """Parse a YAML into an Input class"""
    with open(filename, 'r') as stream:
        return Input.from_dict(yaml.load(stream))


def print_color(lines: Iterator[str]) -> None:
    for i, line in enumerate(lines):
        if i < 2:
            print('\033[1m' + line + '\033[0m', end='')
        elif line[0:2] == '@@':
            print('\033[96m' + line + '\033[0m', end='')
        elif line[0] == '+':
            print('\033[92m' + line + '\033[0m', end='')
        elif line[0] == '-':
            print('\033[91m' + line + '\033[0m', end='')
        else:
            print(line, end='')


def test_samples() -> None:
    """Test all the samples"""
    for name in os.listdir("samples"):
        prefix = "samples/{0}/{0}.".format(name)
        input_data = read_input(prefix + "yaml")
        assert input_data is not None

        cpp = (gen_cpp(input_data) + "\n").splitlines(True)
        cpp_ref = Path(prefix + "cpp").read_text().splitlines(True)
        if cpp != cpp_ref:
            print_color(
                unified_diff(
                    cpp_ref, cpp, fromfile=(prefix + "cpp"), tofile='gen'))
        assert cpp == cpp_ref

        hs = (gen_haskell(input_data) + "\n").splitlines(True)
        hs_ref = Path(prefix + "hs").read_text().splitlines(True)
        if hs != hs_ref:
            print_color(
                unified_diff(
                    hs_ref, hs, fromfile=(prefix + "hs"), tofile='gen'))
        assert hs == hs_ref

        print("OK", name)


if __name__ == "__main__":
    test_samples()
