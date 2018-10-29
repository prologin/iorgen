#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate c++ parser for the given input"""

import os
import subprocess
import sys
from difflib import unified_diff
from pathlib import Path
from typing import Iterator, List, Optional

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


def gen_is_same_as_sample(input_data: Input, prefix_path: str, extension: str,
                          gen_func) -> bool:
    filename = prefix_path + extension
    generated = gen_func(input_data).splitlines(True)
    ref = Path(filename).read_text().splitlines(True)
    if generated != ref:
        print_color(
            unified_diff(
                ref, generated, fromfile=filename, tofile='generated'))
        return False
    return True


def run_on_input(input_data: Input, name: str, extension: str,
        gen_func, command: List[str]) -> bool:
    filename = "/tmp/iorgen/tests/{0}/{1}.{0}".format(extension, name)
    generated = gen_func(input_data, True)

    Path(os.path.dirname(filename)).mkdir(parents=True, exist_ok=True)
    Path(filename).write_text(generated)

    cwd = os.getcwd()
    os.chdir(os.path.dirname(filename))
    subprocess.run(command + [filename])
    os.chdir(cwd)
    return True


def test_samples() -> None:
    """Test all the samples"""
    for name in os.listdir("samples"):
        prefix = "samples/{0}/{0}.".format(name)
        input_data = read_input(prefix + "yaml")
        assert input_data is not None

        assert gen_is_same_as_sample(input_data, prefix, "cpp", gen_cpp)
        assert gen_is_same_as_sample(input_data, prefix, "hs", gen_haskell)

        run_on_input(input_data, name, "hs", gen_haskell, ["ghc", "-dynamic"])

        print("OK", name)


if __name__ == "__main__":
    test_samples()
