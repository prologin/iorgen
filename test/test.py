#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Check that parsers are the same as before, and parse correctly"""

import os
import shutil
import sys
from difflib import unified_diff
from pathlib import Path
from typing import Iterator, List

sys.path.insert(0, "..")
# pylint: disable=wrong-import-position
from iorgen import Input, ALL_LANGUAGES, Language, parse_input


def print_color(lines: Iterator[str]) -> None:
    """Print a diff with some console colors"""
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


def check_diff(generated: List[str], filename: str,
               tofile: str = 'generated') -> bool:
    """Check if a generated result is the same as a reference file"""
    ref = Path(filename).read_text().splitlines(True)
    if generated != ref:
        print_color(
            unified_diff(ref, generated, fromfile=filename, tofile=tofile))
        return False
    return True


def gen_is_same_as_sample(input_data: Input, prefix_path: str,
                          language: Language) -> bool:
    """Check that the generated parser is the same as the reference file"""
    filename = prefix_path + language.extension
    generated = language.generate(input_data).splitlines(True)
    return check_diff(generated, filename)


def run_on_input(input_data: Input, name: str, language: Language) -> bool:
    """Check that the generated parser prints the input is it fed in"""
    filename = "/tmp/iorgen/tests/{0}/{1}.{0}".format(language.extension, name)
    generated = language.generator(input_data, True)

    Path(os.path.dirname(filename)).mkdir(parents=True, exist_ok=True)
    Path(filename).write_text(generated)

    reffile = "samples/{0}/{0}.sample_input".format(name)
    out = language.compile_and_run(filename, reffile)
    reprint = out.splitlines(True)
    return check_diff(reprint, reffile, 'generated from ' + language.extension)


def test_samples() -> None:
    """Test all the samples"""
    try:
        shutil.rmtree("/tmp/iorgen/tests/")
    except FileNotFoundError:
        pass
    for name in os.listdir("samples"):
        prefix = "samples/{0}/{0}.".format(name)
        with open(prefix + "yaml", 'r') as stream:
            input_data = parse_input(stream)

        for language in ALL_LANGUAGES:
            assert gen_is_same_as_sample(input_data, prefix, language)
            if not language.is_pseudo_code():
                assert run_on_input(input_data, name, language)

        print("OK", name)


if __name__ == "__main__":
    test_samples()
