#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Generate c++ parser for the given input"""

import sys
from typing import TextIO

import yaml

from iorgen import gen_csharp, Input


def generate_cpp(stream: TextIO) -> str:
    """Generate the cpp reader for the input described in the file"""
    input_data = Input.from_dict(yaml.load(stream))
    if not input_data:
        print("Could not parse input data")
        exit(1)
    return gen_csharp(input_data)


if __name__ == "__main__":
    with open(sys.argv[1], 'r') as filestream:
        print(generate_cpp(filestream))
