#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Regenerate the test samples"""

import os
import sys
from pathlib import Path
from typing import List, Optional

import yaml

sys.path.insert(0, "..")
# pylint: disable=wrong-import-position
from iorgen import Input, ALL_LANGUAGES


def read_input(filename: str) -> Optional[Input]:
    """Parse a YAML into an Input class"""
    with open(filename, 'r') as stream:
        return Input.from_dict(yaml.load(stream))


def regenerate_samples() -> None:
    """Regenerate all the samples"""
    for name in os.listdir("samples"):
        prefix = "samples/{0}/{0}.".format(name)
        input_data = read_input(prefix + "yaml")
        assert input_data is not None

        for language in ALL_LANGUAGES:
            Path(prefix + language.extension).write_text(
                language.generate(input_data))


if __name__ == "__main__":
    regenerate_samples()
