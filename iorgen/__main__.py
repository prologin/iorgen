# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Multi-languages parser generator"""

import argparse

from iorgen.generator import ALL_LANGUAGES
from iorgen.checkinput import parse_input


def main() -> None:
    """The iorgen module execution"""
    languages = {i.extension: i for i in ALL_LANGUAGES}

    parser = argparse.ArgumentParser(
        prog="python3 -m iorgen",
        description="Multi-languages parser generator")
    parser.add_argument(
        '-l',
        '--languages',
        action='append',
        help='Languages for which to generate a parser',
        choices=list(languages.keys()))
    parser.add_argument(
        'yaml',
        metavar='input.yaml',
        type=open,
        help='the yaml file describing the input')
    try:
        args = parser.parse_args()
    except FileNotFoundError as error:
        parser.error("Input file not found: {}".format(error))

    try:
        input_data = parse_input(args.yaml)
    except ValueError as error:
        print("Could not parse input data: {}".format(error))
        exit(1)

    selected_languages = args.languages or list(languages.keys())
    for language in selected_languages:
        print(languages[language].generate(input_data))


if __name__ == '__main__':
    main()
