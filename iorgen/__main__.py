# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2021 Sacha Delanoue
# Copyright 2018 Matthieu Moatti
# Copyright 2021 Grégoire Geis
"""Multi-languages parser generator"""

import argparse
import os
import sys
import glob
from pathlib import Path

from iorgen.generator import ALL_LANGUAGES, gen_compile_run_and_compare
from iorgen.checkinput import parse_input
from iorgen.markdown import gen_markdown
from iorgen.random_input import generate_random_input
from iorgen.validator import input_errors


def get_parser() -> argparse.ArgumentParser:
    """Create the ArgumentParser for iorgen"""
    parser = argparse.ArgumentParser(description="Multi-languages parser generator")
    parser.add_argument(
        "--languages",
        "-l",
        action="append",
        help="languages for which to generate a parser",
        choices=[i.extension for i in ALL_LANGUAGES],
    )
    parser.add_argument(
        "--markdown",
        "-m",
        default="fr",
        help="language for the subject in markdown",
        choices=["None", "en", "fr"],
    )
    parser.add_argument(
        "--output_dir",
        "-o",
        default="skeleton",
        metavar="output-dir",
        help="output folder",
    )
    parser.add_argument(
        "--validate",
        "-v",
        default="",
        metavar="raw-input",
        help="Instead of generating, check that a raw input is correct",
    )
    parser.add_argument(
        "--generate_random",
        "-g",
        default=False,
        action="store_true",
        help="Instead of generating a parser, generate a randow raw input",
    )
    parser.add_argument(
        "--specify",
        "-s",
        default=[],
        nargs="*",
        metavar="NAME VALUE",
        help=(
            "Use with --generate_random option : specify values, max or"
            " min of input lists you want (NAME_max VALUE for specify max)"
        ),
    )
    parser.add_argument(
        "--perf_mode",
        "-p",
        default=False,
        action="store_true",
        help="Use with --validate or --generate_randow option: perf mode",
    )
    parser.add_argument(
        "yaml",
        metavar="input.yaml",
        type=open,
        help="the yaml file describing the input",
    )
    parser.add_argument(
        "--run",
        "-r",
        default="",
        help=(
            "Run mode: check that generated parser prints "
            "the input it is fed in. Argument is the "
            "filename of the inputs (wildcard supported)"
        ),
    )
    return parser


def main() -> None:
    """The iorgen module execution"""
    parser = get_parser()
    try:
        args = parser.parse_args()
    except FileNotFoundError as error:
        parser.error("Input file not found: {}".format(error))

    try:
        input_data = parse_input(args.yaml)
    except ValueError as error:
        print("Could not parse input data: {}".format(error))
        sys.exit(1)

    if args.validate:
        if args.generate_random:
            print("Can not use both validate and generate_random modes")
            sys.exit(2)
        status = input_errors(input_data, args.validate, args.perf_mode)
        if status:
            print("Input is invalid: {}".format(status))
            sys.exit(3)
        print("Input is valid")
        sys.exit(0)

    if args.generate_random:
        print(generate_random_input(input_data, args.specify, args.perf_mode), end="")
        sys.exit(0)

    languages = {i.extension: i for i in ALL_LANGUAGES}
    selected_languages = args.languages or list(languages.keys())
    prefix = os.path.split(os.path.splitext(args.yaml.name)[0])[1]

    if args.run:
        success = True
        check_files = glob.glob(args.run)
        print(
            "Check with {} (found {} match): ".format(args.run, len(check_files)),
            end="",
            flush=True,
        )
        for language in selected_languages:
            print(language, end=" ", flush=True)
            if not gen_compile_run_and_compare(
                input_data, prefix, languages[language], "run", check_files, False
            ):
                print("\nError with", language)
                success = False
        print("SUCCESS" if success else "FAILURE")
        sys.exit(0 if success else 1)

    Path(args.output_dir).mkdir(exist_ok=True)

    for language in selected_languages:
        path = Path(
            os.path.join(args.output_dir, prefix + "." + languages[language].extension)
        )
        path.write_text(languages[language].generate(input_data), encoding="utf-8")
    path = Path(os.path.join(args.output_dir, "..", "subject-io-stub.md"))
    if args.markdown != "None":
        path.write_text(gen_markdown(input_data, args.markdown), encoding="utf-8")


if __name__ == "__main__":
    main()
