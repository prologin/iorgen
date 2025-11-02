# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2025 Sacha Delanoue
# Copyright 2018 Matthieu Moatti
# Copyright 2021 Grégoire Geis
# Copyright 2022 Kenji Gaillac
"""Multi-languages parser generator"""

import argparse
import glob
import sys
from collections.abc import Iterable
from pathlib import Path
from typing import NoReturn

from iorgen.checkinput import parse_input
from iorgen.generator import ALL_LANGUAGES, gen_compile_run_and_compare
from iorgen.markdown import gen_markdown
from iorgen.random_input import generate_random_input
from iorgen.types import Input
from iorgen.validator import input_errors


def parse_yaml_input_or_exit(yaml_path: Path) -> Input | NoReturn:
    """Parse a YAML input file (.iorgen) or exits if an error occurred"""
    with open(yaml_path, encoding="UTF-8") as yaml_file:
        try:
            return parse_input(yaml_file)
        except ValueError as error:
            print(f"Could not parse input data: {error}")
            sys.exit(1)


def globs_to_paths(input_globs: Iterable[str], warn: bool = True) -> list[Path]:
    """Return all paths matching the globs"""
    input_files = set()

    for input_glob in input_globs:
        matched_files = glob.glob(input_glob, recursive=True)
        if not matched_files:
            if warn:
                print(f"Warning! `{input_glob}` did not match any file")
        else:
            input_files.update(
                [Path(input_file).resolve() for input_file in matched_files]
            )

    return sorted(input_files)


LANGUAGES_MAPPING = {i.extension: i for i in ALL_LANGUAGES}
LANGUAGE_EXTENSIONS = [i.extension for i in ALL_LANGUAGES]


def command_gen_stubs(
    yaml_list: list[Path],
    output_dir: Path,
    same_dir: bool,
    languages: list[str],
    markdown: str,
) -> NoReturn:
    """
    gen-stubs subcommand: generate subject stub and parsers for all the
    selected languages (also called skeletons)
    """
    for yaml in yaml_list:
        if not yaml.exists():
            print(f"Input file `{yaml}` not found, aborting", file=sys.stderr)
            sys.exit(1)

        print(f"Proceeding with file `{yaml}`")

        yaml_input_data = parse_yaml_input_or_exit(yaml)
        prefix = yaml.stem
        skeleton_dir = yaml.parent / output_dir if same_dir else output_dir

        # Create output_dir if it does not exists
        skeleton_dir.mkdir(exist_ok=True)

        # Generate skeletons for all selected languages
        for language in languages:
            path = skeleton_dir / f"{prefix}.{LANGUAGES_MAPPING[language].extension}"
            path.write_text(
                LANGUAGES_MAPPING[language].generate(yaml_input_data), encoding="utf-8"
            )

        print("Skeletons have been generated")

        # Generate the markdown subject stub
        if markdown != "None":
            path = skeleton_dir.parent / (
                "subject-io-stub.md" if same_dir else f"{prefix}-subject-io-stub.md"
            )
            path.write_text(gen_markdown(yaml_input_data, markdown), encoding="utf-8")

    sys.exit(0)


def command_gen_input(
    yaml_input_data: Input, specs: list[str], perf_mode: bool
) -> NoReturn:
    """gen-input subcommand: generate a random raw input"""
    print(generate_random_input(yaml_input_data, specs, perf_mode), end="")
    sys.exit(0)


def command_validate(
    yaml_input_data: Input, input_files: Iterable[Path], perf_mode: bool
) -> NoReturn:
    """validate subcommand: validate input"""
    all_inputs_valid = True

    for input_file in input_files:
        status = input_errors(yaml_input_data, str(input_file), perf_mode)
        if status:
            print(f"Input {input_file} is invalid: {status}", file=sys.stderr)
            all_inputs_valid = False

    if all_inputs_valid:
        print("All inputs are valid")
        sys.exit(0)
    else:
        print("Some inputs are invalid, see above", file=sys.stderr)
        sys.exit(3)


def command_run(
    yaml_input_data: Input, name: str, input_files: Iterable[Path], languages: list[str]
) -> NoReturn:
    """
    run subcommand: check that generated parsers properly parse and print the
    input they are fed with
    """
    success = True

    for language in languages:
        print(language, end=" ", flush=True)
        if not gen_compile_run_and_compare(
            yaml_input_data,
            name,
            LANGUAGES_MAPPING[language],
            "run",
            [str(f) for f in input_files],
        ):
            print("\nError with", language)
            success = False

    print("\nSUCCESS" if success else "\nFAILURE")
    sys.exit(0 if success else 1)


def build_parser() -> argparse.ArgumentParser:
    """Create the ArgumentParser for iorgen"""
    parser = argparse.ArgumentParser(
        prog="iorgen",
        description="Multi-languages parser generator",
    )

    commands = parser.add_subparsers(
        title="required subcommand", required=True, dest="command"
    )
    gen_stubs_cmd = commands.add_parser(
        "gen-stubs",
        description=(
            "Generate subject stub and parsers for all the selected languages (also"
            " called skeletons)"
        ),
        help="Generate subject and code stubs (skeletons)",
    )
    gen_input_cmd = commands.add_parser(
        "gen-input",
        description="Generate a random raw input",
        help="Generate a random raw input",
    )
    validate_cmd = commands.add_parser(
        "validate", description="Validate input", help="Validate input"
    )
    run_cmd = commands.add_parser(
        "run",
        description=(
            "Check that generated parsers properly parse and print the input they are"
            " fed with."
        ),
        help="Check that parsers properly parse",
    )

    gen_stubs_cmd.add_argument(
        "--markdown",
        "-m",
        default="fr",
        help=(
            "language for the subject in markdown (None disables subject stub"
            " generation) [default: %(default)s]"
        ),
        choices=["None", "en", "fr"],
    )
    gen_stubs_cmd.add_argument(
        "--output_dir",
        "-o",
        default="skeleton",
        metavar="output-dir",
        type=Path,
        help=(
            "directory in which code stubs will be generated. If it does not exists, it"
            " will be created. [default: %(default)s]"
        ),
    )
    gen_stubs_cmd.add_argument(
        "--same_dir",
        "-x",
        default=False,
        action="store_true",
        help=(
            "when generating code and subject stubs, if set to True, will generate in"
            " the same directory as the iorgen input file instead of the current working"
            " directory. [default: %(default)s]"
        ),
    )
    gen_stubs_cmd.add_argument(
        "yaml_list",
        metavar="file.yaml",
        type=Path,
        help="list of iorgen input files (YAML format)",
        nargs="+",
    )

    gen_input_cmd.add_argument(
        "--specify",
        "-s",
        default=[],
        nargs="*",
        metavar="NAME VALUE",
        help=(
            "specify min or max values of input lists you want (NAME_max VALUE to"
            " specify max value for NAME variable)"
        ),
    )

    # `--languages` option` for `gen_stubs`, `gen-input` and `run` subcommands
    for cmd in [gen_stubs_cmd, run_cmd]:
        cmd.add_argument(
            "--languages",
            "-l",
            action="append",
            help="languages for which to generate a parser",
            choices=LANGUAGE_EXTENSIONS,
        )

    # `--perf_mode` option` for `gen-input` and `validate` subcommands
    for cmd in [gen_input_cmd, validate_cmd]:
        cmd.add_argument(
            "--perf_mode",
            "-p",
            default=False,
            action="store_true",
            help="perf mode [default: %(default)s]",
        )

    # `yaml` argument` for `gen-inputs`, `validate` and `run` subcommands
    for cmd in [gen_input_cmd, validate_cmd, run_cmd]:
        cmd.add_argument(
            "yaml",
            metavar="file.yaml",
            type=Path,
            help="iorgen input file (YAML format)",
        )

    # `inputs`` argument for `validate` and `run` subcommands
    for cmd in [validate_cmd, run_cmd]:
        cmd.add_argument(
            "inputs",
            metavar="file_glob",
            help="list of input files globs (supports wildcards)",
            nargs="+",
        )

    return parser


def main() -> NoReturn:
    """The iorgen module execution"""
    parser = build_parser()
    args = parser.parse_args()

    try:
        args.languages = args.languages or [lang.extension for lang in ALL_LANGUAGES]
    except AttributeError:
        # raised if `languages` is not an argument of the specified subcommand,
        # can be safely ignored since it won't be used if not given
        pass

    if args.command == "gen-stubs":
        command_gen_stubs(
            args.yaml_list,
            args.output_dir,
            args.same_dir,
            args.languages,
            args.markdown,
        )

    iorgen = parse_yaml_input_or_exit(args.yaml)

    if args.command == "gen-input":
        command_gen_input(iorgen, args.specify, args.perf_mode)
    elif args.command == "validate":
        command_validate(iorgen, globs_to_paths(args.inputs), args.perf_mode)
    elif args.command == "run":
        command_run(iorgen, args.yaml.stem, globs_to_paths(args.inputs), args.languages)

    raise RuntimeError("Unreachable code")


if __name__ == "__main__":
    main()
