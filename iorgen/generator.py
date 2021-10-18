# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2021 Sacha Delanoue
"""Helpers to generate, compile and run parsers for all supported languages"""

import subprocess
import os
import tempfile
from difflib import unified_diff
from pathlib import Path
from typing import Callable, Iterator, List, Optional

from iorgen.types import Input
from iorgen.markdown import gen_markdown
from iorgen.parser_c import gen_c
from iorgen.parser_cpp import gen_cpp
from iorgen.parser_csharp import gen_csharp
from iorgen.parser_d import gen_d
from iorgen.parser_go import gen_go
from iorgen.parser_haskell import gen_haskell
from iorgen.parser_java import gen_java
from iorgen.parser_javascript import gen_javascript
from iorgen.parser_lua import gen_lua
from iorgen.parser_ocaml import gen_ocaml
from iorgen.parser_pascal import gen_pascal
from iorgen.parser_perl import gen_perl
from iorgen.parser_php import gen_php
from iorgen.parser_prolog import gen_prolog
from iorgen.parser_python import gen_python
from iorgen.parser_ruby import gen_ruby
from iorgen.parser_rust import gen_rust
from iorgen.parser_scheme import gen_scheme


class Language:
    """Describe how to generate, compile and run, the parser for a language"""

    def __init__(
        self,
        extension: str,
        generator: Callable[[Input, bool], str],
        compile_command: List[str],
        exec_command: Optional[List[str]] = None,
        no_stderr: bool = False,
    ) -> None:
        # pylint: disable=too-many-arguments
        self.extension = extension
        self.generator = generator
        self.compile_command = compile_command
        self.exec_command = [] if exec_command is None else exec_command
        self.no_stderr = no_stderr

    def compile(self, filename: str) -> str:
        """Compile the file at location 'filename'"""
        if self.compile_command:
            name = filename[: -len(self.extension) - 1]
            command = [i.format(name=name) for i in self.compile_command]
            stderr = subprocess.DEVNULL if self.no_stderr else None
            subprocess.run(
                command + [filename],
                stderr=stderr,
                cwd=os.path.dirname(filename),
                check=True,
            )
            return name
        return filename

    def run(self, exe: str, filename: str, input_file: str) -> str:
        """Run the executable (by self.compile) with input_file as stdin"""
        out = ""
        with open(input_file, encoding="utf-8") as sample_input:
            res = subprocess.run(
                self.exec_command + [exe],
                stdin=sample_input,
                stdout=subprocess.PIPE,
                cwd=os.path.dirname(filename),
                check=True,
                universal_newlines=True,  # also known as 'text' in python >= 3.7
            )
            out = res.stdout
        return out

    def generate(self, input_data: Input) -> str:
        """Generate an input parser with a function to complete"""
        return self.generator(input_data, False)


ALL_LANGUAGES = [
    Language(
        "c",
        gen_c,
        ["gcc", "-std=c11", "-Wall", "-Wextra", "-O2", "-lm", "-o", "{name}"],
    ),
    Language(
        "cpp", gen_cpp, ["g++", "-std=c++17", "-Wall", "-Wextra", "-O2", "-o", "{name}"]
    ),
    Language("cs", gen_csharp, ["mcs", "-optimize", "-out:{name}"], ["mono"]),
    Language("d", gen_d, ["gdc", "-Wall", "-O2", "-o", "{name}"]),
    Language("go", gen_go, ["go", "build", "-buildmode=exe"]),
    Language(
        "hs",
        gen_haskell,
        ["ghc", "-v0", "-Wall", "-Wno-name-shadowing", "-dynamic", "-O2"],
    ),
    Language("java", gen_java, ["javac", "-encoding", "UTF-8"], ["java", "Main"]),
    Language("js", gen_javascript, [], ["node"]),
    Language("lua", gen_lua, [], ["lua"]),
    Language("ml", gen_ocaml, ["ocamlopt", "-w", "A-24", "-o", "{name}"]),
    Language("pas", gen_pascal, ["fpc", "-v0ew", "-l-"], no_stderr=True),
    Language("php", gen_php, [], ["php"]),
    Language("pl", gen_perl, [], ["perl"]),
    Language("pro", gen_prolog, [], ["swipl", "--quiet", "-t", "halt", "-l"]),
    Language("py", gen_python, [], ["python3", "-S"]),
    Language("rb", gen_ruby, [], ["ruby"]),
    Language("rs", gen_rust, ["rustc", "-W", "warnings", "-O"]),
    Language("scm", gen_scheme, [], ["gsi"]),
]

ALL_MARKDOWN = [
    Language("en.md", (lambda i, _: gen_markdown(i, "en")), []),
    Language("fr.md", (lambda i, _: gen_markdown(i, "fr")), []),
]


def print_colored_diff(lines: Iterator[str]) -> None:
    """Print a diff with some console colors"""
    for i, line in enumerate(lines):
        if i < 2:
            print("\033[1m" + line + "\033[0m", end="")
        elif line[0:2] == "@@":
            print("\033[96m" + line + "\033[0m", end="")
        elif line[0] == "+":
            print("\033[92m" + line + "\033[0m", end="")
        elif line[0] == "-":
            print("\033[91m" + line + "\033[0m", end="")
        else:
            print(line, end="")


def compare_files(
    generated_content: List[str], reference_filename: str, tofile: str = "generated"
) -> bool:
    """Check if a generated result is the same as a reference file"""
    ref = Path(reference_filename).read_text(encoding="utf-8").splitlines(True)
    if generated_content != ref:
        print_colored_diff(
            unified_diff(
                ref, generated_content, fromfile=reference_filename, tofile=tofile
            )
        )
        return False
    return True


def gen_compile_run_and_compare(
    input_data: Input,
    name: str,
    language: Language,
    folder_for_generated_source: str,
    stdin_filename: List[str],
    no_compile: bool = False,
) -> bool:
    # pylint: disable = too-many-arguments
    """Check that the generated parser prints the input it is fed in"""
    source = os.path.join(
        tempfile.gettempdir(),
        "iorgen",
        folder_for_generated_source,
        language.extension,
        name,
        name + "." + language.extension,
    )

    # Generate source
    generated = language.generator(input_data, True)
    Path(os.path.dirname(source)).mkdir(parents=True, exist_ok=True)
    Path(source).write_text(generated, encoding="utf-8")

    if no_compile:
        return True

    # Compile and compare
    exe = language.compile(source)
    success = True
    for file_ in stdin_filename:
        stdout = language.run(exe, source, file_).splitlines(True)
        success = (
            compare_files(stdout, file_, "generated from " + language.extension)
            and success
        )
    return success
