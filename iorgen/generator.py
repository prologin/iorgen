# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Helpers to generate, compile and run parsers for all supported languages"""

import subprocess
import os
from typing import Callable, Optional, List

from iorgen.types import Input
from iorgen.markdown import gen_markdown
from iorgen.parser_c import gen_c
from iorgen.parser_cpp import gen_cpp
from iorgen.parser_csharp import gen_csharp
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

    def __init__(self,
                 extension: str,
                 generator: Callable[[Input, bool], str],
                 compile_command: List[str],
                 exec_command: Optional[List[str]] = None,
                 no_stderr: bool = False) -> None:
        # pylint: disable=too-many-arguments
        self.extension = extension
        self.generator = generator
        self.compile_command = compile_command
        self.exec_command = [] if exec_command is None else exec_command
        self.no_stderr = no_stderr

    def compile(self, filename: str) -> str:
        """Compile the file at location 'filename'"""
        if self.compile_command:
            cwd = os.getcwd()
            os.chdir(os.path.dirname(filename))
            name = filename[:-len(self.extension) - 1]
            command = [i.format(name=name) for i in self.compile_command]
            stderr = subprocess.DEVNULL if self.no_stderr else None
            subprocess.run(command + [filename], stderr=stderr)
            os.chdir(cwd)
            return name
        return filename

    def compile_and_run(self, filename: str, input_file: str) -> str:
        """Compile filename, and run the executable with input_file as stdin"""
        exe = self.compile(filename)
        out = ""
        with open(input_file) as sample_input:
            cwd = os.getcwd()
            os.chdir(os.path.dirname(filename))
            res = subprocess.run(
                self.exec_command + [exe],
                stdin=sample_input,
                stdout=subprocess.PIPE)
            os.chdir(cwd)
            out = res.stdout.decode()
        return out

    def generate(self, input_data: Input) -> str:
        """Generate an input parser with a function to complete"""
        return self.generator(input_data, False)


ALL_LANGUAGES = [
    Language(
        "c", gen_c,
        ["gcc", "-std=c11", "-Wall", "-Wextra", "-O2", "-lm", "-o", "{name}"]),
    Language("cpp", gen_cpp,
             ["g++", "-std=c++17", "-Wall", "-Wextra", "-O2", "-o", "{name}"]),
    Language("cs", gen_csharp, ["mcs", "-optimize", "-out:{name}"], ["mono"]),
    Language("go", gen_go, ["go", "build", "-buildmode=exe"]),
    Language(
        "hs", gen_haskell,
        ["ghc", "-v0", "-Wall", "-Wno-name-shadowing", "-dynamic", "-O2"]),
    Language("java", gen_java, ["javac", "-encoding", "UTF-8"],
             ["java", "Main"]),
    Language("js", gen_javascript, [], ["node"]),
    Language("lua", gen_lua, [], ["lua5.3"]),
    Language("ml", gen_ocaml, ["ocamlopt", "-w", "A", "-o", "{name}"]),
    Language("pas", gen_pascal, ["fpc", "-v0ew", "-l-"], no_stderr=True),
    Language("php", gen_php, [], ["php"]),
    Language("pl", gen_perl, [], ["perl"]),
    Language("pro", gen_prolog, [], ["swipl", "--quiet", "-t", "halt", "-l"]),
    Language("py", gen_python, [], ["python3", "-S"]),
    Language("rb", gen_ruby, [], ["ruby"]),
    Language("rs", gen_rust, ["rustc", "-W", "warnings", "-O"]),
    Language("scm", gen_scheme, [], ["gsi"])
]

ALL_MARKDOWN = [
    Language("en.md", (lambda i, _: gen_markdown(i, 'en')), []),
    Language("fr.md", (lambda i, _: gen_markdown(i, 'fr')), [])
]
