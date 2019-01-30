# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Input parser generators"""

from iorgen.checkinput import parse_input
from iorgen.types import Input, Struct, Variable
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
from iorgen.generator import Language, ALL_LANGUAGES, ALL_MARKDOWN
from iorgen.random_input import generate_random_input
from iorgen.validator import input_errors
