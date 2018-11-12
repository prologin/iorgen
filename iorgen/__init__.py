# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Input parser generators"""

from iorgen.checkinput import parse_input
from iorgen.types import Input, Struct, Variable
from iorgen.parser_c import gen_c
from iorgen.parser_cpp import gen_cpp
from iorgen.parser_csharp import gen_csharp
from iorgen.parser_haskell import gen_haskell
from iorgen.parser_java import gen_java
from iorgen.parser_javascript import gen_javascript
from iorgen.parser_ocaml import gen_ocaml
from iorgen.parser_php import gen_php
from iorgen.parser_python import gen_python
from iorgen.parser_rust import gen_rust
from iorgen.generator import Language, ALL_LANGUAGES
