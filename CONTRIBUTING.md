Contributing to Iorgen
======================

Thank for wanting to contribute to *Iorgen*. Here are a few things to know if
you want to contribute. Please keep in mind while contributing, that this
project main goal is to be used for the
[Prologin contest](https://prologin.org/). If by any chance, someone wants to
add features not needed for Prologin, they may, but only if this makes it not
harder to maintain this tool for Prologin.

Dependencies
------------

To develop for *Iorgen*, you will need to run some of its tests, so you will
need to install:

- Every dependencies needed to run *Iorgen* itself: meaning Python 3.6 or
  above, and PyYAML. You can install PyYAML through your package manager (it
  will be probably be named `python-yaml`, or `python3-yaml`), or with `pip3
  install pyyaml`.
- Every compiler for every language supported by *Iorgen*, as listed
  [in the README](README.md#testing-the-languages). If you are working only on
  one language, and do not touch the others, you may only install a subset of
  the compilers.
- The tools to check the code quality: black, mypy and pylint. Be sure to have
  the latest version of them. You can install them through you package manager
  **if you have an up-to-date** distribution (like Archlinux), or else install
  them with `pip3 install -U pyyaml mypy types-PyYAML pylint black`. The CI
  build use fixed versions of these tools to prevent the build failing
  unexpectedly with new changing releasing of the tools, but these version are
  meant to be kept up to date.

Coding style
------------

The code should follow the Python
[PEP 8](https://www.python.org/dev/peps/pep-0008/), and other Python good
practices.

Also, all code uses [type
hints](https://docs.python.org/3/library/typing.html). This helps tools to
check the code better.

To ensure your code follow the coding style, please run the `./check_code.sh`
script.

Architecture
------------

The *Iorgen* module is written in the `iorgen` directory. The files that might
interess you are `types.py` that declare the types *Iorgen* is working with:
variables, structs, etc. Those types are then generated in parser code with
the `parser_{lang}.py` files.

The `parser_{lang}.py` files are all independants, and often their only
dependencies are `types.py` and `utils.py`. The latter contains some common
function that can be used by several parser generators. Do not try to factorize
the code too much between the different languages. You might think, for
instance, that C and C++ are very close, and thus some functions between the
two languages might be factorized. But it only makes the code more complicated
for both languages, and it makes it close to impossible to made some tweaks to
one of the two. *Iorgen*'s premise is that each language will have its own
specificities, and will have a unique way to be generated.

If you want to add support for a new language, just add a new
`parser_{lang}.py`. It should only contain a `gen_{lang}` function, taking
in parameter an `Input` and a `bool` (for the “reprint” mode), and returning a
string: the content of the generated parser. Then add an entry in
`generator.py` to automatically add support for the language in *Iorgen*. Also,
add the `gen_{lang}` function in `__init__.py` in case someone wants to use
*Iorgen* from a script.

Tests
-----

*Iorgen* has tests located in the `test` folder.

There is no unit tests, because it is not very relevant for *Iorgen*: we want
to be sure a generated parser works in its whole, not that the line in the
middle works with its very specific input. You may add some unit tests if it is
relevant in your case, but for language generators, I think you will probably
just lose some time.

There are two kind of tests. The first one is to make sure the generated code
stays the same with previous versions. This is useful when you do a refactor,
and do not want to change previous behavior. The reference files are committed
in `samples`, and are generated from the YAML file located in each of these
folders. If you made some change in the code that affect the content of those
file, simply run the `./regenerate.py` file.

The second test consists in generating all the parsers in the “reprint” mode,
compiling them, and then executing them with a sample input in stdin. The
program should then output the exact same thing as the input, proving that it
has correctly parsed everything in stdin. The standard input is given by the
file with extension `.sample_input`.

To run the tests, run the command `./test.py`. You will often want to run
`./regenerate.py` beforehand: be sure to then check with a `git diff` that the
modifications are the one you wanted to make to the generated files.

If you want to run the tests for one language only (because you do not have all
the compilers installed, or simply because it is much faster), run
`./test -l {language_extension}`.

Contributing code
-----------------

When your changes are to your liking, please check the following before doing a
pull request:

- You have ran the tests with success (`cd test && ./test.py`).
- Your code respects the coding style
  (`pip3 install -U mypy types-PyYAML pylint pyyaml`, then `./check_code.py`).
- You agree to publish your code under the
  [GNU General Public License v3, or later](COPYING). Place a line on top of
  the files you have modified with `# Copyright YYYY Name` (under the other
  ones) so that we can keep track of the copyright holders smoothly.
