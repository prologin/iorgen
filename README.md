IO Reader GENerator
===================

## Description

*Iorgen* is a multi languages code generator to parse a predefined input
template. The user writes a YAML describing the input, and *Iorgen* will
generate the code to read this input from stdin, in all supported languages.

The list of currently supported languages is: C, C++, C#, D, Go, Haskell, Java,
Javascript, Lua, Ocaml, Pascal, Perl, PHP, Prolog, Python, Ruby, Rust, Scheme.
A markdown description of the input in English and French can also be
generated.

## Installation

You can install `iorgen` in a virtual environment like this:

```
git clone git@github.com:prologin/iorgen.git
cd iorgen
python3 -m venv .venv
source .venv/bin/activate
pip3 install -e .
```

You can then run it with the `iorgen` command.

### Testing the languages

You should be able to trust that *Iorgen* will generate valid files. But if you
want to be sure that those files are valid, and want to generate the test suite
for instance, you will have to install lots of compilers.

The complete dependencies for Archlinux are:
```shell
pacman -S --needed python-yaml fpc gambit-c gcc gdc ghc go jdk8-openjdk lua \
    mono nodejs ocaml perl php ruby rust swi-prolog
```

For Debian 9 (and probably Ubuntu), they are:
```shell
sudo apt install python3-yaml mono-mcs gdc golang ghc openjdk-8-jdk-headless \
    nodejs-legacy lua5.3 ocaml-nox fp-compiler php-cli swi-prolog-nox ruby \
    rustc gambc
```

## Usage

Make sure python (version 3.5 and above) and python-yaml are installed on your
computer and run `python3 -m iorgen input.yaml`. This will generate all
languages parsers in a `skeleton` folder, and a `subject-io-stub.md` describing
the input (in French by default).

Several options are available:

- The `--output_dir` option specify the path and name of the directory holding
  the generated codes. The filenames will be the same as the input yaml, with
  the extensions replaced by the canonical extension of each language.
- The `--languages` option can generate a subselections of languages.
- The `--markdown` option specify the language (not programming language) in
  which the `subject-io-stub.md` will be written. The `subject-io-stub.md` is a
  file generated next to the output directory, and not inside.
- The `--validate` option, changes completly *Iorgen* behavior. Instead of
  generating parsers reading some raw input, *Iorgen* will here directly read
  a file containing such a raw input, and check that it is valid, that is,
  that it matches the format described in the input YAML.
- The `--generate_random` also changes the behavior. It will not generate a
  parser, but a valid possible raw input.
- The `--specify` is to be used with `--generate_random`, it allows the user
  to specify the value, the min value or the max value of a variable for this
  output. `NAME VALUE` set the value. `NAME_max VALUE` (or `NAME_min VALUE`)
  set the maximum value (or the minimum).
  ```shell
  $ iorgen -g -s N_max 10 integer 42 -- example.yaml
  ```
- The `perf_mode` option is used with the `--validate` or `--generate_random`
  mode. This means that the raw input will be treated as in performance mode.
  The performance mode is a mode where the constraints are differents, usually
  the integers are bigger.
- The `--run` option changes the behavior of *Iorgen*: generated parsers are
  written in a temporary folder, and *Iorgen* will compile those parsers and
  run them with the input given in arguments to the `run` option (a single
  path that can represent many input files thanks to a wildcard support).
  It will check that the parser is able to recreate exactly the input, and can
  be used as a proof that the generated parser is working. Using this option
  requires compilers of all tested languages to be installed (see
  [this section](#testing-the-languages) to know more).


## Input format

### Types

*Iorgen* can use the following types:

- **Integer**: the default integer type for the language
- **Char**: can be either a byte, or a string depending of the language
- **String**: a string with a given maximum size
- **List**: an array, list, vector… of a given size, containing one of the
  *Iorgen* supported types
- **Struct**: a C like struct, or a map which have strings as keys; each field
  can have any of *Iorgen* supported types (except the exact same struct)

### Format

The input is described in [YAML](http://yaml.org/), and must have the following
format:

- A `"function_name"` field, containing the name of the generated function
- A `"subject"` field, containing a string (can be several paragraphs)
  describing what the input is about (will no be used in generated code)
- An `"ouput"` field, containing a string (can be several paragraphs)
  describing what the end user have to do with the parsed input
- An `"input"` field, containing a list of variables. Each variable is a map
  with the following fields:
    - A `"type"` field, containing a string (see the type syntax below)
    - A `"name"` field, containing a string: the variable’s name
    - A `"comment"` field, containing a string: a description of the variable
    - An optional `"min"` field, if the variable is a integer, or a list (or
      list or list, or list of list of list, etc) of integers. This will be
      the minimal value possible for this variable. This is used in the
      markdown generator to show the constraints, and in some langages
      generators to check if the size of a list or a string is garantied to be
      not null. The `"min"` field can either be an integer, or a variable name.
    - An optional `"max"` field (similar to the `"min"` one).
    - An optional `"min_perf"` field: like the `"min"` one, but only used in
      the case of _performance_ cases, often meaning that the variable will
      have a very big value.
    - An optional `"max_perf"` field (similar to the `"min_perf"` one).
    - An optional `"choices"` field, if the variable is a char or a integer, or
      a list (or list of list, etc) of chars or integers (for this definition
      a string is considered as a list of chars). `"choices"` is a list of
      values possible for this integer or char. If this list is not empty,
      then the `"min"` fields and similar fields will be ignored.
- An optional `"structs"` field, if your input uses structs, a list of structs.
  Each struct is a map with the following fields:
    - A `"name"` field, containing a string: the struct’s name
    - A `"comment"` field, containing a string: a description of the struct
    - A `"fields"` field, containing a list of the struct’s fields (same syntax
      as `"input"`)

### Syntax

Any `"name"` field (or `"function_name"`) can hold any alphanumic character or
spaces, but must start with a letter, and can not have trailing whitespaces.
You do not have to worry about the name beeing a language’s keyword: it will
automatically be modified if that is the case, usually by adding a trailing
underscore.

A `"comment"` field can hold any character other than a newline. For now,
strings that end comments in some languages, such as `*/` should be avoided. A
protection against this will be added in a later version.

A `"type"` field must have one of the following format `int`, `char`,
`str(size)`, `List[type](size)`, `@structname`. You must replace `size`, `type`
and `structname` following this guidelines:

- `size` can be either a number, or a variable name. If it is a variable name,
  it must be a toplevel one (i.e. in the `"input"` list), and must have been
  declared before use. One exception: you can use a struct with two fields:
  one integer, and a other a type whose size is the first field. For strings,
  the given size, in the maximum size the string will have, but it could be
  less.
- `type` can be any valid type, even an other list
- `structname` is the name of a struct, as declared in the `"name"` field of
  `"structs"`

### Example

```yaml
function_name: example
subject: This input is an example for Iorgen's README
structs:
    - name: a struct
      comment: A struct for the example
      fields:
          - type: int
            name: integer
            comment: an integer
            choices: [-4, 42, 1337]
          - type: char
            name: character
            comment: a char
            choices: [a, b, c]
input:
    - type: int
      name: N
      comment: a number, used as a size
      min: 1
      max: 10
      max_perf: 10000
    - type: List[@a struct](N)
      name: list
      comment: a list of structs
output: In a real life scenario, you will describe here what you want the end
    user to do with this generated code
```

If you want to generate the C code for parsing this kind of input, run
`python3 -m iorgen -l c example.yaml`, and you will get the following
`skeleton/example.c`:

```C
#include <stdio.h>
#include <stdlib.h>

/// A struct for the example
struct a_struct {
    int integer; ///< an integer
    char character; ///< a char
};

/// \param n a number, used as a size
/// \param list a list of structs
void example(int n, struct a_struct* list) {
    /* TODO In a real life scenario, you will describe here what you want the
    end user to do with this generated code */
}

int main() {
    int n;
    scanf("%d", &n);
    struct a_struct* list = (struct a_struct*)malloc(n * sizeof(struct a_struct));
    for (int i = 0; i < n; ++i) {
        scanf("%d %c", &list[i].integer, &list[i].character);
    }
    example(n, list);

    return 0;
}
```

You can see every other thing that _Iorgen_ can generate in the
[test samples](test/samples/example/); you can find parsers for lots of
languages, and also a [generated description of the input in
YAML](test/samples/example/example.en.md).
