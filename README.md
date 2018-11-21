IO Reader GENerator
===================

## Description

*Iorgen* is a multi languages code generator to parse a predefined input
template. The user writes a YAML describing the input, and *Iorgen* will
generate the code to read this input from stdin, in all supported languages.

The list of currently supported languages is: C, C++, C#, Go, Haskell, Java,
Javascript, Ocaml, PHP, Python, Ruby, Rust. A markdown description of the input
in English and French can also be generated.

## Usage

Make sure python (version 3.5 and above) and python-yaml are installed on your
computer and run `python3 -m iorgen input.yaml`. You can see other options,
like selecting only a subset of language, by displaying the help with the `-h`
flag.

## Input format

### Types

*Iorgen* can use the following types:

- **Integer**: the default integer type for the language
- **Char**: can be either a byte, or a string depending of the language
- **String**: a string of a given size
- **List**: an array, list, vector… of a given size, containing one of the
  *Iorgen* supported types
- **Struct**: a C like struct, or a map which have strings as keys; each field
  can have any of *Iorgen* supported types (except the exact same struct)

### Format

The input is described in [YAML](http://yaml.org/), and must have the following
format:

- A `"name"` field, containing a short string: the input’s name (this will be
  the name of the generated function)
- A `"subject"` field, containing a string (can be several paragraphs)
  describing what the input is about (will no be used in generated code)
- An `"ouput"` field, containing a string (can be several paragraphs)
  describing what the end user have to do with the parsed input
- An `"input"` field, containing a list of variables. Each variable is a map
  with the following fields:
    - A `"type"` field, containing a string (see the type syntax below)
    - A `"name"` field, containing a string: the variable’s name
    - A `"comment"` field, containing a string: a description of the variable
- An optional `"structs"` field, if your input uses structs, a list of structs.
  Each struct is a map with the following fields:
    - A `"name"` field, containing a string: the struct’s name
    - A `"comment"` field, containing a string: a description of the struct
    - A `"fields"` field, containing a list of the struct’s fields (same syntax
      as `"input"`)

### Syntax

Any `"name"` field can hold any alphanumic character or spaces, but must start
with a letter, and can not have trailing whitespaces. You do not have to worry
about the name beeing a language’s keyword: it will automatically be modified
if that is the case, usually by adding a trailing underscore.

A `"comment"` field can hold any character other than a newline. For now,
strings that end comments in some languages, such as `*/` should be avoided. A
protection against this will be added in a later version.

A `"type"` field must have one of the following format `int`, `char`,
`str(size)`, `List[type](size)`, `@structname`. You must replace `size`, `type`
and `structname` following this guidelines:

- `size` can be either a number, or a variable name. If it is a variable name,
  it must be a toplevel one (i.e. if the `"input"` list), and must have been
  declared before use. One exception: you can use a struct with two fields:
  one integer, and a other a type whose size is the first field.
- `type` can be any valid type, even an other list
- `structname` is the name of a struct, as declared in the `"name"` field of
  `"structs"`

### Example

```yaml
name: example
subject: This input is an example for Iorgen's README
structs:
    - name: a struct
      comment: A struct for the example
      fields:
          - type: int
            name: integer
            comment: an integer
          - type: char
            name: character
            comment: a char
input:
    - type: int
      name: N
      comment: a number, used as a size
    - type: List[@a struct](N)
      name: list
      comment: a list of structs
output: In a real life scenario, you will describe here what you want the end
    user to do with this generated code
```

If you want to generate the C code for parsing this kind of input, run
`python3 -m iorgen -l c example.yaml`, and you will get:

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
    int n; ///< a number, used as a size
    scanf("%d\n", &n);
    struct a_struct* list = calloc(n, sizeof(struct a_struct)); ///< a list of structs
    for (int i = 0; i < n; ++i) {
        scanf("%d %c\n", &list[i].integer, &list[i].character);
    }
    example(n, list);

    return 0;
}
```
