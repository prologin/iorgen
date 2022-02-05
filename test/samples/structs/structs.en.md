## Subject

This will generate a program reading structs.

### Input

The input will contain:

- On the first line, separated by spaces, an integer **foo** (a field), and an
  integer **bar** (a field): **struct**, a struct 1 instance.
- On the next line, an integer: **n**, a number.
- On the next lines, a list of **n** elements: **struct list**, a list a struct
  1.
    - One line per list element: separated by spaces, an integer **foo** (a
      field), and an integer **bar** (a field).
- On the next lines, a list of **3** elements: **triangle**, a triangle.
    - Each list element is on several lines: a struct **point**.
        - On the first line, a char: **name**, the point's name (single
          character).
        - On the next line, a string of size **12** or less: **description**,
          the point's description.
        - On the next line, separated by spaces, an integer **x** (X), an
          integer **y** (Y), and an integer **z** (Z): **pos**, the point's
          position.
- On the next line, separated by spaces, a char **first char** (a first char),
  a char **second char** (a second char), and a char **third char** (a third
  char): **struct chars**, a struct of chars.
- On the next lines, a struct **with list**.
    - On the first line, an integer: **int**, int.
    - On the next lines, a list of **2** elements: **big list**, list nested 3
      times!.
        - Each list element is on several lines: a list of **2** elements.
            - One line per list element: a list of **2** integers separated by
              spaces.

### Output

Look at them structs.

### Constraints

- $0 \le n$
- $foo \in \{1, 3, 8, 28, 43\}$
- $2 \le bar \le 99$
- $name \in \{A, B, O\}$

### Performance constraints

- $2 \le n$
