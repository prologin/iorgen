## Subject

This will generate a program reading floats.

### Input

The input will contain:

- On the first line, a floating-point number: **f**, a float.
- On the next line, a floating-point number: **g**, a float, greater than f.
- On the next line, separated by spaces, a floating-point number **x** (X), a
  floating-point number **y** (Y), and a floating-point number **z** (Z):
  **point**, some coordinates.
- On the next line, an integer: **n**, a number.
- On the next line, a list of **n** floating-point numbers separated by spaces:
  **float list**, a list of floats.
- On the next line, a list of **9** floating-point numbers separated by spaces:
  **other list**, a list of floats.
- On the next lines, a list of **3** elements: **inlined**, some inlined
  structs.
    - One line per list element: separated by spaces, an integer **integer**
      (an integer), a char **char** (a char), and a floating-point number
      **float** (a float).
- On the next lines, a struct **multiline mix**.
    - On the first line, an integer: **integer 2**, an other integer.
    - On the next line, a string of size **5** or less: **string**, a string of
      size 5.
    - On the next line, a floating-point number: **float 2**, an other float.

### Output

Parsing is often easy, reprint mode is harder

### Constraints

- $-3.7 \le f \le 3\,890\,000.0$
- $f \le g$
- $0 \le n \le 10$
- $x \in \{0.3, 4.0, 8.0\}$
- $0.0 \le y$
- $z \le 12.0$
