## Subject

This reads several kinds of list.

### Input

The input will contain:

- On the first line, an integer: **N**, the first list's size.
- On the next line, a list of **N** integers separated by spaces: **list int**,
  a list containing ints.
- On the next line, an integer: **size**, an other size.
- On the next line, a list of **size** chars next to each other: **list char**,
  a list of char.
- On the next line, a string of size **20** or less: **string**, a string.
- On the next lines, a list of **size** elements: **list string4**, a list of
  strings of size 4.
    - One line per list element: a string of size **4** or less.
- On the next lines, a list of **2** elements: **list list string2**, a list of
  list of strings of size 2 of size 2 of size 2.
    - Each list element is on several lines: a list of **2** elements.
        - One line per list element: a string of size **2** or less.
- On the next lines, a list of **size** elements: **matrix**, a matrix of int.
    - One line per list element: a list of **size** integers separated by
      spaces.

### Output

Aren't these lists beautifull?

### Constraints

- $1 \le N \le 10$
- $1 \le (list int)[ ] \le 2\,000$
- $0 \le size \le 5$
- $(list char)[ ] \in \{f, o\}$
- $0 \le matrix[ ] \le 3\,456$

### Performance constraints

- $0 \le matrix[ ] \le 10\,000$
