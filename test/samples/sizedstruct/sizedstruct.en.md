## Subject

A special kind a struct containing the size of a list, or string.

### Input

The input will contain:

- On the first line, an integer: **n**, the size of the lists.
- On the next lines, a list of **n** elements: **lists**, a list of list of
  different sizes.
    - Each list element is on several lines: a struct **list**.
        - On the first line, an integer: **size1**, the list's size.
        - On the next line, a list of **size1** integers separated by spaces:
          **int list**, the integer list.
- On the next lines, a list of **n** elements: **strings**, a list of strings
  of different sizes.
    - Each list element is on several lines: a struct **string**.
        - On the first line, an integer: **size2**, the list's size.
        - On the next line, a string of size **size2**: **string list**, the
          string list.
- On the next lines, a list of **2** elements: **matrices**, a list of matrices
  of different sizes.
    - Each list element is on several lines: a struct **matrix**.
        - On the first line, an integer: **size3**, the list's size.
        - On the next lines, a list of **size3** elements: **list list**, the
          list list.
            - One line per list element: a list of **2** integers separated by
              spaces.
- On the next lines, a list of **n** elements: **same**, a list of list of same
  sizes.
    - Each list element is on several lines: a struct **not a sized struct**.
        - On the first line, an integer: **size4**, not the list's size.
        - On the next line, a list of **n** integers separated by spaces: **int
          list n**, the integer list.

### Output

The is a special case.

### Constraints

- $0 \le n$
- $0 \le size1$
- $1 \le size2$
- $0 \le size3$
