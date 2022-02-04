## Subject

Very rarely, some lines may be empty, because we read a list or a string that
has an empty size. Here we check that those cases work well.

### Input

The input will contain:

- On the first line, a list of **0** integers separated by spaces: **empty
  list**, an empty list.
- On the next line, a string of size **3** or less: **buffer string**, here to
  check correct parsing of empty line above.
- On the next line, an integer: **N**, an integer, will be 0 in the sample
  input.
- On the next line, a list of **N** integers separated by spaces: **empty in
  sample**, an empty list (only in the sample).
- On the next line, a string of size **0** or less: **empty string**, an empty
  string.
- On the next line, a string of size **4** or less: **main**, an other buffer
  string.
- On the next line, a list of **0** chars next to each other: **empty char
  list**, an empty char list.
- On the next line, a list of **5** chars next to each other: **non empty char
  list**, an char list, non empty.
- On the next lines, a struct **a**.
    - On the first line, a list of **N** integers separated by spaces: **list
      in struct**, a list in a struct.
    - On the next line, separated by spaces, a char **char1** (a char), and an
      integer **int2** (an integer): **struct in struct**, a struct in a
      struct.
- On the next lines, a struct **sized struct**.
    - On the first line, an integer: **size**, the size.
    - On the next line, a string of size **size** or less: **string in
      struct**, the string.
- On the next line, a string of size **6** or less: **finish**, a string to
  finish.

### Output

Wow, lots of empty lines!

### Constraints

- $0 \le N$
- $0 \le size$
