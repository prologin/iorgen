## Énoncé

Very rarely, some lines may be empty, because we read a list or a string that
has an empty size. Here we check that those cases work well.

### Entrée

L’entrée contiendra :

- Sur la première ligne, une liste de **0** entiers séparés par des espaces :
  **empty list**, an empty list.
- Sur la ligne suivante, une chaine de **3** caractères ou moins : **buffer
  string**, here to check correct parsing of empty line above.
- Sur la ligne suivante, un entier : **N**, an integer, will be 0 in the sample
  input.
- Sur la ligne suivante, une liste de **N** entiers séparés par des espaces :
  **empty in sample**, an empty list (only in the sample).
- Sur la ligne suivante, une chaine de **0** caractères ou moins : **empty
  string**, an empty string.
- Sur la ligne suivante, une chaine de **4** caractères ou moins : **main**, an
  other buffer string.
- Sur la ligne suivante, une liste de **0** caractères juxtaposés : **empty
  char list**, an empty char list.
- Sur la ligne suivante, une liste de **5** caractères juxtaposés : **non empty
  char list**, an char list, non empty.
- Sur les lignes suivantes, une struct **a**.
    - Sur la première ligne, une liste de **N** entiers séparés par des
      espaces : **list in struct**, a list in a struct.
    - Sur la ligne suivante, séparés par des espaces, un caractère **char1** (a
      char), et un entier **int2** (an integer) : **struct in struct**, a
      struct in a struct.
- Sur les lignes suivantes, une struct **sized struct**.
    - Sur la première ligne, un entier : **size**, the size.
    - Sur la ligne suivante, une chaine de **size** caractères ou moins :
      **string in struct**, the string.
- Sur la ligne suivante, une chaine de **6** caractères ou moins : **finish**,
  a string to finish.

### Sortie

Wow, lots of empty lines!

### Contraintes

- $0 \le N$
- $0 \le size$
