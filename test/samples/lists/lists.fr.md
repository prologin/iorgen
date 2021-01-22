## Énoncé

This reads several kinds of list.

### Entrée

L’entrée contiendra :

- Sur la première ligne, un entier : **N**, the first list's size.
- Sur la ligne suivante, une liste de **N** entiers séparés par des espaces :
  **list int**, a list containing ints.
- Sur la ligne suivante, un entier : **size**, an other size.
- Sur la ligne suivante, une liste de **size** caractères juxtaposés : **list
  char**, a list of char.
- Sur la ligne suivante, une chaine de **20** caractères : **string**, a
  string.
- Sur les lignes suivantes, une liste de **size** éléments : **list string4**,
  a list of strings of size 4.
    - Une ligne par élément de la liste : une chaine de **4** caractères.
- Sur les lignes suivantes, une liste de **2** éléments : **list list
  string2**, a list of list of strings of size 2 of size 2 of size 2.
    - Chaque élément de la liste est sur plusieurs lignes : une liste de **2**
      éléments.
        - Une ligne par élément de la liste : une chaine de **2** caractères.
- Sur les lignes suivantes, une liste de **size** éléments : **matrix**, a
  matrix of int.
    - Une ligne par élément de la liste : une liste de **size** entiers séparés
      par des espaces.

### Sortie

Aren't these lists beautifull?

### Contraintes

- $1 \le N \le 10$
- $1 \le (list int)[ ] \le 2\,000$
- $0 \le size \le 5$
- $(list char)[ ] \in \{f, o\}$
- $0 \le matrix[ ] \le 3\,456$

### Contraintes de performance

- $0 \le matrix[ ] \le 10\,000$
