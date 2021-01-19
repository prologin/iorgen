## Énoncé

A special kind a struct containing the size of a list, or string.

### Entrée

L’entrée contiendra :

- Sur la première ligne, un entier : **n**, the size of the lists.
- Sur les lignes suivantes, une liste de **n** éléments : **lists**, a list of
  list of different sizes.
    - Chaque élément de la liste est sur plusieurs lignes : une struct
      **list**.
        - Sur la première ligne, un entier : **size1**, the list's size.
        - Sur la ligne suivante, une liste de **size1** entiers séparés par des
          espaces : **int list**, the integer list.
- Sur les lignes suivantes, une liste de **n** éléments : **strings**, a list
  of strings of different sizes.
    - Chaque élément de la liste est sur plusieurs lignes : une struct
      **string**.
        - Sur la première ligne, un entier : **size2**, the list's size.
        - Sur la ligne suivante, une chaine de **size2** caractères : **string
          list**, the string list.
- Sur les lignes suivantes, une liste de **2** éléments : **matrices**, a list
  of matrices of different sizes.
    - Chaque élément de la liste est sur plusieurs lignes : une struct
      **matrix**.
        - Sur la première ligne, un entier : **size3**, the list's size.
        - Sur les lignes suivantes, une liste de **size3** éléments : **list
          list**, the list list.
            - Une ligne par élément de la liste : une liste de **2** entiers
              séparés par des espaces.
- Sur les lignes suivantes, une liste de **n** éléments : **same**, a list of
  list of same sizes.
    - Chaque élément de la liste est sur plusieurs lignes : une struct **not a
      sized struct**.
        - Sur la première ligne, un entier : **size4**, not the list's size.
        - Sur la ligne suivante, une liste de **n** entiers séparés par des
          espaces : **int list n**, the integer list.

### Sortie

The is a special case.

### Contraintes

- $0 \le n$
- $0 \le size1$
- $1 \le size2$
- $0 \le size3$
