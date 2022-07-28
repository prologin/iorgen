## Énoncé

This will generate a program reading structs.

### Entrée

L’entrée contiendra :

- Sur la première ligne, séparés par des espaces, un entier **foo** (a field),
  et un entier **bar** (a field) : **struct**, a struct 1 instance.
- Sur la ligne suivante, un entier : **n**, a number.
- Sur les lignes suivantes, une liste de **n** éléments : **struct list**, a
  list a struct 1.
    - Une ligne par élément de la liste : séparés par des espaces, un entier
      **foo** (a field), et un entier **bar** (a field).
- Sur les lignes suivantes, une liste de **3** éléments : **triangle**, a
  triangle.
    - Chaque élément de la liste est sur plusieurs lignes : une struct
      **point**.
        - Sur la première ligne, un caractère : **name**, the point's name
          (single character).
        - Sur la ligne suivante, une chaine de **12** caractères ou moins :
          **description**, the point's description.
        - Sur la ligne suivante, séparés par des espaces, un nombre à virgule
          **x** (X), un nombre à virgule **y** (Y), et un nombre à virgule
          **z** (Z) : **pos**, the point's position.
- Sur la ligne suivante, séparés par des espaces, un caractère **first char**
  (a first char), un caractère **second char** (a second char), et un caractère
  **third char** (a third char) : **struct chars**, a struct of chars.
- Sur les lignes suivantes, une struct **with list**.
    - Sur la première ligne, un entier : **int**, int.
    - Sur les lignes suivantes, une liste de **2** éléments : **big list**,
      list nested 3 times!.
        - Chaque élément de la liste est sur plusieurs lignes : une liste de
          **2** éléments.
            - Une ligne par élément de la liste : une liste de **2** entiers
              séparés par des espaces.

### Sortie

Look at them structs.

### Contraintes

- $0 \le n$
- $foo \in \{1, 3, 8, 28, 43\}$
- $2 \le bar \le 99$
- $name \in \{A, B, O\}$

### Contraintes de performance

- $2 \le n$
