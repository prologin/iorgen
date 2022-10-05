## Énoncé

This will generate a program reading floats.

### Entrée

L’entrée contiendra :

- Sur la première ligne, un nombre à virgule : **f**, a float.
- Sur la ligne suivante, un nombre à virgule : **g**, a float, greater than f.
- Sur la ligne suivante, séparés par des espaces, un nombre à virgule **x**
  (X), un nombre à virgule **y** (Y), et un nombre à virgule **z** (Z) :
  **point**, some coordinates.
- Sur la ligne suivante, un entier : **n**, a number.
- Sur la ligne suivante, une liste de **n** nombres à virgules séparés par des
  espaces : **float list**, a list of floats.
- Sur la ligne suivante, une liste de **9** nombres à virgules séparés par des
  espaces : **other list**, a list of floats.
- Sur les lignes suivantes, une liste de **3** éléments : **inlined**, some
  inlined structs.
    - Une ligne par élément de la liste : séparés par des espaces, un entier
      **integer** (an integer), un caractère **char** (a char), et un nombre à
      virgule **float** (a float).
- Sur les lignes suivantes, une struct **multiline mix**.
    - Sur la première ligne, un entier : **integer 2**, an other integer.
    - Sur la ligne suivante, une chaine de **5** caractères ou moins :
      **string**, a string of size 5.
    - Sur la ligne suivante, un nombre à virgule : **float 2**, an other float.

### Sortie

Parsing is often easy, reprint mode is harder

### Contraintes

- $-3.7 \le f \le 3\,890\,000.0$
- $f \le g$
- $0 \le n \le 10$
- $x \in \{0.3, 4.0, 8.0\}$
- $0.0 \le y$
- $z \le 12.0$
