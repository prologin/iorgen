#include <stdio.h>
#include <stdlib.h>

/// \param n the first list's size
/// \param list_int a list containing ints
/// \param size an other size
/// \param list_char a list of char
/// \param list_string4 a list of strings of size 4
/// \param matrix a matrix of int
void lists(int n, int* list_int, int size, char* list_char, char** list_string4, int** matrix) {
    /* TODO Aren't these lists beautifull? */
}

int main() {
    int n; ///< the first list's size
    scanf("%d", &n);
    int* list_int = calloc(n, sizeof(int)); ///< a list containing ints
    for (int i = 0; i < n; ++i)
        scanf("%d", &list_int[i]);
    int size; ///< an other size
    scanf("%d", &size);
    getchar(); // \n
    char* list_char = calloc(size + 1, sizeof(char)); ///< a list of char
    fgets(list_char, size + 1, stdin);
    getchar(); // \n
    char** list_string4 = calloc(size, sizeof(char*)); ///< a list of strings of size 4
    for (int i = 0; i < size; ++i) {
        list_string4[i] = calloc(4 + 1, sizeof(char));
        fgets(list_string4[i], 4 + 1, stdin);
        getchar(); // \n
    }
    int** matrix = calloc(size, sizeof(int*)); ///< a matrix of int
    for (int i = 0; i < size; ++i) {
        matrix[i] = calloc(size, sizeof(int*));
        for (int j = 0; j < size; ++j)
            scanf("%d", &matrix[i][j]);
    }
    lists(n, list_int, size, list_char, list_string4, matrix);

    return 0;
}
