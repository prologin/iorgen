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
    scanf("%d\n", &n);
    int* list_int = calloc(n, sizeof(int)); ///< a list containing ints
    for (int list_int_index = 0; list_int_index < n; ++list_int_index)
        scanf("%d ", &list_int[list_int_index]);
    int size; ///< an other size
    scanf("%d\n", &size);
    char* list_char = calloc(size + 1, sizeof(char)); ///< a list of char
    fgets(list_char, size + 1, stdin);
    getchar(); // \n
    char** list_string4 = calloc(size, sizeof(char*)); ///< a list of strings of size 4
    for (int list_string4_index = 0; list_string4_index < size; ++list_string4_index) {
        list_string4[list_string4_index] = calloc(4 + 1, sizeof(char));
        fgets(list_string4[list_string4_index], 4 + 1, stdin);
        getchar(); // \n
    }
    int** matrix = calloc(size, sizeof(int*)); ///< a matrix of int
    for (int matrix_index = 0; matrix_index < size; ++matrix_index) {
        matrix[matrix_index] = calloc(size, sizeof(int*));
        for (int matrix_index_index = 0; matrix_index_index < size; ++matrix_index_index)
            scanf("%d ", &matrix[matrix_index][matrix_index_index]);
    }
    lists(n, list_int, size, list_char, list_string4, matrix);

    return 0;
}
