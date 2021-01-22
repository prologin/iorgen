#include <stdio.h>
#include <stdlib.h>

/// \param n the first list's size
/// \param list_int a list containing ints
/// \param size an other size
/// \param list_char a list of char
/// \param string a string
/// \param list_string4 a list of strings of size 4
/// \param list_list_string2 a list of list of strings of size 2 of size 2 of size 2
/// \param matrix a matrix of int
void lists(int n, int* list_int, int size, char* list_char, char* string, char** list_string4, char*** list_list_string2, int** matrix) {
    /* TODO Aren't these lists beautifull? */
}

int main() {
    int n;
    scanf("%d", &n);
    int* list_int = (int*)malloc(n * sizeof(int));
    for (int i = 0; i < n; ++i)
        scanf("%d", &list_int[i]);
    int size;
    scanf("%d", &size);
    getchar(); // \n
    char* list_char = (char*)malloc((size + 1) * sizeof(char));
    fgets(list_char, size + 1, stdin);
    getchar(); // \n
    char* string = (char*)malloc((20 + 1) * sizeof(char));
    scanf("%[^\n]", string);
    getchar(); // \n
    char** list_string4 = (char**)malloc(size * sizeof(char*));
    for (int i = 0; i < size; ++i) {
        list_string4[i] = (char*)malloc((4 + 1) * sizeof(char));
        scanf("%[^\n]", list_string4[i]);
        getchar(); // \n
    }
    char*** list_list_string2 = (char***)malloc(2 * sizeof(char**));
    for (int i = 0; i < 2; ++i) {
        list_list_string2[i] = (char**)malloc(2 * sizeof(char*));
        for (int j = 0; j < 2; ++j) {
            list_list_string2[i][j] = (char*)malloc((2 + 1) * sizeof(char));
            scanf("%[^\n]", list_list_string2[i][j]);
            getchar(); // \n
        }
    }
    int** matrix = (int**)malloc(size * sizeof(int*));
    for (int i = 0; i < size; ++i) {
        matrix[i] = (int*)malloc(size * sizeof(int));
        for (int j = 0; j < size; ++j)
            scanf("%d", &matrix[i][j]);
    }
    lists(n, list_int, size, list_char, string, list_string4, list_list_string2, matrix);

    return 0;
}
