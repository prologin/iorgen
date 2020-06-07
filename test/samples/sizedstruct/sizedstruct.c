#include <stdio.h>
#include <stdlib.h>

/// contains a list
struct list {
    int size1; ///< the list's size
    int* int_list; ///< the integer list
};

/// contains a string
struct string {
    int size2; ///< the list's size
    char* string_list; ///< the string list
};

/// contains a matrix
struct matrix {
    int size3; ///< the list's size
    int** list_list; ///< the list list
};

/// this is not a 'sized struct', but a regular one!
struct not_a_sized_struct {
    int size4; ///< not the list's size
    int* int_list_n; ///< the integer list
};

/// \param n the size of the lists
/// \param lists a list of list of different sizes
/// \param strings a list of strings of different sizes
/// \param matrices a list of matrices of different sizes
/// \param same a list of list of same sizes
void sized_struct(int n, struct list* lists, struct string* strings, struct matrix* matrices, struct not_a_sized_struct* same) {
    /* TODO The is a special case. */
}

int main() {
    int n;
    scanf("%d", &n);
    getchar(); // \n
    struct list* lists = (struct list*)malloc(n * sizeof(struct list));
    for (int i = 0; i < n; ++i) {
        scanf("%d", &lists[i].size1);
        getchar(); // \n
        lists[i].int_list = (int*)malloc(lists[i].size1 * sizeof(int));
        for (int j = 0; j < lists[i].size1; ++j)
            scanf("%d", &lists[i].int_list[j]);
        getchar(); // \n
    }
    struct string* strings = (struct string*)malloc(n * sizeof(struct string));
    for (int i = 0; i < n; ++i) {
        scanf("%d", &strings[i].size2);
        getchar(); // \n
        strings[i].string_list = (char*)malloc((strings[i].size2 + 1) * sizeof(char));
        fgets(strings[i].string_list, strings[i].size2 + 1, stdin);
    }
    struct matrix* matrices = (struct matrix*)malloc(2 * sizeof(struct matrix));
    for (int i = 0; i < 2; ++i) {
        scanf("%d", &matrices[i].size3);
        matrices[i].list_list = (int**)malloc(matrices[i].size3 * sizeof(int*));
        for (int j = 0; j < matrices[i].size3; ++j) {
            matrices[i].list_list[j] = (int*)malloc(2 * sizeof(int));
            for (int k = 0; k < 2; ++k)
                scanf("%d", &matrices[i].list_list[j][k]);
        }
    }
    struct not_a_sized_struct* same = (struct not_a_sized_struct*)malloc(n * sizeof(struct not_a_sized_struct));
    for (int i = 0; i < n; ++i) {
        scanf("%d", &same[i].size4);
        same[i].int_list_n = (int*)malloc(n * sizeof(int));
        for (int j = 0; j < n; ++j)
            scanf("%d", &same[i].int_list_n[j]);
    }
    sized_struct(n, lists, strings, matrices, same);

    return 0;
}
