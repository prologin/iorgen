#include <stdio.h>
#include <stdlib.h>

/// a char struct
struct struct_with_a_char {
    char char1; ///< a char
    int int2; ///< an integer
};

/// a struct
struct a {
    int* list_in_struct; ///< a list in a struct
    struct struct_with_a_char struct_in_struct; ///< a struct in a struct
};

/// a sized struct
struct sized_struct {
    int size; ///< the size
    char* string_in_struct; ///< the string
};

/// \param empty_list an empty list
/// \param buffer_string here to check correct parsing of empty line above
/// \param n an integer, will be 0 in the sample input
/// \param empty_in_sample an empty list (only in the sample)
/// \param empty_string an empty string
/// \param main_ an other buffer string
/// \param empty_char_list an empty char list
/// \param non_empty_char_list an char list, non empty
/// \param struct_with_empty_line a struct containing an empty line, then a struct
/// \param a_sized_struct a sized struct containing an empty line
/// \param finish a string to finish
void empty_lines(int* empty_list, char* buffer_string, int n, int* empty_in_sample, char* empty_string, char* main_, char* empty_char_list, char* non_empty_char_list, struct a struct_with_empty_line, struct sized_struct a_sized_struct, char* finish) {
    /* TODO Wow, lots of empty lines! */
}

int main() {
    int* empty_list = (int*)malloc(0 * sizeof(int));
    for (int i = 0; i < 0; ++i)
        scanf("%d", &empty_list[i]);
    getchar(); // \n
    char* buffer_string = (char*)malloc((3 + 1) * sizeof(char));
    fgets(buffer_string, 3 + 1, stdin);
    int n;
    scanf("%d", &n);
    getchar(); // \n
    int* empty_in_sample = (int*)malloc(n * sizeof(int));
    for (int i = 0; i < n; ++i)
        scanf("%d", &empty_in_sample[i]);
    getchar(); // \n
    char* empty_string = (char*)malloc((0 + 1) * sizeof(char));
    fgets(empty_string, 0 + 1, stdin);
    getchar(); // \n
    char* main_ = (char*)malloc((4 + 1) * sizeof(char));
    fgets(main_, 4 + 1, stdin);
    getchar(); // \n
    char* empty_char_list = (char*)malloc((0 + 1) * sizeof(char));
    fgets(empty_char_list, 0 + 1, stdin);
    getchar(); // \n
    char* non_empty_char_list = (char*)malloc((5 + 1) * sizeof(char));
    fgets(non_empty_char_list, 5 + 1, stdin);
    getchar(); // \n
    struct a struct_with_empty_line;
    struct_with_empty_line.list_in_struct = (int*)malloc(n * sizeof(int));
    for (int i = 0; i < n; ++i)
        scanf("%d", &struct_with_empty_line.list_in_struct[i]);
    getchar(); // \n
    scanf("%c %d", &struct_with_empty_line.struct_in_struct.char1, &struct_with_empty_line.struct_in_struct.int2);
    struct sized_struct a_sized_struct;
    scanf("%d", &a_sized_struct.size);
    getchar(); // \n
    a_sized_struct.string_in_struct = (char*)malloc((a_sized_struct.size + 1) * sizeof(char));
    fgets(a_sized_struct.string_in_struct, a_sized_struct.size + 1, stdin);
    getchar(); // \n
    char* finish = (char*)malloc((6 + 1) * sizeof(char));
    fgets(finish, 6 + 1, stdin);
    empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main_, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish);

    return 0;
}
