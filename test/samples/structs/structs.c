#include <stdio.h>
#include <stdlib.h>

/// A simple struct
struct struct_1 {
    int foo; ///< a field
    int bar; ///< a field
};

/// Represents a position
struct position {
    int x; ///< X
    int y; ///< Y
    int z; ///< Z
};

/// A point's name and position
struct point {
    char name; ///< the point's name (single character)
    char* description; ///< the point's description
    struct position pos; ///< the point's position
};

/// a struct of chars
struct chars {
    char first_char; ///< a first char
    char second_char; ///< a second char
    char third_char; ///< a third char
};

/// contains a big list inside
struct with_list {
    int int_; ///< int
    int*** big_list; ///< list nested 3 times!
};

/// \param struct_ a struct 1 instance
/// \param n a number
/// \param struct_list a list a struct 1
/// \param triangle a triangle
/// \param struct_chars a struct of chars
/// \param big_list_struct the big list struct
void structs(struct struct_1 struct_, int n, struct struct_1* struct_list, struct point* triangle, struct chars struct_chars, struct with_list big_list_struct) {
    /* TODO Look at them structs. */
}

int main() {
    struct struct_1 struct_;
    scanf("%d %d", &struct_.foo, &struct_.bar);
    int n;
    scanf("%d", &n);
    getchar(); // \n
    struct struct_1* struct_list = (struct struct_1*)malloc(n * sizeof(struct struct_1));
    for (int i = 0; i < n; ++i) {
        scanf("%d %d", &struct_list[i].foo, &struct_list[i].bar);
        getchar(); // \n
    }
    struct point* triangle = (struct point*)malloc(3 * sizeof(struct point));
    for (int i = 0; i < 3; ++i) {
        triangle[i].name = getchar();
        getchar(); // \n
        triangle[i].description = (char*)malloc((12 + 1) * sizeof(char));
        scanf("%[^\n]", triangle[i].description);
        scanf("%d %d %d", &triangle[i].pos.x, &triangle[i].pos.y, &triangle[i].pos.z);
        getchar(); // \n
    }
    struct chars struct_chars;
    scanf("%c %c %c", &struct_chars.first_char, &struct_chars.second_char, &struct_chars.third_char);
    struct with_list big_list_struct;
    scanf("%d", &big_list_struct.int_);
    big_list_struct.big_list = (int***)malloc(2 * sizeof(int**));
    for (int i = 0; i < 2; ++i) {
        big_list_struct.big_list[i] = (int**)malloc(2 * sizeof(int*));
        for (int j = 0; j < 2; ++j) {
            big_list_struct.big_list[i][j] = (int*)malloc(2 * sizeof(int));
            for (int k = 0; k < 2; ++k)
                scanf("%d", &big_list_struct.big_list[i][j][k]);
        }
    }
    structs(struct_, n, struct_list, triangle, struct_chars, big_list_struct);

    return 0;
}
