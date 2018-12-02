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
    struct position pos; ///< the point's position
};

/// a struct of chars
struct chars {
    char first_char; ///< a first char
    char second_char; ///< a second char
    char third_char; ///< a third char
};

/// \param struct_ a struct 1 instance
/// \param n a number
/// \param struct_list a list a struct 1
/// \param triangle a triangle
/// \param struct_chars a struct of chars
void structs(struct struct_1 struct_, int n, struct struct_1* struct_list, struct point* triangle, struct chars struct_chars) {
    /* TODO Look at them structs. */
}

int main() {
    struct struct_1 struct_; ///< a struct 1 instance
    scanf("%d %d", &struct_.foo, &struct_.bar);
    int n; ///< a number
    scanf("%d", &n);
    getchar(); // \n
    struct struct_1* struct_list = calloc(n, sizeof(struct struct_1)); ///< a list a struct 1
    for (int i = 0; i < n; ++i) {
        scanf("%d %d", &struct_list[i].foo, &struct_list[i].bar);
        getchar(); // \n
    }
    struct point* triangle = calloc(3, sizeof(struct point)); ///< a triangle
    for (int i = 0; i < 3; ++i) {
        triangle[i].name = getchar();
        scanf("%d %d %d", &triangle[i].pos.x, &triangle[i].pos.y, &triangle[i].pos.z);
        getchar(); // \n
    }
    struct chars struct_chars; ///< a struct of chars
    scanf("%c %c %c", &struct_chars.first_char, &struct_chars.second_char, &struct_chars.third_char);
    structs(struct_, n, struct_list, triangle, struct_chars);

    return 0;
}
