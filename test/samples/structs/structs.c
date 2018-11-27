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

/// \param struct_ a struct 1 instance
/// \param n a number
/// \param struct_list a list a struct 1
/// \param triangle a triangle
void structs(struct struct_1 struct_, int n, struct struct_1* struct_list, struct point* triangle) {
    /* TODO Look at them structs. */
}

int main() {
    struct struct_1 struct_; ///< a struct 1 instance
    scanf("%d %d", &struct_.foo, &struct_.bar);
    getchar(); // \n
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
        getchar(); // \n
        scanf("%d %d %d", &triangle[i].pos.x, &triangle[i].pos.y, &triangle[i].pos.z);
        getchar(); // \n
    }
    structs(struct_, n, struct_list, triangle);

    return 0;
}
