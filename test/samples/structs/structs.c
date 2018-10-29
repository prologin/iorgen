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
    scanf("%d %d\n", &struct_.foo, &struct_.bar);
    int n; ///< a number
    scanf("%d\n", &n);
    struct struct_1* struct_list = calloc(n, sizeof(struct struct_1)); ///< a list a struct 1
    for (int struct_list_index = 0; struct_list_index < n; ++struct_list_index) {
        scanf("%d %d\n", &struct_list[struct_list_index].foo, &struct_list[struct_list_index].bar);
    }
    struct point* triangle = calloc(3, sizeof(struct point)); ///< a triangle
    for (int triangle_index = 0; triangle_index < 3; ++triangle_index) {
        triangle[triangle_index].name = getchar();
        getchar(); // \n
        scanf("%d %d %d\n", &triangle[triangle_index].pos.x, &triangle[triangle_index].pos.y, &triangle[triangle_index].pos.z);
    }
    structs(struct_, n, struct_list, triangle);

    return 0;
}
