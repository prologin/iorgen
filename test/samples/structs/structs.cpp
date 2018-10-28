#include <iostream>
#include <vector>

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
    position pos; ///< the point's position
};

/// \param struct_ a struct 1 instance
/// \param n a number
/// \param struct_list a list a struct 1
/// \param triangle a triangle
void structs(struct_1 struct_, int n, const std::vector<struct_1>& struct_list, const std::vector<point>& triangle) {
    /* TODO Look at them structs. */
}

int main() {
    struct_1 struct_; ///< a struct 1 instance
    std::cin >> struct_.foo >> struct_.bar;
    int n; ///< a number
    std::cin >> n;
    std::vector<struct_1> struct_list(n); ///< a list a struct 1
    for (struct_1& struct_list_elem : struct_list) {
        std::cin >> struct_list_elem.foo >> struct_list_elem.bar;
    }
    std::vector<point> triangle(3); ///< a triangle
    for (point& triangle_elem : triangle) {
        std::cin >> triangle_elem.name;
        std::cin >> triangle_elem.pos.x >> triangle_elem.pos.y >> triangle_elem.pos.z;
    }
    structs(struct_, n, struct_list, triangle);
}
