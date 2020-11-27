#include <iostream>
#include <istream>
#include <string>
#include <vector>

/// A simple struct
struct Struct1 {
    int foo; ///< a field
    int bar; ///< a field
};

/// Represents a position
struct Position {
    int x; ///< X
    int y; ///< Y
    int z; ///< Z
};

/// A point's name and position
struct Point {
    char name; ///< the point's name (single character)
    std::string description; ///< the point's description
    Position pos; ///< the point's position
};

/// a struct of chars
struct Chars {
    char first_char; ///< a first char
    char second_char; ///< a second char
    char third_char; ///< a third char
};

/// \param struct_ a struct 1 instance
/// \param n a number
/// \param struct_list a list a struct 1
/// \param triangle a triangle
/// \param struct_chars a struct of chars
void structs(const Struct1& struct_, int n, const std::vector<Struct1>& struct_list, const std::vector<Point>& triangle, const Chars& struct_chars) {
    /* TODO Look at them structs. */
}

int main() {
    Struct1 struct_; ///< a struct 1 instance
    std::cin >> struct_.foo >> struct_.bar;
    int n; ///< a number
    std::cin >> n;
    std::vector<Struct1> struct_list(n); ///< a list a struct 1
    for (Struct1& i : struct_list) {
        std::cin >> i.foo >> i.bar;
    }
    std::vector<Point> triangle(3); ///< a triangle
    for (Point& i : triangle) {
        std::cin >> i.name;
        std::getline(std::cin >> std::ws, i.description);
        std::cin >> i.pos.x >> i.pos.y >> i.pos.z;
    }
    Chars struct_chars; ///< a struct of chars
    std::cin >> struct_chars.first_char >> struct_chars.second_char >> struct_chars.third_char;
    structs(struct_, n, struct_list, triangle, struct_chars);
}
