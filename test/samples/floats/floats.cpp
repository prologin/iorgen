#include <iostream>
#include <istream>
#include <string>
#include <vector>

/// Represents coordinates
struct Coordinates {
    double x; ///< X
    double y; ///< Y
    double z; ///< Z
};

/// Mix of fields that go on one line
struct InlinedMix {
    int integer; ///< an integer
    char char_; ///< a char
    double float_; ///< a float
};

/// a struct of chars
struct MultilineMix {
    int integer_2; ///< an other integer
    std::string string; ///< a string of size 5
    double float_2; ///< an other float
};

/// \param f a float
/// \param g a float, greater than f
/// \param point some coordinates
/// \param n a number
/// \param float_list a list of floats
/// \param other_list a list of floats
/// \param inlined some inlined structs
/// \param multiline a multiline struct
void floats(double f, double g, const Coordinates& point, int n, const std::vector<double>& float_list, const std::vector<double>& other_list, const std::vector<InlinedMix>& inlined, const MultilineMix& multiline) {
    /* TODO Parsing is often easy, reprint mode is harder */
}

int main() {
    double f; ///< a float
    std::cin >> f;
    double g; ///< a float, greater than f
    std::cin >> g;
    Coordinates point; ///< some coordinates
    std::cin >> point.x >> point.y >> point.z;
    int n; ///< a number
    std::cin >> n;
    std::vector<double> float_list(n); ///< a list of floats
    for (double& i : float_list)
        std::cin >> i;
    std::vector<double> other_list(9); ///< a list of floats
    for (double& i : other_list)
        std::cin >> i;
    std::vector<InlinedMix> inlined(3); ///< some inlined structs
    for (InlinedMix& i : inlined) {
        std::cin >> i.integer >> i.char_ >> i.float_;
    }
    MultilineMix multiline; ///< a multiline struct
    std::cin >> multiline.integer_2;
    std::getline(std::cin >> std::ws, multiline.string);
    std::cin >> multiline.float_2;
    floats(f, g, point, n, float_list, other_list, inlined, multiline);
}
