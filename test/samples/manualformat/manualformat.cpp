#include <iostream>
#include <vector>

/// \param a a first number
/// \param b a second number
/// \param c a third number
/// \param n This one on a new line
/// \param one_per_line an integer list, one per line
void manual_format(int a, int b, int c, int n, const std::vector<int>& one_per_line) {
    /* TODO From the function perspective, this is just 4 integers */
}

int main() {
    int a; ///< a first number
    std::cin >> a;
    int b; ///< a second number
    std::cin >> b;
    int c; ///< a third number
    std::cin >> c;
    int n; ///< This one on a new line
    std::cin >> n;
    std::vector<int> one_per_line(3); ///< an integer list, one per line
    for (int& i : one_per_line)
        std::cin >> i;
    manual_format(a, b, c, n, one_per_line);
}
