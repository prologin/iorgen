#include <iostream>
#include <istream>
#include <string>
#include <vector>

/// \param n the first list's size
/// \param list_int a list containing ints
/// \param size an other size
/// \param list_char a list of char
/// \param list_string4 a list of strings of size 4
/// \param matrix a matrix of int
void lists(int n, const std::vector<int>& list_int, int size, const std::vector<char>& list_char, const std::vector<std::string>& list_string4, const std::vector<std::vector<int>>& matrix) {
    /* TODO Aren't these lists beautifull? */
}

int main() {
    int n; ///< the first list's size
    std::cin >> n;
    std::vector<int> list_int(n); ///< a list containing ints
    for (int& i : list_int)
        std::cin >> i;
    int size; ///< an other size
    std::cin >> size;
    std::vector<char> list_char(size); ///< a list of char
    for (char& i : list_char)
        std::cin >> i;
    std::vector<std::string> list_string4(size); ///< a list of strings of size 4
    for (std::string& i : list_string4) {
        std::getline(std::cin >> std::ws, i);
    }
    std::vector<std::vector<int>> matrix(size); ///< a matrix of int
    for (std::vector<int>& i : matrix) {
        i.resize(size);
        for (int& j : i)
            std::cin >> j;
    }
    lists(n, list_int, size, list_char, list_string4, matrix);
}
