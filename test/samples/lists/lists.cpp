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
    for (int& list_int_elem : list_int)
        std::cin >> list_int_elem;
    int size; ///< an other size
    std::cin >> size;
    std::vector<char> list_char(size); ///< a list of char
    for (char& list_char_elem : list_char)
        std::cin >> list_char_elem;
    std::vector<std::string> list_string4(size); ///< a list of strings of size 4
    for (std::string& list_string4_elem : list_string4) {
        std::getline(std::cin >> std::ws, list_string4_elem);
    }
    std::vector<std::vector<int>> matrix(size); ///< a matrix of int
    for (std::vector<int>& matrix_elem : matrix) {
        matrix_elem.resize(size);
        for (int& matrix_elem_elem : matrix_elem)
            std::cin >> matrix_elem_elem;
    }
    lists(n, list_int, size, list_char, list_string4, matrix);
}
