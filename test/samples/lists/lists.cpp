#include <iostream>
#include <istream>
#include <string>
#include <vector>

/// \param n the first list's size
/// \param list_int a list containing ints
/// \param size an other size
/// \param list_char a list of char
/// \param string a string
/// \param list_string4 a list of strings of size 4
/// \param list_list_string2 a list of list of strings of size 2 of size 2 of size 2
/// \param matrix a matrix of int
void lists(int n, const std::vector<int>& list_int, int size, const std::vector<char>& list_char, const std::string& string, const std::vector<std::string>& list_string4, const std::vector<std::vector<std::string>>& list_list_string2, const std::vector<std::vector<int>>& matrix) {
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
    std::string string; ///< a string
    std::getline(std::cin >> std::ws, string);
    std::vector<std::string> list_string4(size); ///< a list of strings of size 4
    for (std::string& i : list_string4) {
        std::getline(std::cin >> std::ws, i);
    }
    std::vector<std::vector<std::string>> list_list_string2(2); ///< a list of list of strings of size 2 of size 2 of size 2
    for (std::vector<std::string>& i : list_list_string2) {
        i.resize(2);
        for (std::string& j : i) {
            std::getline(std::cin >> std::ws, j);
        }
    }
    std::vector<std::vector<int>> matrix(size); ///< a matrix of int
    for (std::vector<int>& i : matrix) {
        i.resize(size);
        for (int& j : i)
            std::cin >> j;
    }
    lists(n, list_int, size, list_char, string, list_string4, list_list_string2, matrix);
}
