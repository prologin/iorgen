#include <iostream>
#include <istream>
#include <string>
#include <vector>

/// contains a list
struct List {
    int size1; ///< the list's size
    std::vector<int> int_list; ///< the integer list
};

/// contains a string
struct String {
    int size2; ///< the list's size
    std::string string_list; ///< the string list
};

/// contains a matrix
struct Matrix {
    int size3; ///< the list's size
    std::vector<std::vector<int>> list_list; ///< the list list
};

/// this is not a 'sized struct', but a regular one!
struct NotASizedStruct {
    int size4; ///< not the list's size
    std::vector<int> int_list_n; ///< the integer list
};

/// \param n the size of the lists
/// \param lists a list of list of different sizes
/// \param strings a list of strings of different sizes
/// \param matrices a list of matrices of different sizes
/// \param same a list of list of same sizes
void sized_struct(int n, const std::vector<List>& lists, const std::vector<String>& strings, const std::vector<Matrix>& matrices, const std::vector<NotASizedStruct>& same) {
    /* TODO The is a special case. */
}

int main() {
    int n; ///< the size of the lists
    std::cin >> n;
    std::vector<List> lists(n); ///< a list of list of different sizes
    for (List& i : lists) {
        std::cin >> i.size1;
        i.int_list.resize(i.size1);
        for (int& j : i.int_list)
            std::cin >> j;
    }
    std::vector<String> strings(n); ///< a list of strings of different sizes
    for (String& i : strings) {
        std::cin >> i.size2;
        if (i.size2 > 0)
            std::getline(std::cin >> std::ws, i.string_list);
    }
    std::vector<Matrix> matrices(2); ///< a list of matrices of different sizes
    for (Matrix& i : matrices) {
        std::cin >> i.size3;
        i.list_list.resize(i.size3);
        for (std::vector<int>& j : i.list_list) {
            j.resize(2);
            for (int& k : j)
                std::cin >> k;
        }
    }
    std::vector<NotASizedStruct> same(n); ///< a list of list of same sizes
    for (NotASizedStruct& i : same) {
        std::cin >> i.size4;
        i.int_list_n.resize(n);
        for (int& j : i.int_list_n)
            std::cin >> j;
    }
    sized_struct(n, lists, strings, matrices, same);
}
