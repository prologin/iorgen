#include <iostream>
#include <istream>
#include <string>
#include <vector>

/// a char struct
struct StructWithAChar {
    char char1; ///< a char
    int int2; ///< an integer
};

/// a struct
struct A {
    std::vector<int> list_in_struct; ///< a list in a struct
    StructWithAChar struct_in_struct; ///< a struct in a struct
};

/// a sized struct
struct SizedStruct {
    int size; ///< the size
    std::string string_in_struct; ///< the string
};

/// \param empty_list an empty list
/// \param buffer_string here to check correct parsing of empty line above
/// \param n an integer, will be 0 in the sample input
/// \param empty_in_sample an empty list (only in the sample)
/// \param empty_string an empty string
/// \param main an other buffer string
/// \param empty_char_list an empty char list
/// \param non_empty_char_list an char list, non empty
/// \param struct_with_empty_line a struct containing an empty line, then a struct
/// \param a_sized_struct a sized struct containing an empty line
/// \param finish a string to finish
void empty_lines(const std::vector<int>& empty_list, const std::string& buffer_string, int n, const std::vector<int>& empty_in_sample, const std::string& empty_string, const std::string& main, const std::vector<char>& empty_char_list, const std::vector<char>& non_empty_char_list, const A& struct_with_empty_line, const SizedStruct& a_sized_struct, const std::string& finish) {
    /* TODO Wow, lots of empty lines! */
}

int main() {
    std::vector<int> empty_list(0); ///< an empty list
    for (int& i : empty_list)
        std::cin >> i;
    std::string buffer_string; ///< here to check correct parsing of empty line above
    std::getline(std::cin >> std::ws, buffer_string);
    int n; ///< an integer, will be 0 in the sample input
    std::cin >> n;
    std::vector<int> empty_in_sample(n); ///< an empty list (only in the sample)
    for (int& i : empty_in_sample)
        std::cin >> i;
    std::string empty_string; ///< an empty string
    if (0 > 0)
        std::getline(std::cin >> std::ws, empty_string);
    std::string main; ///< an other buffer string
    std::getline(std::cin >> std::ws, main);
    std::vector<char> empty_char_list(0); ///< an empty char list
    for (char& i : empty_char_list)
        std::cin >> i;
    std::vector<char> non_empty_char_list(5); ///< an char list, non empty
    for (char& i : non_empty_char_list)
        std::cin >> i;
    A struct_with_empty_line; ///< a struct containing an empty line, then a struct
    for (int& i : struct_with_empty_line.list_in_struct)
        std::cin >> i;
    std::cin >> struct_with_empty_line.struct_in_struct.char1 >> struct_with_empty_line.struct_in_struct.int2;
    SizedStruct a_sized_struct; ///< a sized struct containing an empty line
    std::cin >> a_sized_struct.size;
    if (a_sized_struct.size > 0)
        std::getline(std::cin >> std::ws, a_sized_struct.string_in_struct);
    std::string finish; ///< a string to finish
    std::getline(std::cin >> std::ws, finish);
    empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish);
}
