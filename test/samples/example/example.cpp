#include <iostream>
#include <vector>

/// A struct for the example
struct AStruct {
    int integer; ///< an integer
    char character; ///< a char
};

/// \param n a number, used as a size
/// \param list a list of structs
void example(int n, const std::vector<AStruct>& list) {
    /* TODO In a real life scenario, you will describe here what you want the
    end user to do with this generated code */
}

int main() {
    int n; ///< a number, used as a size
    std::cin >> n;
    std::vector<AStruct> list(n); ///< a list of structs
    for (AStruct& i : list) {
        std::cin >> i.integer >> i.character;
    }
    example(n, list);
}
