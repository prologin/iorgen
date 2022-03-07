#include <iostream>
#include <istream>
#include <string>
#include <vector>

/// may conflict in c#
struct Console {
    int a; ///< the first letter of the alphabet
    int static_; ///< an integer
};

/// may conflict in c#
struct System {
    int return_; ///< not the end of the function
    std::vector<int> void_; ///< not nothing
};

/// not the main function
struct Main {
    System int_; ///< not an integer
    int if_true; ///< should not cause conflict
};

/// \param if_ not a condition
/// \param class_ not a class
/// \param i just a string
/// \param in not in
/// \param for_ not a loop
/// \param words contains lots of things
/// \param words_1 an integer
void keywords(int if_, char class_, const std::string& i, const Console& in, const std::vector<int>& for_, const std::vector<Main>& words, int words_1) {
    /* TODO If this compiles, it is already a good step! */
}

int main() {
    int if_; ///< not a condition
    std::cin >> if_;
    char class_; ///< not a class
    std::cin >> class_;
    std::string i; ///< just a string
    std::getline(std::cin >> std::ws, i);
    Console in; ///< not in
    std::cin >> in.a >> in.static_;
    std::vector<int> for_(if_); ///< not a loop
    for (int& j : for_)
        std::cin >> j;
    std::vector<Main> words(2); ///< contains lots of things
    for (Main& j : words) {
        std::cin >> j.int_.return_;
        j.int_.void_.resize(3);
        for (int& k : j.int_.void_)
            std::cin >> k;
        std::cin >> j.if_true;
    }
    int words_1; ///< an integer
    std::cin >> words_1;
    keywords(if_, class_, i, in, for_, words, words_1);
}
