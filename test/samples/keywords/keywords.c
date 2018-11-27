#include <stdio.h>
#include <stdlib.h>

/// may conflict in c#
struct console {
    int a; ///< the first letter of the alphabet
    int static_; ///< an integer
};

/// may conflict in c#
struct system {
    int return_; ///< not the end of the function
    int* void_; ///< not nothing
};

/// not the main function
struct main {
    struct system int_; ///< not an integer
    int if_true; ///< should not cause conflict
};

/// \param if_ not a condition
/// \param class not a class
/// \param i just a string
/// \param in not in
/// \param for_ not a loop
/// \param words contains lots of things
void keywords(int if_, char class, char* i, struct console in, int* for_, struct main* words) {
    /* TODO If this compiles, it is already a good step! */
}

int main() {
    int if_; ///< not a condition
    scanf("%d", &if_);
    getchar(); // \n
    char class = getchar(); ///< not a class
    getchar(); // \n
    char* i = calloc(8 + 1, sizeof(char));; ///< just a string
    fgets(i, 8 + 1, stdin);
    getchar(); // \n
    struct console in; ///< not in
    scanf("%d %d", &in.a, &in.static_);
    getchar(); // \n
    int* for_ = calloc(if_, sizeof(int)); ///< not a loop
    for (int j = 0; j < if_; ++j)
        scanf("%d", &for_[j]);
    getchar(); // \n
    struct main* words = calloc(2, sizeof(struct main)); ///< contains lots of things
    for (int j = 0; j < 2; ++j) {
        scanf("%d", &words[j].int_.return_);
        getchar(); // \n
        words[j].int_.void_ = calloc(3, sizeof(int*));
        for (int k = 0; k < 3; ++k)
            scanf("%d", &words[j].int_.void_[k]);
        getchar(); // \n
        scanf("%d", &words[j].if_true);
        getchar(); // \n
    }
    keywords(if_, class, i, in, for_, words);

    return 0;
}
