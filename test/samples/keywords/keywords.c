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
    int if_;
    scanf("%d", &if_);
    getchar(); // \n
    char class = getchar();
    getchar(); // \n
    char* i = (char*)malloc((8 + 1) * sizeof(char));
    scanf("%[^\n]", i);
    struct console in;
    scanf("%d %d", &in.a, &in.static_);
    int* for_ = (int*)malloc(if_ * sizeof(int));
    for (int j = 0; j < if_; ++j)
        scanf("%d", &for_[j]);
    struct main* words = (struct main*)malloc(2 * sizeof(struct main));
    for (int j = 0; j < 2; ++j) {
        scanf("%d", &words[j].int_.return_);
        words[j].int_.void_ = (int*)malloc(3 * sizeof(int));
        for (int k = 0; k < 3; ++k)
            scanf("%d", &words[j].int_.void_[k]);
        scanf("%d", &words[j].if_true);
    }
    keywords(if_, class, i, in, for_, words);

    return 0;
}
