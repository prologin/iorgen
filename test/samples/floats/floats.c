#include <stdio.h>
#include <stdlib.h>

/// Represents coordinates
struct coordinates {
    double x; ///< X
    double y; ///< Y
    double z; ///< Z
};

/// Mix of fields that go on one line
struct inlined_mix {
    int integer; ///< an integer
    char char_; ///< a char
    double float_; ///< a float
};

/// a struct of chars
struct multiline_mix {
    int integer_2; ///< an other integer
    char* string; ///< a string of size 5
    double float_2; ///< an other float
};

/// \param f a float
/// \param g a float, greater than f
/// \param point some coordinates
/// \param n a number
/// \param float_list a list of floats
/// \param other_list a list of floats
/// \param inlined some inlined structs
/// \param multiline a multiline struct
void floats(double f, double g, struct coordinates point, int n, double* float_list, double* other_list, struct inlined_mix* inlined, struct multiline_mix multiline) {
    /* TODO Parsing is often easy, reprint mode is harder */
}

int main() {
    double f;
    scanf("%lf", &f);
    double g;
    scanf("%lf", &g);
    struct coordinates point;
    scanf("%lf %lf %lf", &point.x, &point.y, &point.z);
    int n;
    scanf("%d", &n);
    double* float_list = (double*)malloc(n * sizeof(double));
    for (int i = 0; i < n; ++i)
        scanf("%lf", &float_list[i]);
    double* other_list = (double*)malloc(9 * sizeof(double));
    for (int i = 0; i < 9; ++i)
        scanf("%lf", &other_list[i]);
    struct inlined_mix* inlined = (struct inlined_mix*)malloc(3 * sizeof(struct inlined_mix));
    for (int i = 0; i < 3; ++i) {
        scanf("%d %c %lf", &inlined[i].integer, &inlined[i].char_, &inlined[i].float_);
    }
    struct multiline_mix multiline;
    scanf("%d", &multiline.integer_2);
    getchar(); // \n
    multiline.string = (char*)malloc((5 + 1) * sizeof(char));
    scanf("%[^\n]", multiline.string);
    scanf("%lf", &multiline.float_2);
    floats(f, g, point, n, float_list, other_list, inlined, multiline);

    return 0;
}
