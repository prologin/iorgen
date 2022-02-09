#include <stdio.h>
#include <stdlib.h>

/// \param a a first number
/// \param b a second number
/// \param c a third number
/// \param n This one on a new line
/// \param one_per_line an integer list, one per line
void manual_format(int a, int b, int c, int n, int* one_per_line) {
    /* TODO From the function perspective, this is just 4 integers */
}

int main() {
    int a;
    scanf("%d", &a);
    int b;
    scanf("%d", &b);
    int c;
    scanf("%d", &c);
    int n;
    scanf("%d", &n);
    int* one_per_line = (int*)malloc(3 * sizeof(int));
    for (int i = 0; i < 3; ++i)
        scanf("%d", &one_per_line[i]);
    manual_format(a, b, c, n, one_per_line);

    return 0;
}
