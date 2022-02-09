module manualFormat;

import std.stdio : stdin;

/**
Params:
    a = a first number
    b = a second number
    c = a third number
    n = This one on a new line
    onePerLine = an integer list, one per line
*/
void manualFormat(int a, int b, int c, int n, int[] onePerLine)
{
    // TODO From the function perspective, this is just 4 integers
}

void main()
{
    int a, b, c;
    stdin.readf("%d %d %d\n", &a, &b, &c);
    int n;
    stdin.readf("%d\n", &n);
    int[] onePerLine;
    onePerLine.length = 3;
    for (size_t i = 0; i < onePerLine.length; i++)
    {
        stdin.readf("%d\n", &onePerLine[i]);
    }

    manualFormat(a, b, c, n, onePerLine);
}
