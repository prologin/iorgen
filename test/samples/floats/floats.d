module floats;

import std.algorithm.iteration : map;
import std.array : array, split;
import std.conv : to;
import std.stdio : stdin;

/// Represents coordinates
struct Coordinates
{
    double x; /// X
    double y; /// Y
    double z; /// Z
}

/// Mix of fields that go on one line
struct InlinedMix
{
    int integer; /// an integer
    char char_; /// a char
    double float_; /// a float
}

/// a struct of chars
struct MultilineMix
{
    int integer2; /// an other integer
    string string_; /// a string of size 5
    double float2; /// an other float
}

/**
Params:
    f = a float
    g = a float, greater than f
    point = some coordinates
    n = a number
    floatList = a list of floats
    otherList = a list of floats
    inlined = some inlined structs
    multiline = a multiline struct
*/
void floats(double f, double g, Coordinates point, int n, double[] floatList, double[] otherList, InlinedMix[] inlined, MultilineMix multiline)
{
    // TODO Parsing is often easy, reprint mode is harder
}

void main()
{
    double f;
    stdin.readf("%g\n", &f);
    double g;
    stdin.readf("%g\n", &g);
    Coordinates point;
    stdin.readf("%g %g %g\n", &point.x, &point.y, &point.z);
    int n;
    stdin.readf("%d\n", &n);
    double[] floatList;
    floatList = stdin.readln.split.map!(to!double).array;
    double[] otherList;
    otherList = stdin.readln.split.map!(to!double).array;
    InlinedMix[] inlined;
    inlined.length = 3;
    for (size_t i = 0; i < inlined.length; i++)
    {
        stdin.readf("%d %c %g\n", &inlined[i].integer, &inlined[i].char_, &inlined[i].float_);
    }
    MultilineMix multiline;
    stdin.readf("%d\n", &multiline.integer2);
    stdin.readf("%s\n", &multiline.string_);
    stdin.readf("%g\n", &multiline.float2);

    floats(f, g, point, n, floatList, otherList, inlined, multiline);
}
