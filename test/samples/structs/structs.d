module structs;

import std.algorithm.iteration : map;
import std.array : array, split;
import std.conv : to;
import std.stdio : stdin;

/// A simple struct
struct Struct1
{
    int foo; /// a field
    int bar; /// a field
}

/// Represents a position
struct Position
{
    int x; /// X
    int y; /// Y
    int z; /// Z
}

/// A point's name and position
struct Point
{
    char name; /// the point's name (single character)
    string description; /// the point's description
    Position pos; /// the point's position
}

/// a struct of chars
struct Chars
{
    char firstChar; /// a first char
    char secondChar; /// a second char
    char thirdChar; /// a third char
}

/// contains a big list inside
struct WithList
{
    int int_; /// int
    int[][][] bigList; /// list nested 3 times!
}

/**
Params:
    struct_ = a struct 1 instance
    n = a number
    structList = a list a struct 1
    triangle = a triangle
    structChars = a struct of chars
    bigListStruct = the big list struct
*/
void structs(Struct1 struct_, int n, Struct1[] structList, Point[] triangle, Chars structChars, WithList bigListStruct)
{
    // TODO Look at them structs.
}

void main()
{
    Struct1 struct_;
    stdin.readf("%d %d\n", &struct_.foo, &struct_.bar);
    int n;
    stdin.readf("%d\n", &n);
    Struct1[] structList;
    structList.length = n;
    for (size_t i = 0; i < structList.length; i++)
    {
        stdin.readf("%d %d\n", &structList[i].foo, &structList[i].bar);
    }
    Point[] triangle;
    triangle.length = 3;
    for (size_t i = 0; i < triangle.length; i++)
    {
        stdin.readf("%c\n", &triangle[i].name);
        stdin.readf("%s\n", &triangle[i].description);
        stdin.readf("%d %d %d\n", &triangle[i].pos.x, &triangle[i].pos.y, &triangle[i].pos.z);
    }
    Chars structChars;
    stdin.readf("%c %c %c\n", &structChars.firstChar, &structChars.secondChar, &structChars.thirdChar);
    WithList bigListStruct;
    stdin.readf("%d\n", &bigListStruct.int_);
    bigListStruct.bigList.length = 2;
    for (size_t i = 0; i < bigListStruct.bigList.length; i++)
    {
        bigListStruct.bigList[i].length = 2;
        for (size_t j = 0; j < bigListStruct.bigList[i].length; j++)
        {
            bigListStruct.bigList[i][j] = stdin.readln.split.map!(to!int).array;
        }
    }

    structs(struct_, n, structList, triangle, structChars, bigListStruct);
}
