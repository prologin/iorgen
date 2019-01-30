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
    Position pos; /// the point's position
}

/// a struct of chars
struct Chars
{
    char firstChar; /// a first char
    char secondChar; /// a second char
    char thirdChar; /// a third char
}

/**
Params:
    struct_ = a struct 1 instance
    n = a number
    structList = a list a struct 1
    triangle = a triangle
    structChars = a struct of chars
*/
void structs(Struct1 struct_, int n, Struct1[] structList, Point[] triangle, Chars structChars)
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
        stdin.readf("%d %d %d\n", &triangle[i].pos.x, &triangle[i].pos.y, &triangle[i].pos.z);
    }
    Chars structChars;
    stdin.readf("%c %c %c\n", &structChars.firstChar, &structChars.secondChar, &structChars.thirdChar);

    structs(struct_, n, structList, triangle, structChars);
}
