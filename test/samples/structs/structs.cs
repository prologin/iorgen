using System;

/// A simple struct
struct Struct1
{
    public int foo; //!< a field
    public int bar; //!< a field
}

/// Represents a position
struct Position
{
    public int x; //!< X
    public int y; //!< Y
    public int z; //!< Z
}

/// A point's name and position
struct Point
{
    public char name; //!< the point's name (single character)
    public string description; //!< the point's description
    public Position pos; //!< the point's position
}

/// a struct of chars
struct Chars
{
    public char firstChar; //!< a first char
    public char secondChar; //!< a second char
    public char thirdChar; //!< a third char
}

/// contains a big list inside
struct WithList
{
    public int @int; //!< int
    public int[][][] bigList; //!< list nested 3 times!
}

class Program
{
    /// \param @struct a struct 1 instance
    /// \param n a number
    /// \param structList a list a struct 1
    /// \param triangle a triangle
    /// \param structChars a struct of chars
    /// \param bigListStruct the big list struct
    static void Structs(Struct1 @struct, int n, Struct1[] structList, Point[] triangle, Chars structChars, WithList bigListStruct)
    {
        /* TODO Look at them structs. */
    }

    static void Main()
    {
        string[] words = Console.ReadLine().Split(' ');
        Struct1 @struct = new Struct1 {foo = int.Parse(words[0]), bar = int.Parse(words[1])};
        int n = int.Parse(Console.ReadLine());
        Struct1[] structList = new Struct1[n];
        for (int i = 0; i < n; ++i)
        {
            string[] words1 = Console.ReadLine().Split(' ');
            structList[i] = new Struct1 {foo = int.Parse(words1[0]), bar = int.Parse(words1[1])};
        }
        Point[] triangle = new Point[3];
        for (int i = 0; i < 3; ++i)
        {
            triangle[i].name = Console.ReadLine()[0];
            triangle[i].description = Console.ReadLine();
            string[] words1 = Console.ReadLine().Split(' ');
            triangle[i].pos = new Position {x = int.Parse(words1[0]), y = int.Parse(words1[1]), z = int.Parse(words1[2])};
        }
        string[] words2 = Console.ReadLine().Split(' ');
        Chars structChars = new Chars {firstChar = words2[0][0], secondChar = words2[1][0], thirdChar = words2[2][0]};
        WithList bigListStruct;
        bigListStruct.@int = int.Parse(Console.ReadLine());
        bigListStruct.bigList = new int[2][][];
        for (int i = 0; i < 2; ++i)
        {
            bigListStruct.bigList[i] = new int[2][];
            for (int j = 0; j < 2; ++j)
            {
                bigListStruct.bigList[i][j] = Array.ConvertAll(Console.ReadLine().Split(' '), int.Parse);
            }
        }

        Structs(@struct, n, structList, triangle, structChars, bigListStruct);
    }
}
