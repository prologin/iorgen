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
    public Position pos; //!< the point's position
}

class Program
{
    /// \param @struct a struct 1 instance
    /// \param n a number
    /// \param structList a list a struct 1
    /// \param triangle a triangle
    static void Structs(Struct1 @struct, int n, Struct1[] structList, Point[] triangle)
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
            string[] words1 = Console.ReadLine().Split(' ');
            triangle[i].pos = new Position {x = int.Parse(words1[0]), y = int.Parse(words1[1]), z = int.Parse(words1[2])};
        }

        Structs(@struct, n, structList, triangle);
    }
}
