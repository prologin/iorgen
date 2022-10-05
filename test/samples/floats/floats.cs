using System;
using System.Globalization;

/// Represents coordinates
struct Coordinates
{
    public double x; //!< X
    public double y; //!< Y
    public double z; //!< Z
}

/// Mix of fields that go on one line
struct InlinedMix
{
    public int integer; //!< an integer
    public char @char; //!< a char
    public double @float; //!< a float
}

/// a struct of chars
struct MultilineMix
{
    public int integer2; //!< an other integer
    public string @string; //!< a string of size 5
    public double float2; //!< an other float
}

class Program
{
    /// \param f a float
    /// \param g a float, greater than f
    /// \param point some coordinates
    /// \param n a number
    /// \param floatList a list of floats
    /// \param otherList a list of floats
    /// \param inlined some inlined structs
    /// \param multiline a multiline struct
    static void Floats(double f, double g, Coordinates point, int n, double[] floatList, double[] otherList, InlinedMix[] inlined, MultilineMix multiline)
    {
        /* TODO Parsing is often easy, reprint mode is harder */
    }

    static void Main()
    {
        double f = double.Parse(Console.ReadLine(), CultureInfo.InvariantCulture);
        double g = double.Parse(Console.ReadLine(), CultureInfo.InvariantCulture);
        string[] words = Console.ReadLine().Split(' ');
        Coordinates point = new Coordinates {x = double.Parse(words[0], CultureInfo.InvariantCulture), y = double.Parse(words[1], CultureInfo.InvariantCulture), z = double.Parse(words[2], CultureInfo.InvariantCulture)};
        int n = int.Parse(Console.ReadLine());
        double[] floatList = Array.ConvertAll(Console.ReadLine().Split(new char[] {' '}, StringSplitOptions.RemoveEmptyEntries), x => double.Parse(x, CultureInfo.InvariantCulture));
        double[] otherList = Array.ConvertAll(Console.ReadLine().Split(' '), x => double.Parse(x, CultureInfo.InvariantCulture));
        InlinedMix[] inlined = new InlinedMix[3];
        for (int i = 0; i < 3; ++i)
        {
            string[] words1 = Console.ReadLine().Split(' ');
            inlined[i] = new InlinedMix {integer = int.Parse(words1[0]), @char = words1[1][0], @float = double.Parse(words1[2], CultureInfo.InvariantCulture)};
        }
        MultilineMix multiline;
        multiline.integer2 = int.Parse(Console.ReadLine());
        multiline.@string = Console.ReadLine();
        multiline.float2 = double.Parse(Console.ReadLine(), CultureInfo.InvariantCulture);

        Floats(f, g, point, n, floatList, otherList, inlined, multiline);
    }
}
