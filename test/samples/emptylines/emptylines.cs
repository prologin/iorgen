using System;

/// a char struct
struct StructWithAChar
{
    public char char1; //!< a char
    public int int2; //!< an integer
}

/// a struct
struct A
{
    public int[] listInStruct; //!< a list in a struct
    public StructWithAChar structInStruct; //!< a struct in a struct
}

/// a sized struct
struct SizedStruct
{
    public int size; //!< the size
    public string stringInStruct; //!< the string
}

class Program
{
    /// \param emptyList an empty list
    /// \param bufferString here to check correct parsing of empty line above
    /// \param n an integer, will be 0 in the sample input
    /// \param emptyInSample an empty list (only in the sample)
    /// \param emptyString an empty string
    /// \param main an other buffer string
    /// \param emptyCharList an empty char list
    /// \param nonEmptyCharList an char list, non empty
    /// \param structWithEmptyLine a struct containing an empty line, then a struct
    /// \param aSizedStruct a sized struct containing an empty line
    /// \param finish a string to finish
    static void EmptyLines(int[] emptyList, string bufferString, int n, int[] emptyInSample, string emptyString, string main, char[] emptyCharList, char[] nonEmptyCharList, A structWithEmptyLine, SizedStruct aSizedStruct, string finish)
    {
        /* TODO Wow, lots of empty lines! */
    }

    static void Main()
    {
        int[] emptyList = Array.ConvertAll(Console.ReadLine().Split(' ', StringSplitOptions.RemoveEmptyEntries), int.Parse);
        string bufferString = Console.ReadLine();
        int n = int.Parse(Console.ReadLine());
        int[] emptyInSample = Array.ConvertAll(Console.ReadLine().Split(' ', StringSplitOptions.RemoveEmptyEntries), int.Parse);
        string emptyString = Console.ReadLine();
        string main = Console.ReadLine();
        char[] emptyCharList = Console.ReadLine().ToCharArray();
        char[] nonEmptyCharList = Console.ReadLine().ToCharArray();
        A structWithEmptyLine;
        structWithEmptyLine.listInStruct = Array.ConvertAll(Console.ReadLine().Split(' ', StringSplitOptions.RemoveEmptyEntries), int.Parse);
        string[] words = Console.ReadLine().Split(' ');
        structWithEmptyLine.structInStruct = new StructWithAChar {char1 = words[0][0], int2 = int.Parse(words[1])};
        SizedStruct aSizedStruct;
        aSizedStruct.size = int.Parse(Console.ReadLine());
        aSizedStruct.stringInStruct = Console.ReadLine();
        string finish = Console.ReadLine();

        EmptyLines(emptyList, bufferString, n, emptyInSample, emptyString, main, emptyCharList, nonEmptyCharList, structWithEmptyLine, aSizedStruct, finish);
    }
}
