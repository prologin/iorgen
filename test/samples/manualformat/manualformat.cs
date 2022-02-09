using System;

class Program
{
    /// \param a a first number
    /// \param b a second number
    /// \param c a third number
    /// \param n This one on a new line
    /// \param onePerLine an integer list, one per line
    static void ManualFormat(int a, int b, int c, int n, int[] onePerLine)
    {
        /* TODO From the function perspective, this is just 4 integers */
    }

    static void Main()
    {
        string[] words = Console.ReadLine().Split(' ');
        int a = int.Parse(words[0]);
        int b = int.Parse(words[1]);
        int c = int.Parse(words[2]);
        int n = int.Parse(Console.ReadLine());
        int[] onePerLine = new int[3];
        for (int i = 0; i < 3; ++i)
        {
            onePerLine[i] = int.Parse(Console.ReadLine());
        }

        ManualFormat(a, b, c, n, onePerLine);
    }
}
