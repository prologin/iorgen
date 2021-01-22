using System;

class Program
{
    /// \param n the first list's size
    /// \param listInt a list containing ints
    /// \param size an other size
    /// \param listChar a list of char
    /// \param @string a string
    /// \param listString4 a list of strings of size 4
    /// \param listListString2 a list of list of strings of size 2 of size 2 of size 2
    /// \param matrix a matrix of int
    static void Lists(int n, int[] listInt, int size, char[] listChar, string @string, string[] listString4, string[][] listListString2, int[][] matrix)
    {
        /* TODO Aren't these lists beautifull? */
    }

    static void Main()
    {
        int n = int.Parse(Console.ReadLine());
        int[] listInt = Array.ConvertAll(Console.ReadLine().Split(' '), int.Parse);
        int size = int.Parse(Console.ReadLine());
        char[] listChar = Console.ReadLine().ToCharArray();
        string @string = Console.ReadLine();
        string[] listString4 = new string[size];
        for (int i = 0; i < size; ++i)
        {
            listString4[i] = Console.ReadLine();
        }
        string[][] listListString2 = new string[2][];
        for (int i = 0; i < 2; ++i)
        {
            listListString2[i] = new string[2];
            for (int j = 0; j < 2; ++j)
            {
                listListString2[i][j] = Console.ReadLine();
            }
        }
        int[][] matrix = new int[size][];
        for (int i = 0; i < size; ++i)
        {
            matrix[i] = Array.ConvertAll(Console.ReadLine().Split(new char[] {' '}, StringSplitOptions.RemoveEmptyEntries), int.Parse);
        }

        Lists(n, listInt, size, listChar, @string, listString4, listListString2, matrix);
    }
}
