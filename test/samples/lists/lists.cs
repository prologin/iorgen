using System;

class Program
{
    /// \param n the first list's size
    /// \param listInt a list containing ints
    /// \param size an other size
    /// \param listChar a list of char
    /// \param listString4 a list of strings of size 4
    /// \param matrix a matrix of int
    static void Lists(int n, int[] listInt, int size, char[] listChar, string[] listString4, int[][] matrix)
    {
        /* TODO Aren't these lists beautifull? */
    }

    static void Main()
    {
        int n = int.Parse(Console.ReadLine());
        int[] listInt = Array.ConvertAll(Console.ReadLine().Split(' ', StringSplitOptions.RemoveEmptyEntries), int.Parse);
        int size = int.Parse(Console.ReadLine());
        char[] listChar = Console.ReadLine().ToCharArray();
        string[] listString4 = new string[size];
        for (int i = 0; i < size; ++i)
        {
            listString4[i] = Console.ReadLine();
        }
        int[][] matrix = new int[size][];
        for (int i = 0; i < size; ++i)
        {
            matrix[i] = Array.ConvertAll(Console.ReadLine().Split(' ', StringSplitOptions.RemoveEmptyEntries), int.Parse);
        }

        Lists(n, listInt, size, listChar, listString4, matrix);
    }
}
