using System;

/// contains a list
struct List
{
    public int size1; //!< the list's size
    public int[] intList; //!< the integer list
}

/// contains a string
struct String_
{
    public int size2; //!< the list's size
    public string stringList; //!< the string list
}

/// contains a matrix
struct Matrix
{
    public int size3; //!< the list's size
    public int[][] listList; //!< the list list
}

/// this is not a 'sized struct', but a regular one!
struct NotASizedStruct
{
    public int size4; //!< not the list's size
    public int[] intListN; //!< the integer list
}

class Program
{
    /// \param n the size of the lists
    /// \param lists a list of list of different sizes
    /// \param strings a list of strings of different sizes
    /// \param matrices a list of matrices of different sizes
    /// \param same a list of list of same sizes
    static void SizedStruct(int n, List[] lists, String_[] strings, Matrix[] matrices, NotASizedStruct[] same)
    {
        /* TODO The is a special case. */
    }

    static void Main()
    {
        int n = int.Parse(Console.ReadLine());
        List[] lists = new List[n];
        for (int i = 0; i < n; ++i)
        {
            lists[i].size1 = int.Parse(Console.ReadLine());
            lists[i].intList = Array.ConvertAll(Console.ReadLine().Split(' '), int.Parse);
        }
        String_[] strings = new String_[n];
        for (int i = 0; i < n; ++i)
        {
            strings[i].size2 = int.Parse(Console.ReadLine());
            strings[i].stringList = Console.ReadLine();
        }
        Matrix[] matrices = new Matrix[2];
        for (int i = 0; i < 2; ++i)
        {
            matrices[i].size3 = int.Parse(Console.ReadLine());
            matrices[i].listList = new int[matrices[i].size3][];
            for (int j = 0; j < matrices[i].size3; ++j)
            {
                matrices[i].listList[j] = Array.ConvertAll(Console.ReadLine().Split(' '), int.Parse);
            }
        }
        NotASizedStruct[] same = new NotASizedStruct[n];
        for (int i = 0; i < n; ++i)
        {
            same[i].size4 = int.Parse(Console.ReadLine());
            same[i].intListN = Array.ConvertAll(Console.ReadLine().Split(' '), int.Parse);
        }

        SizedStruct(n, lists, strings, matrices, same);
    }
}
