using System;

/// A struct for the example
struct AStruct
{
    public int integer; //!< an integer
    public char character; //!< a char
}

class Program
{
    /// \param n a number, used as a size
    /// \param list a list of structs
    static void Example(int n, AStruct[] list)
    {
        /* TODO In a real life scenario, you will describe here what you want
        the end user to do with this generated code */
    }

    static void Main()
    {
        int n = int.Parse(Console.ReadLine());
        AStruct[] list = new AStruct[n];
        for (int i = 0; i < n; ++i)
        {
            string[] words = Console.ReadLine().Split(' ');
            list[i] = new AStruct {integer = int.Parse(words[0]), character = words[1][0]};
        }

        Example(n, list);
    }
}
