using System;

/// may conflict in c#
struct Console_
{
    public int a; //!< the first letter of the alphabet
    public int @static; //!< an integer
}

/// may conflict in c#
struct System_
{
    public int @return; //!< not the end of the function
    public int[] @void; //!< not nothing
}

/// not the main function
struct Main_
{
    public System_ @int; //!< not an integer
    public int ifTrue; //!< should not cause conflict
}

class Program
{
    /// \param @if not a condition
    /// \param @class not a class
    /// \param i just a string
    /// \param @in not in
    /// \param @for not a loop
    /// \param words contains lots of things
    static void Keywords(int @if, char @class, string i, Console_ @in, int[] @for, Main_[] words)
    {
        /* TODO If this compiles, it is already a good step! */
    }

    static void Main()
    {
        int @if = int.Parse(Console.ReadLine());
        char @class = Console.ReadLine()[0];
        string i = Console.ReadLine();
        string[] words1 = Console.ReadLine().Split(' ');
        Console_ @in = new Console_ {a = int.Parse(words1[0]), @static = int.Parse(words1[1])};
        int[] @for = Array.ConvertAll(Console.ReadLine().Split(' '), int.Parse);
        Main_[] words = new Main_[2];
        for (int j = 0; j < 2; ++j)
        {
            words[j].@int.@return = int.Parse(Console.ReadLine());
            words[j].@int.@void = Array.ConvertAll(Console.ReadLine().Split(' '), int.Parse);
            words[j].ifTrue = int.Parse(Console.ReadLine());
        }

        Keywords(@if, @class, i, @in, @for, words);
    }
}
