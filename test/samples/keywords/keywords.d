import std.algorithm.iteration : map;
import std.array : array, split;
import std.conv : to;
import std.stdio : stdin;

/// may conflict in c#
struct Console
{
    int a; /// the first letter of the alphabet
    int static_; /// an integer
}

/// may conflict in c#
struct System
{
    int return_; /// not the end of the function
    int[] void_; /// not nothing
}

/// not the main function
struct Main
{
    System int_; /// not an integer
    int ifTrue; /// should not cause conflict
}

/**
Params:
    if_ = not a condition
    class_ = not a class
    i = just a string
    in_ = not in
    for_ = not a loop
    words = contains lots of things
*/
void keywords(int if_, char class_, string i, Console in_, int[] for_, Main[] words)
{
    // TODO If this compiles, it is already a good step!
}

void main()
{
    int if_;
    stdin.readf("%d\n", if_);
    char class_;
    stdin.readf("%c\n", class_);
    string i;
    stdin.readf("%s\n", i);
    Console in_;
    stdin.readf("%d %d\n", in_.a, in_.static_);
    int[] for_;
    for_ = stdin.readln.split.map!(to!int).array;
    Main[] words;
    words.length = 2;
    for (size_t j = 0; j < words.length; j++)
    {
        stdin.readf("%d\n", words[j].int_.return_);
        words[j].int_.void_ = stdin.readln.split.map!(to!int).array;
        stdin.readf("%d\n", words[j].ifTrue);
    }

    keywords(if_, class_, i, in_, for_, words);
}
