import std.stdio : stdin;

/// A struct for the example
struct AStruct
{
    int integer; /// an integer
    char character; /// a char
}

/**
Params:
    n = a number, used as a size
    list = a list of structs
*/
void example(int n, AStruct[] list)
{
    // TODO In a real life scenario, you will describe here what you want the
    // end user to do with this generated code
}

void main()
{
    int n;
    stdin.readf("%d\n", n);
    AStruct[] list;
    list.length = n;
    for (size_t i = 0; i < list.length; i++)
    {
        stdin.readf("%d %c\n", list[i].integer, list[i].character);
    }

    example(n, list);
}
