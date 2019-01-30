import std.algorithm.iteration : map;
import std.array : array, split;
import std.conv : to;
import std.stdio : stdin;
import std.string : chop;

/**
Params:
    n = the first list's size
    listInt = a list containing ints
    size = an other size
    listChar = a list of char
    listString4 = a list of strings of size 4
    matrix = a matrix of int
*/
void lists(int n, int[] listInt, int size, char[] listChar, string[] listString4, int[][] matrix)
{
    // TODO Aren't these lists beautifull?
}

void main()
{
    int n;
    stdin.readf("%d\n", n);
    int[] listInt;
    listInt = stdin.readln.split.map!(to!int).array;
    int size;
    stdin.readf("%d\n", size);
    char[] listChar;
    listChar = stdin.readln.chop.to!(char[]);
    string[] listString4;
    listString4.length = size;
    for (size_t i = 0; i < listString4.length; i++)
    {
        stdin.readf("%s\n", listString4[i]);
    }
    int[][] matrix;
    matrix.length = size;
    for (size_t i = 0; i < matrix.length; i++)
    {
        matrix[i] = stdin.readln.split.map!(to!int).array;
    }

    lists(n, listInt, size, listChar, listString4, matrix);
}
