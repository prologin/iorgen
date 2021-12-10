module lists;

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
    string_ = a string
    listString4 = a list of strings of size 4
    listListString2 = a list of list of strings of size 2 of size 2 of size 2
    matrix = a matrix of int
*/
void lists(int n, int[] listInt, int size, char[] listChar, string string_, string[] listString4, string[][] listListString2, int[][] matrix)
{
    // TODO Aren't these lists beautifull?
}

void main()
{
    int n;
    stdin.readf("%d\n", &n);
    int[] listInt;
    listInt = stdin.readln.split.map!(to!int).array;
    int size;
    stdin.readf("%d\n", &size);
    char[] listChar;
    listChar = stdin.readln.chop.to!(char[]);
    string string_;
    stdin.readf("%s\n", &string_);
    string[] listString4;
    listString4.length = size;
    for (size_t i = 0; i < listString4.length; i++)
    {
        stdin.readf("%s\n", &listString4[i]);
    }
    string[][] listListString2;
    listListString2.length = 2;
    for (size_t i = 0; i < listListString2.length; i++)
    {
        listListString2[i].length = 2;
        for (size_t j = 0; j < listListString2[i].length; j++)
        {
            stdin.readf("%s\n", &listListString2[i][j]);
        }
    }
    int[][] matrix;
    matrix.length = size;
    for (size_t i = 0; i < matrix.length; i++)
    {
        matrix[i] = stdin.readln.split.map!(to!int).array;
    }

    lists(n, listInt, size, listChar, string_, listString4, listListString2, matrix);
}
