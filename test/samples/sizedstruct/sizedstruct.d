import std.algorithm.iteration : map;
import std.array : array, split;
import std.conv : to;
import std.stdio : stdin;

/// contains a list
struct List
{
    int size1; /// the list's size
    int[] intList; /// the integer list
}

/// contains a string
struct String
{
    int size2; /// the list's size
    string stringList; /// the string list
}

/// contains a matrix
struct Matrix
{
    int size3; /// the list's size
    int[][] listList; /// the list list
}

/// this is not a 'sized struct', but a regular one!
struct NotASizedStruct
{
    int size4; /// not the list's size
    int[] intListN; /// the integer list
}

/**
Params:
    n = the size of the lists
    lists = a list of list of different sizes
    strings = a list of strings of different sizes
    matrices = a list of matrices of different sizes
    same = a list of list of same sizes
*/
void sizedStruct(int n, List[] lists, String[] strings, Matrix[] matrices, NotASizedStruct[] same)
{
    // TODO The is a special case.
}

void main()
{
    int n;
    stdin.readf("%d\n", &n);
    List[] lists;
    lists.length = n;
    for (size_t i = 0; i < lists.length; i++)
    {
        stdin.readf("%d\n", &lists[i].size1);
        lists[i].intList = stdin.readln.split.map!(to!int).array;
    }
    String[] strings;
    strings.length = n;
    for (size_t i = 0; i < strings.length; i++)
    {
        stdin.readf("%d\n", &strings[i].size2);
        stdin.readf("%s\n", &strings[i].stringList);
    }
    Matrix[] matrices;
    matrices.length = 2;
    for (size_t i = 0; i < matrices.length; i++)
    {
        stdin.readf("%d\n", &matrices[i].size3);
        matrices[i].listList.length = matrices[i].size3;
        for (size_t j = 0; j < matrices[i].listList.length; j++)
        {
            matrices[i].listList[j] = stdin.readln.split.map!(to!int).array;
        }
    }
    NotASizedStruct[] same;
    same.length = n;
    for (size_t i = 0; i < same.length; i++)
    {
        stdin.readf("%d\n", &same[i].size4);
        same[i].intListN = stdin.readln.split.map!(to!int).array;
    }

    sizedStruct(n, lists, strings, matrices, same);
}
