import std.algorithm.iteration : map;
import std.array : array, split;
import std.conv : to;
import std.stdio : stdin;
import std.string : chop;

/// a char struct
struct StructWithAChar
{
    char char1; /// a char
    int int2; /// an integer
}

/// a struct
struct A
{
    int[] listInStruct; /// a list in a struct
    StructWithAChar structInStruct; /// a struct in a struct
}

/// a sized struct
struct SizedStruct
{
    int size; /// the size
    string stringInStruct; /// the string
}

/**
Params:
    emptyList = an empty list
    bufferString = here to check correct parsing of empty line above
    n = an integer, will be 0 in the sample input
    emptyInSample = an empty list (only in the sample)
    emptyString = an empty string
    main_ = an other buffer string
    emptyCharList = an empty char list
    nonEmptyCharList = an char list, non empty
    structWithEmptyLine = a struct containing an empty line, then a struct
    aSizedStruct = a sized struct containing an empty line
    finish = a string to finish
*/
void emptyLines(int[] emptyList, string bufferString, int n, int[] emptyInSample, string emptyString, string main_, char[] emptyCharList, char[] nonEmptyCharList, A structWithEmptyLine, SizedStruct aSizedStruct, string finish)
{
    // TODO Wow, lots of empty lines!
}

void main()
{
    int[] emptyList;
    emptyList = stdin.readln.split.map!(to!int).array;
    string bufferString;
    stdin.readf("%s\n", &bufferString);
    int n;
    stdin.readf("%d\n", &n);
    int[] emptyInSample;
    emptyInSample = stdin.readln.split.map!(to!int).array;
    string emptyString;
    stdin.readf("%s\n", &emptyString);
    string main_;
    stdin.readf("%s\n", &main_);
    char[] emptyCharList;
    emptyCharList = stdin.readln.chop.to!(char[]);
    char[] nonEmptyCharList;
    nonEmptyCharList = stdin.readln.chop.to!(char[]);
    A structWithEmptyLine;
    structWithEmptyLine.listInStruct = stdin.readln.split.map!(to!int).array;
    stdin.readf("%c %d\n", &structWithEmptyLine.structInStruct.char1, &structWithEmptyLine.structInStruct.int2);
    SizedStruct aSizedStruct;
    stdin.readf("%d\n", &aSizedStruct.size);
    stdin.readf("%s\n", &aSizedStruct.stringInStruct);
    string finish;
    stdin.readf("%s\n", &finish);

    emptyLines(emptyList, bufferString, n, emptyInSample, emptyString, main_, emptyCharList, nonEmptyCharList, structWithEmptyLine, aSizedStruct, finish);
}
