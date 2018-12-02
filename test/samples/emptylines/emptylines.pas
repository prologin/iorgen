program EmptyLines;

type
    { a char struct }
    StructWithAChar = record
        Char1: char; { a char }
        Int2: longint; { an integer }
    end;

    { a struct }
    A = record
        ListInStruct: array of longint; { a list in a struct }
        StructInStruct: StructWithAChar; { a struct in a struct }
    end;

    { a sized struct }
    SizedStruct = record
        Size: longint; { the size }
        StringInStruct: string; { the string }
    end;

    T_EmptyList = array of longint;
    T_EmptyInSample = array of longint;

{ @param EmptyList an empty list }
{ @param BufferString here to check correct parsing of empty line above }
{ @param N an integer, will be 0 in the sample input }
{ @param EmptyInSample an empty list (only in the sample) }
{ @param EmptyString an empty string }
{ @param Main an other buffer string }
{ @param EmptyCharList an empty char list }
{ @param NonEmptyCharList an char list, non empty }
{ @param StructWithEmptyLine a struct containing an empty line, then a struct }
{ @param ASizedStruct a sized struct containing an empty line }
{ @param Finish a string to finish }
procedure EmptyLines(const EmptyList: T_EmptyList; const BufferString: string; N: longint; const EmptyInSample: T_EmptyInSample; const EmptyString: string; const Main: string; const EmptyCharList: string; const NonEmptyCharList: string; const StructWithEmptyLine: A; const ASizedStruct: SizedStruct; const Finish: string);
begin
    {* TODO Wow, lots of empty lines! *}
end;

var
    EmptyList: T_EmptyList; { an empty list }
    BufferString: string; { here to check correct parsing of empty line above }
    N: longint; { an integer, will be 0 in the sample input }
    EmptyInSample: T_EmptyInSample; { an empty list (only in the sample) }
    EmptyString: string; { an empty string }
    Main: string; { an other buffer string }
    EmptyCharList: string; { an empty char list }
    NonEmptyCharList: string; { an char list, non empty }
    StructWithEmptyLine: A; { a struct containing an empty line, then a struct }
    ASizedStruct: SizedStruct; { a sized struct containing an empty line }
    Finish: string; { a string to finish }
    i: longint;
begin
    setLength(EmptyList, 0);
    for i := 0 to 0 - 1 do
        read(EmptyList[i]);
    readln();
    readln(BufferString);
    readln(N);
    setLength(EmptyInSample, N);
    for i := 0 to N - 1 do
        read(EmptyInSample[i]);
    readln();
    readln(EmptyString);
    readln(Main);
    readln(EmptyCharList);
    readln(NonEmptyCharList);
    setLength(StructWithEmptyLine.ListInStruct, N);
    for i := 0 to N - 1 do
        read(StructWithEmptyLine.ListInStruct[i]);
    readln();
    readln(StructWithEmptyLine.StructInStruct.Char1, StructWithEmptyLine.StructInStruct.Int2);
    readln(ASizedStruct.Size);
    readln(ASizedStruct.StringInStruct);
    readln(Finish);
    EmptyLines(EmptyList, BufferString, N, EmptyInSample, EmptyString, Main, EmptyCharList, NonEmptyCharList, StructWithEmptyLine, ASizedStruct, Finish);
end.
