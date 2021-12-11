program SizedStruct;

type
    { contains a list }
    List = record
        Size1: longint; { the list's size }
        IntList: array of longint; { the integer list }
    end;

    { contains a string }
    String_ = record
        Size2: longint; { the list's size }
        StringList: AnsiString; { the string list }
    end;

    { contains a matrix }
    Matrix = record
        Size3: longint; { the list's size }
        ListList: array of array of longint; { the list list }
    end;

    { this is not a 'sized struct', but a regular one! }
    NotASizedStruct = record
        Size4: longint; { not the list's size }
        IntListN: array of longint; { the integer list }
    end;

    T_Lists = array of List;
    T_Strings = array of String_;
    T_Matrices = array of Matrix;
    T_Same = array of NotASizedStruct;

{ @param N the size of the lists }
{ @param Lists a list of list of different sizes }
{ @param Strings a list of strings of different sizes }
{ @param Matrices a list of matrices of different sizes }
{ @param Same a list of list of same sizes }
procedure SizedStruct(N: longint; const Lists: T_Lists; const Strings: T_Strings; const Matrices: T_Matrices; const Same: T_Same);
begin
    {* TODO The is a special case. *}
end;

var
    N: longint; { the size of the lists }
    Lists: T_Lists; { a list of list of different sizes }
    Strings: T_Strings; { a list of strings of different sizes }
    Matrices: T_Matrices; { a list of matrices of different sizes }
    Same: T_Same; { a list of list of same sizes }
    i, j, k: longint;
begin
    readln(N);
    setLength(Lists, N);
    for i := 0 to N - 1 do
    begin
        readln(Lists[i].Size1);
        setLength(Lists[i].IntList, Lists[i].Size1);
        for j := 0 to Lists[i].Size1 - 1 do
            read(Lists[i].IntList[j]);
        readln();
    end;
    setLength(Strings, N);
    for i := 0 to N - 1 do
    begin
        readln(Strings[i].Size2);
        readln(Strings[i].StringList);
    end;
    setLength(Matrices, 2);
    for i := 0 to 2 - 1 do
    begin
        readln(Matrices[i].Size3);
        setLength(Matrices[i].ListList, Matrices[i].Size3, 2);
        for j := 0 to Matrices[i].Size3 - 1 do
        begin
            for k := 0 to 2 - 1 do
                read(Matrices[i].ListList[j][k]);
            readln();
        end;
    end;
    setLength(Same, N);
    for i := 0 to N - 1 do
    begin
        readln(Same[i].Size4);
        setLength(Same[i].IntListN, N);
        for j := 0 to N - 1 do
            read(Same[i].IntListN[j]);
        readln();
    end;
    SizedStruct(N, Lists, Strings, Matrices, Same);
end.
