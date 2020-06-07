program Lists;

type
    T_ListInt = array of longint;
    T_ListString4 = array of string;
    T_Matrix = array of array of longint;

{ @param N the first list's size }
{ @param ListInt a list containing ints }
{ @param Size an other size }
{ @param ListChar a list of char }
{ @param String_ a string }
{ @param ListString4 a list of strings of size 4 }
{ @param Matrix a matrix of int }
procedure Lists(N: longint; const ListInt: T_ListInt; Size: longint; const ListChar: string; const String_: string; const ListString4: T_ListString4; const Matrix: T_Matrix);
begin
    {* TODO Aren't these lists beautifull? *}
end;

var
    N: longint; { the first list's size }
    ListInt: T_ListInt; { a list containing ints }
    Size: longint; { an other size }
    ListChar: string; { a list of char }
    String_: string; { a string }
    ListString4: T_ListString4; { a list of strings of size 4 }
    Matrix: T_Matrix; { a matrix of int }
    i, j: longint;
begin
    readln(N);
    setLength(ListInt, N);
    for i := 0 to N - 1 do
        read(ListInt[i]);
    readln();
    readln(Size);
    readln(ListChar);
    readln(String_);
    setLength(ListString4, Size);
    for i := 0 to Size - 1 do
    begin
        readln(ListString4[i]);
    end;
    setLength(Matrix, Size, Size);
    for i := 0 to Size - 1 do
    begin
        for j := 0 to Size - 1 do
            read(Matrix[i][j]);
        readln();
    end;
    Lists(N, ListInt, Size, ListChar, String_, ListString4, Matrix);
end.
