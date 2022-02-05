program Structs;

type
    { A simple struct }
    Struct1 = record
        Foo: longint; { a field }
        Bar: longint; { a field }
    end;

    { Represents a position }
    Position = record
        X: longint; { X }
        Y: longint; { Y }
        Z: longint; { Z }
    end;

    { A point's name and position }
    Point = record
        Name_: char; { the point's name (single character) }
        Description: AnsiString; { the point's description }
        Pos: Position; { the point's position }
    end;

    { a struct of chars }
    Chars = record
        FirstChar: char; { a first char }
        SecondChar: char; { a second char }
        ThirdChar: char; { a third char }
    end;

    { contains a big list inside }
    WithList = record
        Int: longint; { int }
        BigList: array of array of array of longint; { list nested 3 times! }
    end;

    T_StructList = array of Struct1;
    T_Triangle = array of Point;

{ @param Struct a struct 1 instance }
{ @param N a number }
{ @param StructList a list a struct 1 }
{ @param Triangle a triangle }
{ @param StructChars a struct of chars }
{ @param BigListStruct the big list struct }
procedure Structs(const Struct: Struct1; N: longint; const StructList: T_StructList; const Triangle: T_Triangle; const StructChars: Chars; const BigListStruct: WithList);
begin
    {* TODO Look at them structs. *}
end;

var
    Struct: Struct1; { a struct 1 instance }
    N: longint; { a number }
    StructList: T_StructList; { a list a struct 1 }
    Triangle: T_Triangle; { a triangle }
    StructChars: Chars; { a struct of chars }
    BigListStruct: WithList; { the big list struct }
    i, j, k: longint;
    _: char;
begin
    readln(Struct.Foo, Struct.Bar);
    readln(N);
    setLength(StructList, N);
    for i := 0 to N - 1 do
    begin
        readln(StructList[i].Foo, StructList[i].Bar);
    end;
    setLength(Triangle, 3);
    for i := 0 to 3 - 1 do
    begin
        readln(Triangle[i].Name_);
        readln(Triangle[i].Description);
        readln(Triangle[i].Pos.X, Triangle[i].Pos.Y, Triangle[i].Pos.Z);
    end;
    readln(StructChars.FirstChar, _, StructChars.SecondChar, _, StructChars.ThirdChar);
    readln(BigListStruct.Int);
    setLength(BigListStruct.BigList, 2, 2, 2);
    for i := 0 to 2 - 1 do
    begin
        for j := 0 to 2 - 1 do
        begin
            for k := 0 to 2 - 1 do
                read(BigListStruct.BigList[i][j][k]);
            readln();
        end;
    end;
    Structs(Struct, N, StructList, Triangle, StructChars, BigListStruct);
end.
