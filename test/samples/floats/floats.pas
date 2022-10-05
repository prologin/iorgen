program Floats;

type
    { Represents coordinates }
    Coordinates = record
        X: double; { X }
        Y: double; { Y }
        Z: double; { Z }
    end;

    { Mix of fields that go on one line }
    InlinedMix = record
        Integer_: longint; { an integer }
        Char_: char; { a char }
        Float: double; { a float }
    end;

    { a struct of chars }
    MultilineMix = record
        Integer2: longint; { an other integer }
        String_: AnsiString; { a string of size 5 }
        Float2: double; { an other float }
    end;

    T_FloatList = array of double;
    T_OtherList = array of double;
    T_Inlined = array of InlinedMix;

{ @param F a float }
{ @param G a float, greater than f }
{ @param Point some coordinates }
{ @param N a number }
{ @param FloatList a list of floats }
{ @param OtherList a list of floats }
{ @param Inlined some inlined structs }
{ @param Multiline a multiline struct }
procedure Floats(F: double; G: double; const Point: Coordinates; N: longint; const FloatList: T_FloatList; const OtherList: T_OtherList; const Inlined: T_Inlined; const Multiline: MultilineMix);
begin
    {* TODO Parsing is often easy, reprint mode is harder *}
end;

var
    F: double; { a float }
    G: double; { a float, greater than f }
    Point: Coordinates; { some coordinates }
    N: longint; { a number }
    FloatList: T_FloatList; { a list of floats }
    OtherList: T_OtherList; { a list of floats }
    Inlined: T_Inlined; { some inlined structs }
    Multiline: MultilineMix; { a multiline struct }
    i: longint;
    _: char;
begin
    readln(F);
    readln(G);
    readln(Point.X, Point.Y, Point.Z);
    readln(N);
    setLength(FloatList, N);
    for i := 0 to N - 1 do
        read(FloatList[i]);
    readln();
    setLength(OtherList, 9);
    for i := 0 to 9 - 1 do
        read(OtherList[i]);
    readln();
    setLength(Inlined, 3);
    for i := 0 to 3 - 1 do
    begin
        readln(Inlined[i].Integer_, _, Inlined[i].Char_, Inlined[i].Float);
    end;
    readln(Multiline.Integer2);
    readln(Multiline.String_);
    readln(Multiline.Float2);
    Floats(F, G, Point, N, FloatList, OtherList, Inlined, Multiline);
end.
