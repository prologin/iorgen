program ManualFormat;

type
    T_OnePerLine = array of longint;

{ @param A a first number }
{ @param B a second number }
{ @param C a third number }
{ @param N This one on a new line }
{ @param OnePerLine an integer list, one per line }
procedure ManualFormat(A: longint; B: longint; C: longint; N: longint; const OnePerLine: T_OnePerLine);
begin
    {* TODO From the function perspective, this is just 4 integers *}
end;

var
    A: longint; { a first number }
    B: longint; { a second number }
    C: longint; { a third number }
    N: longint; { This one on a new line }
    OnePerLine: T_OnePerLine; { an integer list, one per line }
    i: longint;
begin
    readln(A, B, C);
    readln(N);
    setLength(OnePerLine, 3);
    for i := 0 to 3 - 1 do
    begin
        readln(OnePerLine[i]);
    end;
    ManualFormat(A, B, C, N, OnePerLine);
end.
