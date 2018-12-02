program Example;

type
    { A struct for the example }
    AStruct = record
        Integer_: longint; { an integer }
        Character: char; { a char }
    end;

    T_List = array of AStruct;

{ @param N a number, used as a size }
{ @param List a list of structs }
procedure Example(N: longint; const List: T_List);
begin
    {* TODO In a real life scenario, you will describe here what you want the
    end user to do with this generated code *}
end;

var
    N: longint; { a number, used as a size }
    List: T_List; { a list of structs }
    i: longint;
    _: char;
begin
    readln(N);
    setLength(List, N);
    for i := 0 to N - 1 do
    begin
        readln(List[i].Integer_, _, List[i].Character);
    end;
    Example(N, List);
end.
