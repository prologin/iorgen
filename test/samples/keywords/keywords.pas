program Keywords;

type
    { may conflict in c# }
    Console = record
        A: longint; { the first letter of the alphabet }
        Static: longint; { an integer }
    end;

    { may conflict in c# }
    System_ = record
        Return: longint; { not the end of the function }
        Void: array of longint; { not nothing }
    end;

    { not the main function }
    Main = record
        Int: System_; { not an integer }
        IfTrue: longint; { should not cause conflict }
    end;

    T_For_ = array of longint;
    T_Words = array of Main;

{ @param If_ not a condition }
{ @param Class not a class }
{ @param I just a string }
{ @param In_ not in }
{ @param For_ not a loop }
{ @param Words contains lots of things }
procedure Keywords(If_: longint; Class: char; const I: string; const In_: Console; const For_: T_For_; const Words: T_Words);
begin
    {* TODO If this compiles, it is already a good step! *}
end;

var
    If_: longint; { not a condition }
    Class: char; { not a class }
    I: string; { just a string }
    In_: Console; { not in }
    For_: T_For_; { not a loop }
    Words: T_Words; { contains lots of things }
    j, k: longint;
begin
    readln(If_);
    readln(Class);
    readln(I);
    readln(In_.A, In_.Static);
    setLength(For_, If_);
    for j := 0 to If_ - 1 do
        read(For_[j]);
    readln();
    setLength(Words, 2);
    for j := 0 to 2 - 1 do
    begin
        readln(Words[j].Int.Return);
        setLength(Words[j].Int.Void, 3);
        for k := 0 to 3 - 1 do
            read(Words[j].Int.Void[k]);
        readln();
        readln(Words[j].IfTrue);
    end;
    Keywords(If_, Class, I, In_, For_, Words);
end.
