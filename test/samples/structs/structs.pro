% Struct: a struct 1 instance
% N: a number
% StructList: a list a struct 1
% Triangle: a triangle
% StructChars: a struct of chars
structs(Struct, N, StructList, Triangle, StructChars) :-
    % TODO Look at them structs.
    nl.

read_line(X) :- read_string(user_input, "\n", "\r", _, X).
read_char(X) :- read_line(S), string_chars(S, C), nth0(0, C, X).
read_char_list(X) :- read_line(S), string_chars(S, X).
read_int(X) :- read_line(S), number_string(X, S).
string_number(X, Y) :- number_string(Y, X).
read_int_list(X) :- read_line_to_codes(user_input, C), (C == [] -> X = []
    ; split_string(C, " ", "", L), maplist(string_number, L, X)).
read_list(_, 0, []) :- !.
read_list(Goal, N, [H|T]) :- call(Goal, H), M is N - 1, read_list(Goal, M, T).
read_assoc_struct_1(X) :- read_int_list(L), pairs_keys_values(P, ["foo", "bar"], L), list_to_assoc(P, X).
read_assoc_position(X) :- read_int_list(L), pairs_keys_values(P, ["x", "y", "z"], L), list_to_assoc(P, X).
read_assoc_point(X) :-
    read_char(Name),
    read_assoc_position(Pos),
    pairs_keys_values(P, ["name", "pos"], [Name, Pos]), list_to_assoc(P, X).
read_assoc_chars(X) :- read_line(S), atomic_list_concat(L, ' ',S), pairs_keys_values(P, ["first char", "second char", "third char"], L), list_to_assoc(P, X).
:-
    prompt(_, ''),
    read_assoc_struct_1(Struct),
    read_int(N),
    read_list(read_assoc_struct_1, N, StructList),
    read_list(read_assoc_point, 3, Triangle),
    read_assoc_chars(StructChars),
    structs(Struct, N, StructList, Triangle, StructChars).
