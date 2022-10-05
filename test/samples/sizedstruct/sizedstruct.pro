% N: the size of the lists
% Lists: a list of list of different sizes
% Strings: a list of strings of different sizes
% Matrices: a list of matrices of different sizes
% Same: a list of list of same sizes
sized_struct(N, Lists, Strings, Matrices, Same) :-
    % TODO The is a special case.
    nl.

read_line(X) :- read_string(user_input, "\n", "\r", _, X).
read_number(X) :- read_line(S), number_string(X, S).
string_number(X, Y) :- number_string(Y, X).
read_number_list(X) :- read_line_to_codes(user_input, C), (C == [] -> X = []
    ; split_string(C, " ", "", L), maplist(string_number, L, X)).
read_list(_, 0, []) :- !.
read_list(Goal, N, [H|T]) :- call(Goal, H), M is N - 1, read_list(Goal, M, T).
read_assoc_list(X) :-
    read_number(Size1),
    read_number_list(IntList),
    pairs_keys_values(P, ["size1", "int list"], [Size1, IntList]), list_to_assoc(P, X).
read_assoc_string(X) :-
    read_number(Size2),
    read_line(StringList),
    pairs_keys_values(P, ["size2", "string list"], [Size2, StringList]), list_to_assoc(P, X).
read_assoc_matrix(X) :-
    read_number(Size3),
    read_list(read_number_list, Size3, ListList),
    pairs_keys_values(P, ["size3", "list list"], [Size3, ListList]), list_to_assoc(P, X).
read_assoc_not_a_sized_struct(X) :-
    read_number(Size4),
    read_number_list(IntListN),
    pairs_keys_values(P, ["size4", "int list n"], [Size4, IntListN]), list_to_assoc(P, X).
:-
    prompt(_, ''),
    read_number(N),
    read_list(read_assoc_list, N, Lists),
    read_list(read_assoc_string, N, Strings),
    read_list(read_assoc_matrix, 2, Matrices),
    read_list(read_assoc_not_a_sized_struct, N, Same),
    sized_struct(N, Lists, Strings, Matrices, Same).
