% N: the first list's size
% ListInt: a list containing ints
% Size: an other size
% ListChar: a list of char
% String: a string
% ListString4: a list of strings of size 4
% Matrix: a matrix of int
lists(N, ListInt, Size, ListChar, String, ListString4, Matrix) :-
    % TODO Aren't these lists beautifull?
    nl.

read_line(X) :- read_string(user_input, "\n", "\r", _, X).
read_char_list(X) :- read_line(S), string_chars(S, X).
read_int(X) :- read_line(S), number_string(X, S).
string_number(X, Y) :- number_string(Y, X).
read_int_list(X) :- read_line_to_codes(user_input, C), (C == [] -> X = []
    ; split_string(C, " ", "", L), maplist(string_number, L, X)).
read_list(_, 0, []) :- !.
read_list(Goal, N, [H|T]) :- call(Goal, H), M is N - 1, read_list(Goal, M, T).
:-
    prompt(_, ''),
    read_int(N),
    read_int_list(ListInt),
    read_int(Size),
    read_char_list(ListChar),
    read_line(String),
    read_list(read_line, Size, ListString4),
    read_list(read_int_list, Size, Matrix),
    lists(N, ListInt, Size, ListChar, String, ListString4, Matrix).
