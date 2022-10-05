% A: a first number
% B: a second number
% C: a third number
% N: This one on a new line
% OnePerLine: an integer list, one per line
manual_format(A, B, C, N, OnePerLine) :-
    % TODO From the function perspective, this is just 4 integers
    nl.

read_line(X) :- read_string(user_input, "\n", "\r", _, X).
read_number(X) :- read_line(S), number_string(X, S).
string_number(X, Y) :- number_string(Y, X).
read_number_list(X) :- read_line_to_codes(user_input, C), (C == [] -> X = []
    ; split_string(C, " ", "", L), maplist(string_number, L, X)).
read_list(_, 0, []) :- !.
read_list(Goal, N, [H|T]) :- call(Goal, H), M is N - 1, read_list(Goal, M, T).
:-
    prompt(_, ''),
    read_number_list([A, B, C]),
    read_number(N),
    read_list(read_number, 3, OnePerLine),
    manual_format(A, B, C, N, OnePerLine).
