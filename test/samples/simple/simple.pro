% N: the first number
% OtherNumber: the second number
simple(N, OtherNumber) :-
    % TODO Just do what you want with these numbers, like sum them.
    nl.

read_line(X) :- read_string(user_input, "\n", "\r", _, X).
read_int(X) :- read_line(S), number_string(X, S).
:-
    prompt(_, ''),
    read_int(N),
    read_int(OtherNumber),
    simple(N, OtherNumber).
