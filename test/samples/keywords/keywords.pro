% If: not a condition
% Class: not a class
% I: just a string
% In: not in
% For: not a loop
% Words: contains lots of things
% Words1: an integer
keywords(If, Class, I, In, For, Words, Words1) :-
    % TODO If this compiles, it is already a good step!
    nl.

read_line(X) :- read_string(user_input, "\n", "\r", _, X).
read_char(X) :- read_line(S), string_chars(S, C), nth0(0, C, X).
read_number(X) :- read_line(S), number_string(X, S).
string_number(X, Y) :- number_string(Y, X).
read_number_list(X) :- read_line_to_codes(user_input, C), (C == [] -> X = []
    ; split_string(C, " ", "", L), maplist(string_number, L, X)).
read_list(_, 0, []) :- !.
read_list(Goal, N, [H|T]) :- call(Goal, H), M is N - 1, read_list(Goal, M, T).
read_assoc_console(X) :- read_number_list(L), pairs_keys_values(P, ["a", "static"], L), list_to_assoc(P, X).
read_assoc_system(X) :-
    read_number(Return),
    read_number_list(Void),
    pairs_keys_values(P, ["return", "void"], [Return, Void]), list_to_assoc(P, X).
read_assoc_main(X) :-
    read_assoc_system(Int),
    read_number(IfTrue),
    pairs_keys_values(P, ["int", "if true"], [Int, IfTrue]), list_to_assoc(P, X).
:-
    prompt(_, ''),
    read_number(If),
    read_char(Class),
    read_line(I),
    read_assoc_console(In),
    read_number_list(For),
    read_list(read_assoc_main, 2, Words),
    read_number(Words1),
    keywords(If, Class, I, In, For, Words, Words1).
