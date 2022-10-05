% F: a float
% G: a float, greater than f
% Point: some coordinates
% N: a number
% FloatList: a list of floats
% OtherList: a list of floats
% Inlined: some inlined structs
% Multiline: a multiline struct
floats(F, G, Point, N, FloatList, OtherList, Inlined, Multiline) :-
    % TODO Parsing is often easy, reprint mode is harder
    nl.

read_line(X) :- read_string(user_input, "\n", "\r", _, X).
read_number(X) :- read_line(S), number_string(X, S).
string_number(X, Y) :- number_string(Y, X).
read_number_list(X) :- read_line_to_codes(user_input, C), (C == [] -> X = []
    ; split_string(C, " ", "", L), maplist(string_number, L, X)).
read_list(_, 0, []) :- !.
read_list(Goal, N, [H|T]) :- call(Goal, H), M is N - 1, read_list(Goal, M, T).
read_assoc_coordinates(X) :- read_number_list(L), pairs_keys_values(P, ["x", "y", "z"], L), list_to_assoc(P, X).
read_assoc_inlined_mix(X) :-
    read_line_to_codes(user_input, C), split_string(C, " ", "", L), empty_assoc(A0),
    nth0(0, L, L0), number_string(C0, L0), put_assoc("integer", A0, C0, A1),
    nth0(1, L, L1), sub_atom(L1, 0, 1, _, C1), put_assoc("char", A1, C1, A2),
    nth0(2, L, L2), number_string(C2, L2), put_assoc("float", A2, C2, A3),
    X = A3.
read_assoc_multiline_mix(X) :-
    read_number(Integer2),
    read_line(String),
    read_number(Float2),
    pairs_keys_values(P, ["integer 2", "string", "float 2"], [Integer2, String, Float2]), list_to_assoc(P, X).
:-
    prompt(_, ''),
    read_number(F),
    read_number(G),
    read_assoc_coordinates(Point),
    read_number(N),
    read_number_list(FloatList),
    read_number_list(OtherList),
    read_list(read_assoc_inlined_mix, 3, Inlined),
    read_assoc_multiline_mix(Multiline),
    floats(F, G, Point, N, FloatList, OtherList, Inlined, Multiline).
