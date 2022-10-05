% N: a number, used as a size
% List: a list of structs
example(N, List) :-
    % TODO In a real life scenario, you will describe here what you want the
    % end user to do with this generated code
    nl.

read_line(X) :- read_string(user_input, "\n", "\r", _, X).
read_number(X) :- read_line(S), number_string(X, S).
read_list(_, 0, []) :- !.
read_list(Goal, N, [H|T]) :- call(Goal, H), M is N - 1, read_list(Goal, M, T).
read_assoc_a_struct(X) :-
    read_line_to_codes(user_input, C), split_string(C, " ", "", L), empty_assoc(A0),
    nth0(0, L, L0), number_string(C0, L0), put_assoc("integer", A0, C0, A1),
    nth0(1, L, L1), sub_atom(L1, 0, 1, _, C1), put_assoc("character", A1, C1, A2),
    X = A2.
:-
    prompt(_, ''),
    read_number(N),
    read_list(read_assoc_a_struct, N, List),
    example(N, List).
