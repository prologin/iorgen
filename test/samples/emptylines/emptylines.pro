% EmptyList: an empty list
% BufferString: here to check correct parsing of empty line above
% N: an integer, will be 0 in the sample input
% EmptyInSample: an empty list (only in the sample)
% EmptyString: an empty string
% Main: an other buffer string
% EmptyCharList: an empty char list
% NonEmptyCharList: an char list, non empty
% StructWithEmptyLine: a struct containing an empty line, then a struct
% ASizedStruct: a sized struct containing an empty line
% Finish: a string to finish
empty_lines(EmptyList, BufferString, N, EmptyInSample, EmptyString, Main, EmptyCharList, NonEmptyCharList, StructWithEmptyLine, ASizedStruct, Finish) :-
    % TODO Wow, lots of empty lines!
    nl.

read_line(X) :- read_string(user_input, "\n", "\r", _, X).
read_char_list(X) :- read_line(S), string_chars(S, X).
read_number(X) :- read_line(S), number_string(X, S).
string_number(X, Y) :- number_string(Y, X).
read_number_list(X) :- read_line_to_codes(user_input, C), (C == [] -> X = []
    ; split_string(C, " ", "", L), maplist(string_number, L, X)).
read_assoc_struct_with_a_char(X) :-
    read_line_to_codes(user_input, C), split_string(C, " ", "", L), empty_assoc(A0),
    nth0(0, L, L0), sub_atom(L0, 0, 1, _, C0), put_assoc("char1", A0, C0, A1),
    nth0(1, L, L1), number_string(C1, L1), put_assoc("int2", A1, C1, A2),
    X = A2.
read_assoc_a(X) :-
    read_number_list(ListInStruct),
    read_assoc_struct_with_a_char(StructInStruct),
    pairs_keys_values(P, ["list in struct", "struct in struct"], [ListInStruct, StructInStruct]), list_to_assoc(P, X).
read_assoc_sized_struct(X) :-
    read_number(Size),
    read_line(StringInStruct),
    pairs_keys_values(P, ["size", "string in struct"], [Size, StringInStruct]), list_to_assoc(P, X).
:-
    prompt(_, ''),
    read_number_list(EmptyList),
    read_line(BufferString),
    read_number(N),
    read_number_list(EmptyInSample),
    read_line(EmptyString),
    read_line(Main),
    read_char_list(EmptyCharList),
    read_char_list(NonEmptyCharList),
    read_assoc_a(StructWithEmptyLine),
    read_assoc_sized_struct(ASizedStruct),
    read_line(Finish),
    empty_lines(EmptyList, BufferString, N, EmptyInSample, EmptyString, Main, EmptyCharList, NonEmptyCharList, StructWithEmptyLine, ASizedStruct, Finish).
