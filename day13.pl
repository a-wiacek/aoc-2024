:- use_module(library(simplex)).
:- [utils].

parse_input(InputFile, Lines) :-
    read_file_to_string(InputFile, Input, []),
    split_to_lines(Input, Lines).

parse_button(Row, X, Y) :-
    re_matchsub("X.(\\d+), Y.(\\d+)", Row, ParsedRow),
    _{1:XAtom, 2:YAtom} :< ParsedRow,
    atom_number(XAtom, X),
    atom_number(YAtom, Y).

parse_case(ARow, BRow, PrizeRow, Delta, [XA, YA, XB, YB, XPrize, YPrize]) :-
    parse_button(ARow, XA, YA),
    parse_button(BRow, XB, YB),
    parse_button(PrizeRow, XLowPrize, YLowPrize),
    XPrize is XLowPrize + Delta,
    YPrize is YLowPrize + Delta.

parse_cases([], _, []).
parse_cases([ARow, BRow, PrizeRow | RestRows], Delta, [ParsedCase | RestParsed]) :-
    parse_case(ARow, BRow, PrizeRow, Delta, ParsedCase),
    parse_cases(RestRows, Delta, RestParsed).

minimize_tokens(Case, Score) :-
    gen_state(S0),
    tokens(Case, S0, S),
    variable_value(S, a, A),
    variable_value(S, b, B),
    Score is 3 * A + B.

tokens([XA, YA, XB, YB, XPrize, YPrize]) -->
    constraint([XA * a, XB * b] = XPrize),
    constraint([YA * a, YB * b] = YPrize),
    constraint([a] >= 0),
    constraint([b] >= 0),
    constraint(integral(a)),
    constraint(integral(b)),
    minimize([3 * a, b]).

count_tokens(InputFile, Delta, Result) :-
    parse_input(InputFile, Lines),
    parse_cases(Lines, Delta, Cases),
    convlist(minimize_tokens, Cases, Scores),
    sum_list(Scores, Result).

main :-
    % count_tokens("inputs/day13a_test.txt", 0, Result),
    % writeln(Result).
    % count_tokens("inputs/day13a.txt", 0, Result),
    % writeln(Result).
    % count_tokens("inputs/day13a_test.txt", 10000000000000, Result2),
    % writeln(Result2).
    count_tokens("inputs/day13a.txt", 10000000000000, Result2),
    writeln(Result2).
