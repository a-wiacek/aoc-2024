:- use_module(library(clpfd)).

split_to_lines(String, SubStrings) :-
    split_string(String, "\n", "\n", SubStrings).

split_and_read_line(String, SepChars, Numbers) :-
    split_string(String, SepChars, " ", SubStrings),
    maplist(atom_number, SubStrings, Numbers).

split_and_read_line(String, Numbers) :-
    split_and_read_line(String, " ", Numbers).

get_raw_number_lists(InputFile, SplitInputLines) :-
    read_file_to_string(InputFile, Input, []),
    split_to_lines(Input, InputLines),
    maplist(split_and_read_line, InputLines, SplitInputLines).

read_array(InputFile, Array) :-
    read_file_to_string(InputFile, String, []),
    split_to_lines(String, Lines),
    maplist(string_chars, Lines, Array).

array_bounds(Array, BoundX-BoundY) :-
    length(Array, LengthY),
    BoundY is LengthY - 1,
    [Row | _] = Array,
    length(Row, LengthX),
    BoundX is LengthX - 1.

head([X | _], X).

% goal should terminate at some point, otherwise iterate and iterate1 will diverge
iterate_all(Goal, Elem0, List) :-
    (   call(Goal, Elem0, Elem1)
    ->  iterate_all(Goal, Elem1, Tail),
        List = [Elem0 | Tail]
    ;   List = [Elem0]
    ).

iterate_all_n(Goal, Times, Elem0, List) :-
    (   Times = 0
    ->  List = [Elem0]
    ;   NewTimes is Times - 1,
        call(Goal, Elem0, Elem1),
        iterate_all_n(Goal, NewTimes, Elem1, Tail),
        List = [Elem0 | Tail]
    ).

% goal should terminate at some point, otherwise iterate and iterate1 will diverge
iterate1(Goal, Elem0, ElemN) :-
    (   call(Goal, Elem0, Elem1)
    ->  iterate1(Goal, Elem1, ElemN)
    ;   ElemN = Elem0
    ).

iterateN(Goal, Times, Elem0, ElemN) :-
    (   Times = 0
    ->  ElemN = Elem0
    ;   NewTimes is Times - 1,
        call(Goal, Elem0, Elem1),
        iterateN(Goal, NewTimes, Elem1, ElemN)
    ).

replicate(Elem, Times, List) :-
    length(List, Times),
    maplist(=(Elem), List).

split_at_rec(0, List, RevTake, Take, List) :-
    reverse(RevTake, Take).
split_at_rec(Index, [H | Tail], RevTake, Take, Drop) :-
    NewIndex is Index - 1,
    split_at_rec(NewIndex, Tail, [H | RevTake], Take, Drop).
split_at(Index, List, Take, Drop) :-
    split_at_rec(Index, List, [], Take, Drop).

update_array(Array, X-Y, OldElem, NewElem, NewArray) :-
    split_at(Y, Array, LeftRows, [Row | RightRows]),
    split_at(X, Row, LeftElems, [OldElem | RightElems]),
    append(LeftElems, [NewElem | RightElems], NewRow),
    append(LeftRows, [NewRow | RightRows], NewArray).

% a, b -> gcd(a, b) = a * x + b * y
ext_gcd(A, B, X, Y) :-
    (   B = 0
    ->  X = 1, Y = 0
    ;   divmod(A, B, Q, R),
        ext_gcd(B, R, S, T),
        X is T,
        Y is S - Q * T
    ).

get_letter(Array, X-Y, Value) :-
    nth0(Y, Array, Row),
    nth0(X, Row, Value).

direction(up).
direction(down).
direction(left).
direction(right).

extend_point(up, X-Y, X-NewY) :- NewY is Y - 1.
extend_point(down, X-Y, X-NewY) :- NewY is Y + 1.
extend_point(left, X-Y, NewX-Y) :- NewX is X - 1.
extend_point(right, X-Y, NewX-Y) :- NewX is X + 1.

clockwise(up, right).
clockwise(right, down).
clockwise(down, left).
clockwise(left, up).
counterclockwise(X, Y) :- clockwise(Y, X).
turn(X, Y) :- clockwise(X, Y).
turn(X, Y) :- counterclockwise(X, Y).

% find the smallest X in range [Min, Max], for which Goal does not fail
% predicate fails if goal isn ot satisfiable by any number in range
binsearch_min(Goal, Min, Max, X) :-
    (   Min > Max
    ->  fail
    ;   Min =:= Max ->
        call(Goal, Min),
        X = Min
    ;   Mid is (Min + Max) // 2,
        (   call(Goal, Mid) ->
            binsearch_min(Goal, Min, Mid, X)
        ;   NewMin is Mid + 1,
            binsearch_min(Goal, NewMin, Max, X)
        )
    ).
