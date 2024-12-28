:- [utils].

direction(upleft).
direction(upright).
direction(downleft).
direction(downright).

extend_point(upleft, X-Y, NewX-NewY) :- NewX is X - 1, NewY is Y - 1.
extend_point(upright, X-Y, NewX-NewY) :- NewX is X + 1, NewY is Y - 1.
extend_point(downleft, X-Y, NewX-NewY) :- NewX is X - 1, NewY is Y + 1.
extend_point(downright, X-Y, NewX-NewY) :- NewX is X + 1, NewY is Y + 1.

valid_point(X-Y, BoundX-BoundY) :-
    between(0, BoundX, X),
    between(0, BoundY, Y).

extend_line(StartLine, ByLength, Direction, Bounds, Line) :-
    (   ByLength = 0
    ->  Line = StartLine
    ;   [Point | _] = StartLine,
        extend_point(Direction, Point, NewPoint),
        valid_point(NewPoint, Bounds),
        RemainingLength is ByLength - 1,
        extend_line([NewPoint | StartLine], RemainingLength, Direction, Bounds, Line)
    ).

valid_coordinate(BoundX-BoundY, X-Y) :-
    between(0, BoundX, X),
    between(0, BoundY, Y).

valid_line(Bound, Length, Line, Direction) :-
    valid_coordinate(Bound, Point),
    direction(Direction),
    NewLength is Length - 1,
    extend_line([Point], NewLength, Direction, Bound, Line).

line_text(Array, Line, Text) :-
    maplist(get_letter(Array), Line, ArrayOfChars),
    string_chars(Text, ArrayOfChars).

valid_value_line(Array, Text, Line, Direction) :-
    string_length(Text, Length),
    array_bounds(Array, Bound),
    valid_line(Bound, Length, Line, Direction),
    line_text(Array, Line, Text).

search_for_sequence(InputFile, Result) :-
    read_array(InputFile, Array),
    findall(Line, valid_value_line(Array, "XMAS", Line, _), Lines),
    length(Lines, Result).

%% ------------------

orthogonal(upleft, upright).
orthogonal(upleft, downleft).
orthogonal(downright, upright).
orthogonal(downright, downleft).

inverse(upleft, downright).
inverse(upright, downleft).
inverse(downleft, upright).
inverse(downright, upleft).

valid_x_mas(Array, Center) :-
    array_bounds(Array, Bound),
    valid_coordinate(Bound, Center),
    get_letter(Array, Center, 'A'),
    orthogonal(Direction1, Direction2),
    inverse(Direction1, Inv1),
    extend_point(Inv1, Center, H1),
    valid_point(H1, Bound),
    extend_point(Direction1, Center, T1),
    valid_point(T1, Bound),
    inverse(Direction2, Inv2),
    extend_point(Inv2, Center, H2),
    valid_point(H2, Bound),
    extend_point(Direction2, Center, T2),
    valid_point(T2, Bound),
    Line1 = [H1, Center, T1],
    Line2 = [H2, Center, T2],
    line_text(Array, Line1, "MAS"),
    line_text(Array, Line2, "MAS").

search_for_cross(InputFile, Result) :-
    read_array(InputFile, Array),
    findall(X, valid_x_mas(Array, X), Xs),
    length(Xs, Result).

main :-
    % search_for_sequence("inputs/day04a_test.txt", CountResult),
    % writeln(CountResult).
    % search_for_sequence("inputs/day04a.txt", CountResult),
    % writeln(CountResult).
    % search_for_cross("inputs/day04a_test.txt", CountResult2),
    % writeln(CountResult2).
    search_for_cross("inputs/day04a.txt", CountResult2),
    writeln(CountResult2).
    

