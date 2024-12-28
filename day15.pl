:- [utils].

atom_direction('<', left).
atom_direction('>', right).
atom_direction('^', up).
atom_direction('v', down).

horizontal(left).
horizontal(right).
vertical(up).
vertical(down).

parse_input(InputFile, Array, Directions) :-
    read_file_to_string(InputFile, String, []),
    split_string(String, "\n", "", Lines),
    append([ArrayUnparsed, [""], DirectionsLinesUnparsed, [""]], Lines),
    maplist(string_chars, ArrayUnparsed, Array),
    maplist(string_chars, DirectionsLinesUnparsed, DirectionsLinesChars),
    append(DirectionsLinesChars, DirectionLineChar),
    maplist(atom_direction, DirectionLineChar, Directions).

extend_point_via_boxes(Array, Direction, P, FinalP) :-
    (   get_letter(Array, P, Char),
        member(Char, ['O', '[', ']'])
    ->  extend_point(Direction, P, NextP),
        extend_point_via_boxes(Array, Direction, NextP, FinalP)
    ;   FinalP = P
    ).

push_step(Direction, Array, NewArray) :-
    get_letter(Array, RobotPos, '@'),
    extend_point(Direction, RobotPos, BasePushPos),
    extend_point_via_boxes(Array, Direction, BasePushPos, FinalPushPos),
    (   get_letter(Array, FinalPushPos, '#')
    ->  NewArray = Array
    ;   get_letter(Array, FinalPushPos, '.')
    ->  update_array(Array, RobotPos, '@', '.', WithRobotRemoved),
        update_array(WithRobotRemoved, BasePushPos, _, '@', WithRobotReturned),
        (   BasePushPos = FinalPushPos % no boxes pushed
        ->  NewArray = WithRobotReturned
        ;   update_array(WithRobotReturned, FinalPushPos, '.', 'O', NewArray)
        )
    ).

point_score(X-Y, Score) :-
    Score is 100 * Y + X.

array_score(Char, Array, Score) :-
    findall(Point, get_letter(Array, Point, Char), Points),
    maplist(point_score, Points, Scores),
    sum_list(Scores, Score).

simulate(InputFile, Score) :-
    parse_input(InputFile, Array, Directions),
    foldl(push_step, Directions, Array, FinalArray),
    array_score('O', FinalArray, Score).

widen('#', ['#', '#']).
widen('O', ['[', ']']).
widen('.', ['.', '.']).
widen('@', ['@', '.']).

widen_array(Array, WideArray) :-
    maplist([Row, WideRow]>>maplist(widen, Row, WideRow), Array, WideArrayUnflattened),
    maplist(append, WideArrayUnflattened, WideArray).

push_wide_box(Direction, BoxStartPoint, Array, NewArray) :-
    horizontal(Direction),
    get_letter(Array, BoxStartPoint, BoxStart),
    \+ BoxStart = '#',
    extend_point(Direction, BoxStartPoint, BoxEndPoint),
    get_letter(Array, BoxEndPoint, BoxEnd),
    extend_point(Direction, BoxEndPoint, PastBoxPoint),
    (   get_letter(Array, PastBoxPoint, '.')
    ->  Array0 = Array
    ;   get_letter(Array, PastBoxPoint, BoxStart)
    ->  push_wide_box(Direction, PastBoxPoint, Array, Array0)
    ),
    update_array(Array0, PastBoxPoint, '.', BoxEnd, Array1),
    update_array(Array1, BoxEndPoint, BoxEnd, BoxStart, Array2),
    update_array(Array2, BoxStartPoint, BoxStart, '.', NewArray).

push_wide_box(Direction, Point, Array, NewArray) :-
    vertical(Direction),
    get_letter(Array, Point, T),
    (   T = '['
    ->  extend_point(right, Point, SecondBoxPoint),
        BothBoxPoints = [Point, SecondBoxPoint]
    ;   T = ']'
    ->  extend_point(left, Point, SecondBoxPoint),
        BothBoxPoints = [SecondBoxPoint, Point]
    ),
    BothBoxPoints = [LeftBoxPoint, RightBoxPoint],
    extend_point(Direction, LeftBoxPoint, ExtendedLeftBoxPoint),
    extend_point(Direction, RightBoxPoint, ExtendedRightBoxPoint),
    get_letter(Array, ExtendedLeftBoxPoint, L),
    get_letter(Array, ExtendedRightBoxPoint, R),
    % 1: both fields empty
    (   L = '.', R = '.'
    ->  RecDoneArray = Array
    % 2: only right field taken
    ;   L = '.', R = '['
    ->  push_wide_box(Direction, ExtendedRightBoxPoint, Array, RecDoneArray)
    % 3: only left field taken
    ;   L = ']', R = '.'
    ->  push_wide_box(Direction, ExtendedLeftBoxPoint, Array, RecDoneArray)
    % 4: both fields taken by two different boxes
    ;   L = ']', R = '['
    ->  push_wide_box(Direction, ExtendedLeftBoxPoint, Array, MidArray),
        push_wide_box(Direction, ExtendedRightBoxPoint, MidArray, RecDoneArray)
    % 5: both fields taken by the same box
    ;   L = '[', R = ']'
    ->  push_wide_box(Direction, ExtendedLeftBoxPoint, Array, RecDoneArray)
    ),
    update_array(RecDoneArray, ExtendedLeftBoxPoint, '.', '[', Array1),
    update_array(Array1, ExtendedRightBoxPoint, '.', ']', Array2),
    update_array(Array2, LeftBoxPoint, '[', '.', Array3),
    update_array(Array3, RightBoxPoint, ']', '.', NewArray).    

try_push_wide_and_move_robot(Direction, Array, NewArray) :-
    get_letter(Array, RobotPos, '@'),
    extend_point(Direction, RobotPos, BasePushPos),
    (   get_letter(Array, BasePushPos, '.')
    ->  update_array(Array, RobotPos, '@', '.', WithRobotRemoved),
        update_array(WithRobotRemoved, BasePushPos, '.', '@', NewArray)
    ;   push_wide_box(Direction, BasePushPos, Array, PushedArray)
    ->  update_array(PushedArray, RobotPos, '@', '.', WithRobotRemoved),
        update_array(WithRobotRemoved, BasePushPos, '.', '@', NewArray)
    ;   NewArray = Array
    ).

simulate2(InputFile, Score) :-
    parse_input(InputFile, Array, Directions),
    widen_array(Array, WideArray),
    foldl(try_push_wide_and_move_robot, Directions, WideArray, FinalArray),
    array_score('[', FinalArray, Score).

main :-
    % simulate("inputs/day15a_test.txt", Result),
    % writeln(Result).
    % simulate("inputs/day15a.txt", Result),
    % writeln(Result).
    % simulate2("inputs/day15a_test.txt", Result2),
    % writeln(Result2).
    simulate2("inputs/day15a.txt", Result2),
    writeln(Result2).
