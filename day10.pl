:- [utils].

parse_input(InputFile, Array) :-
    read_file_to_string(InputFile, Input, []),
    split_to_lines(Input, Lines),
    maplist(string_chars, Lines, AtomArray),
    maplist(maplist(atom_number), AtomArray, Array).

get_value(Array, X-Y, Value) :-
    nth0(Y, Array, Row),
    nth0(X, Row, Value).

valid_point(X-Y, BoundX-BoundY) :-
    between(0, BoundX, X),
    between(0, BoundY, Y).

neighbouring_points(Point, NewPoints) :-
    findall(NewPoint, extend_point(_, Point, NewPoint), NewPoints).

score_array(Array, ScoreArray) :-
    length(Array, LengthY),
    [Row | _] = Array,
    length(Row, LengthX),
    replicate('.', LengthX, ScoreRow),
    replicate(ScoreRow, LengthY, ScoreArray).

value_is(Array, Level, P) :- get_value(Array, P, Level).

add_score(LevelArray, DefaultValueGoal, CombineValuesGoal, Level, Point, ScoreArray, NewScoreArray) :-
    (   Level = 10
    ->  call(DefaultValueGoal, Point, DefaultValue),
        update_array(ScoreArray, Point, _, DefaultValue, NewScoreArray)
    ;   neighbouring_points(Point, NewPoints),
        include(value_is(LevelArray, Level), NewPoints, NewValidPoints),
        maplist(get_value(ScoreArray), NewValidPoints, Values),
        call(CombineValuesGoal, Values, Value),
        update_array(ScoreArray, Point, _, Value, NewScoreArray)
    ).

add_scores(LevelArray, DefaultValueGoal, CombineValuesGoal, [ScoreArray, CurrentLevel], [NewScoreArray, NextLevel]) :-
    CurrentLevel >= 0,
    NextLevel is CurrentLevel - 1,
    PreviousLevel is CurrentLevel + 1,
    findall(CurrentLevelPoint, get_value(LevelArray, CurrentLevelPoint, CurrentLevel), Points),
    foldl(add_score(LevelArray, DefaultValueGoal, CombineValuesGoal, PreviousLevel), Points, ScoreArray, NewScoreArray).

iterate_levels(LevelArray, DefaultValueGoal, CombineValuesGoal, ScoreArray) :-
    score_array(LevelArray, InitScoreArray),
    iterate1(add_scores(LevelArray, DefaultValueGoal, CombineValuesGoal), [InitScoreArray, 9], [ScoreArray, _]).

singleton(X, [X]).
union2(Lists, List) :- foldl(union, Lists, [], List).

trailheads(InputFile, Result) :-
    parse_input(InputFile, LevelArray),
    iterate_levels(LevelArray, singleton, union2, ScoreArray),
    findall(Point, get_value(LevelArray, Point, 0), Points),
    maplist(get_value(ScoreArray), Points, Reachables),
    append(Reachables, AllReachables),
    length(AllReachables, Result).

const1(_, 1).

ratings(InputFile, Result) :-
    parse_input(InputFile, LevelArray),
    iterate_levels(LevelArray, const1, sumlist, ScoreArray),
    findall(Point, get_value(LevelArray, Point, 0), Points),
    maplist(get_value(ScoreArray), Points, Ratings),
    sumlist(Ratings, Result).

main :-
    % trailheads("inputs/day10a_test.txt", Result),
    % writeln(Result).
    % trailheads("inputs/day10a.txt", Result),
    % writeln(Result).
    % ratings("inputs/day10a_test.txt", Result2),
    % writeln(Result2).
    ratings("inputs/day10a.txt", Result2),
    writeln(Result2).
