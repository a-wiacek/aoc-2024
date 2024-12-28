:- use_module(library(clpfd)).
:- [utils].

on_the_edge(Array, Direction, X-Y) :-
    array_bounds(Array, BoundX-BoundY),
    (   Direction = up
    ->  Y = 0
    ;   Direction = down
    ->  Y = BoundY
    ;   Direction = left
    ->  X = 0
    ;   Direction = right
    ->  X = BoundX
    ).

iter_on_path(Array, Path, Result) :-
    [[Point, Direction] | Rest] = Path,
    (   on_the_edge(Array, Direction, Point)
    ->  Result = Path  
    ;   member([Point, Direction], Rest)
    ->  Result = loop
    ;   extend_point(Direction, Point, NewPoint),
        (   get_letter(Array, NewPoint, '#')
        ->  clockwise(Direction, NewDirection),
            iter_on_path(Array, [[Point, NewDirection] | Path], Result)
        ;   iter_on_path(Array, [[NewPoint, Direction] | Path], Result)
        )
    ).

get_path(Array, Path) :-
    get_letter(Array, Start, '^'),
    iter_on_path(Array, [[Start, up]], Path).

count_path(InputFile, Result) :-
    read_array(InputFile, Array),
    get_path(Array, Path),
    maplist(head, Path, PointPath),
    sort(PointPath, UniquePointPath), % removes duplicates!
    length(UniquePointPath, Result).

split_at_rec(0, List, RevTake, Take, List) :-
    reverse(RevTake, Take).
split_at_rec(Index, [H | Tail], RevTake, Take, Drop) :-
    NewIndex is Index - 1,
    split_at_rec(NewIndex, Tail, [H | RevTake], Take, Drop).
split_at(Index, List, Take, Drop) :-
    split_at_rec(Index, List, [], Take, Drop).

with_obstacle(Array, X-Y, NewArray) :-
    update_array(Array, X-Y, '.', '#', NewArray).

loopable(Array) :-
    get_path(Array, loop).

find_loopable(InputFile, NewArray) :-
    read_array(InputFile, Array),
    get_path(Array, Path),
    maplist(head, Path, PointPath),
    sort(PointPath, UniquePointPath), % removes duplicates!
    length(PointPath, Length),
    member(Point, UniquePointPath),
    with_obstacle(Array, Point, NewArray),
    loopable(NewArray).

count_loopable(InputFile, Result) :-
    set_prolog_flag(stack_limit, 2_147_483_648),
    findall(NewArray, find_loopable(InputFile, NewArray), LoopableArrays),
    length(LoopableArrays, Result).

main :-
    % count_path("inputs/day06a_test.txt", Result),
    % writeln(Result).
    % count_path("inputs/day06a.txt", Result),
    % writeln(Result).
    % count_loopable("inputs/day06a_test.txt", Result2),
    % writeln(Result2).
    count_loopable("inputs/day06a.txt", Result2), % takes several hours
    writeln(Result2).
