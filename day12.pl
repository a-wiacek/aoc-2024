:- [utils].

neighbouring_points(Point, NewPoints) :-
    findall(NewPoint, extend_point(_, Point, NewPoint), NewPoints).

collect_region(Array, PointsToConsider, PointsConsidered, Result) :-
    (   PointsToConsider = []
    ->  Result = PointsConsidered
    ;   PointsToConsider = [CurrentPoint | RestToConsider],
        get_letter(Array, CurrentPoint, X),
        neighbouring_points(CurrentPoint, Neighbours),
        include({Array, X}/[P]>>get_letter(Array, P, X), Neighbours, ValidNeighbours),
        append(PointsToConsider, PointsConsidered, AllPoints),
        subtract(ValidNeighbours, AllPoints, NewNeighbours),
        append(NewNeighbours, RestToConsider, NewToConsider),
        collect_region(Array, NewToConsider, [CurrentPoint | PointsConsidered], Result)
    ).

collect_regions(Array, Point, Regions, NewRegions) :-
    (   member(Region, Regions),
        member(Point, Region)
    ->  NewRegions = Regions
    ;   collect_region(Array, [Point], [], Region),
        NewRegions = [Region | Regions]
    ).

regions(Array, Regions) :-
    findall(Point, get_letter(Array, Point, _), Points),
    foldl(collect_regions(Array), Points, [], Regions).

perimeter(Region, Perimeter) :-
    maplist(neighbouring_points, Region, Neighbourhoods),
    maplist([NBH]>>subtract(NBH, Region), Neighbourhoods, BordersAt),
    maplist(length, BordersAt, BordersLengths),
    sum_list(BordersLengths, Perimeter).

region_value(Region, Value) :-
    length(Region, Area),
    perimeter(Region, Perimeter),
    Value is Area * Perimeter.

count_all_prices(InputFile, Result) :-
    read_array(InputFile, Array),
    regions(Array, Regions),
    maplist(region_value, Regions, Values),
    sum_list(Values, Result).

convex_corner(Region, [Point, Direction1]) :-
    member(Point, Region),
    extend_point(Direction1, Point, Ext1),
    \+ member(Ext1, Region),
    clockwise(Direction1, Direction2),
    extend_point(Direction2, Point, Ext2),
    \+ member(Ext2, Region).

convex_corners(Region, ConvexCount) :-
    findall(Convex, convex_corner(Region, Convex), ConvexCorners),
    length(ConvexCorners, ConvexCount).

concave_corner(Region, [Point, Direction1]) :-
    member(Point, Region),
    extend_point(Direction1, Point, Ext1),
    member(Ext1, Region),
    clockwise(Direction1, Direction2),
    extend_point(Direction2, Point, Ext2),
    member(Ext2, Region),
    extend_point(Direction2, Ext1, Ext3),
    \+ member(Ext3, Region).

concave_corners(Region, ConcaveCount) :-
    findall(Concave, concave_corner(Region, Concave), ConcaveCorners),
    length(ConcaveCorners, ConcaveCount).

sides(Region, Sides) :-
    convex_corners(Region, ConvexCount),
    concave_corners(Region, ConcaveCount),
    Sides is ConvexCount + ConcaveCount.

region_value2(Region, Value) :-
    length(Region, Area),
    sides(Region, Sides),
    Value is Area * Sides.

count_all_prices2(InputFile, Result) :-
    read_array(InputFile, Array),
    regions(Array, Regions),
    maplist(region_value2, Regions, Values),
    sum_list(Values, Result).

main :-
    % count_all_prices("inputs/day12a_test.txt", Result),
    % writeln(Result).
    % count_all_prices("inputs/day12a.txt", Result),
    % writeln(Result).
    % count_all_prices2("inputs/day12a_test.txt", Result2),
    % writeln(Result2).
    count_all_prices2("inputs/day12a.txt", Result2),
    writeln(Result2).
