:- [utils].

antennas_group(Array, Group) :-
    group_by(Char, Point, get_letter(Array, Point, Char), Group),
    \+ Char = '.'.

extend_point_by_point(X1-Y1, X2-Y2, X3-Y3) :-
    X3 is 2 * X2 - X1,
    Y3 is 2 * Y2 - Y1.
extend_point_by_point(X1-Y1, X2-Y2, X3-Y3) :-
    X3 is 2 * X1 - X2,
    Y3 is 2 * Y1 - Y2.

valid_point(BoundX-BoundY, X-Y) :-
    between(0, BoundX, X),
    between(0, BoundY, Y).

plus2(X1-Y1, X2-Y2, X3-Y3) :-
    X3 is X1 + X2,
    Y3 is Y1 + Y2.
minus2(X1-Y1, X2-Y2, X3-Y3) :-
    X3 is X1 - X2,
    Y3 is Y1 - Y2.

plus2(Bounds, P1, P2, P3) :-
    plus2(P1, P2, P3),
    valid_point(Bounds, P3).

% -----------

antinode(Bounds, Group, Point) :-
    member(Antenna1, Group),
    member(Antenna2, Group),
    \+ Antenna1 = Antenna2,
    extend_point_by_point(Antenna1, Antenna2, Point),
    valid_point(Bounds, Point).

group_antinodes(AntinodeGoal, Bounds, Group, Points) :-
    findall(Point, call(AntinodeGoal, Bounds, Group, Point), Points).

all_antinodes(AntinodeGoal, Array, Points) :-
    array_bounds(Array, Bounds),
    findall(Group, antennas_group(Array, Group), Groups),
    maplist(group_antinodes(AntinodeGoal, Bounds), Groups, DupPoints),
    append(DupPoints, FlatPoints),
    sort(FlatPoints, Points).

count_all_antinodes(InputFile, Result) :-
    read_array(InputFile, Array),
    all_antinodes(antinode, Array, Points),
    length(Points, Result).

line_of_points(Bounds, Point1, Point2, Point3) :-
    minus2(Point2, Point1, Delta),
    iterate_all(plus2(Bounds, Delta), Point2, List),
    member(Point3, List).

antinode2(Bounds, Group, Point) :-
    member(Antenna1, Group),
    member(Antenna2, Group),
    \+ Antenna1 = Antenna2,
    line_of_points(Bounds, Antenna1, Antenna2, Point).

count_all_antinodes2(InputFile, Result) :-
    read_array(InputFile, Array),
    all_antinodes(antinode2, Array, Points),
    length(Points, Result).

main :-
    % count_all_antinodes("inputs/day08a_test.txt", Result),
    % writeln(Result).
    % count_all_antinodes("inputs/day08a.txt", Result),
    % writeln(Result).
    % count_all_antinodes2("inputs/day08a_test.txt", Result2),
    % writeln(Result2).
    count_all_antinodes2("inputs/day08a.txt", Result2),
    writeln(Result2).
