:- [dijkstra].
:- [utils].

vertex_edges(Array, Point, Neighbours) :-
    findall(
        NewEdge,
        (   extend_point(_, Point, NewPoint),
            \+ get_letter(Array, NewPoint, '#'),
            NewEdge = NewPoint-1
        ),
        Neighbours
    ).

parse_graph(InputFile, Array, GraphRBTree) :-
    read_array(InputFile, Array),
    findall(Vertex, get_letter(Array, Vertex, _), Vertices),
    maplist({Array}/[Vertex, Vertex-Neighbours]>>vertex_edges(Array, Vertex, Neighbours), Vertices, Graph),
    list_to_rbtree(Graph, GraphRBTree).

point_in_distance(MaxDistance, X-Y, NewX-NewY, ActualDistance) :-
    between(0, MaxDistance, XDistance),
    (   XDistance = 0
    ->  XMul = 0
    ;   member(XMul, [-1, 1])
    ),
    NewX is X + XMul * XDistance,
    RemainingDistance is MaxDistance - XDistance,
    between(0, RemainingDistance, YDistance),
    (   YDistance = 0
    ->  YMul = 0
    ;   member(YMul, [-1, 1])
    ),
    NewY is Y + YMul * YDistance,
    ActualDistance is XDistance + YDistance.

cheat_from_given_point(DistanceRBTree, MaxCheatLength, Start, Cheat) :-
    rb_lookup(Start, StartDistance, DistanceRBTree),
    \+ StartDistance = inf,
    point_in_distance(MaxCheatLength, Start, End, CheatLength),
    rb_lookup(End, EndDistance, DistanceRBTree),
    \+ EndDistance = inf,
    CheatDistance is StartDistance + CheatLength,
    Diff is EndDistance - CheatDistance,
    Diff > 0,
    Cheat = cheat(Start, End, Diff).

cheats_from_given_point(DistanceRBTree, MaxCheatLength, Start, Cheats) :-
    findall(Cheat, cheat_from_given_point(DistanceRBTree, MaxCheatLength, Start, Cheat), Cheats).

cheat_sheet(DistanceRBTree, MaxCheatLength, Cheats) :-
    rb_keys(DistanceRBTree, Vertices),
    maplist(cheats_from_given_point(DistanceRBTree, MaxCheatLength), Vertices, CheatsPerPoint),
    append(CheatsPerPoint, Cheats).

% Assumption: File is a long path from start to end, without branching
cheats_in_run(InputFile, MaxCheatLength, Cheats) :-
    parse_graph(InputFile, Array, GraphRBTree),
    get_letter(Array, Start, 'S'),
    dijkstra(GraphRBTree, Start, DistanceRBTree, _),
    !,
    cheat_sheet(DistanceRBTree, MaxCheatLength, Cheats).

find_cheats(InputFile, MinDiff, MaxCheatLength, Result) :-
    cheats_in_run(InputFile, MaxCheatLength, Cheats),
    include({MinDiff}/[cheat(_, _, Diff)]>>(Diff >= MinDiff), Cheats, ValidCheats),
    length(ValidCheats, Result).

main :-
    % find_cheats("inputs/day20a_test.txt", 30, 2, Result),
    % writeln(Result).
    % find_cheats("inputs/day20a.txt", 100, 2, Result),
    % writeln(Result).
    % find_cheats("inputs/day20a_test.txt", 74, 20, Result),
    % writeln(Result).
    find_cheats("inputs/day20a.txt", 100, 20, Result),
    writeln(Result).
