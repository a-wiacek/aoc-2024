:- use_module(library(rbtrees)).
:- [dijkstra].
:- [utils].

:- table get_letter/3.

add_obstacle(Point, Array, NewArray) :-
    update_array(Array, Point, _, '#', NewArray).

create_array(BoundX-BoundY, Obstacles, Array) :-
    replicate('.', BoundX, Row),
    replicate(Row, BoundY, EmptyArray),
    foldl(add_obstacle, Obstacles, EmptyArray, Array).

vertex_edges(Array, Point, Neighbours) :-
    findall(
        NewEdge,
        (   extend_point(_, Point, NewPoint),
            get_letter(Array, NewPoint, '.'),
            NewEdge = NewPoint-1
        ),
        Neighbours
    ).

parse_obstacle(String, X-Y) :-
    split_string(String, ",", "", Unparsed),
    maplist(atom_number, Unparsed, [X, Y]).

parse_obstacles(InputFile, Obstacles) :-
    read_file_to_string(InputFile, String, []),
    split_to_lines(String, Lines),
    maplist(parse_obstacle, Lines, Obstacles).

create_graph(Bounds, Obstacles, GraphRBTree) :-
    create_array(Bounds, Obstacles, Array),
    findall(Vertex, get_letter(Array, Vertex, '.'), Vertices),
    maplist({Array}/[Vertex, Vertex-Neighbours]>>vertex_edges(Array, Vertex, Neighbours), Vertices, Graph),
    list_to_rbtree(Graph, GraphRBTree).

place_obstacles_then_solve(L, Bound, Obstacles, Result) :-
    Bounds = Bound-Bound,
    split_at(L, Obstacles, ObstaclesTaken, _),
    create_graph(Bounds, ObstaclesTaken, GraphRBTree),
    Start = 0-0,
    dijkstra(GraphRBTree, Start, DistanceRBTree, _),
    !,
    E is Bound - 1,
    End = E-E,
    rb_lookup(End, Result, DistanceRBTree).

solve_graph(InputFile, Bound, MaxObstacles, Result) :-
    parse_obstacles(InputFile, Obstacles),
    length(Obstacles, ObstaclesCount),
    L is min(ObstaclesCount, MaxObstacles),
    place_obstacles_then_solve(L, Bound, Obstacles, Result).

lock(InputFile, Bound, Result) :-
    parse_obstacles(InputFile, Obstacles),
    length(Obstacles, ObstaclesCount),
    binsearch_min(
        {Bound, Obstacles}/[L]>>place_obstacles_then_solve(L, Bound, Obstacles, inf),
        0,
        ObstaclesCount,
        ResultAt
    ),
    nth1(ResultAt, Obstacles, X-Y),
    atomic_list_concat([X, Y], ",", Result).

main :-
    % solve_graph("inputs/day18a_test.txt", 7, 12, Result),
    % writeln(Result).
    % solve_graph("inputs/day18a.txt", 71, 1024, Result),
    % writeln(Result).
    % lock("inputs/day18a_test.txt", 7, Result2),
    % writeln(Result2).
    lock("inputs/day18a.txt", 71, Result2),
    writeln(Result2).
