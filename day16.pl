:- use_module(library(rbtrees)).
:- [dijkstra].
:- [utils].

:- table get_letter/3.

graph_vertex(Array, Point-Direction) :-
    get_letter(Array, Point, Char),
    \+ Char = '#',
    direction(Direction).

vertex_edges(Array, Point-Direction, Neighbours) :-
    % move in facing direction
    extend_point(Direction, Point, StepPoint),
    exclude({Array}/[P]>>get_letter(Array, P, '#'), [StepPoint], StepPoints),
    maplist({Direction}/[P, P-Direction-1]>>true, StepPoints, StepPointsDirections),
    % turn around
    findall(NewDirection, turn(Direction, NewDirection), NewDirections),
    maplist({Point}/[NewDirection, Point-NewDirection-1000]>>true, NewDirections, TurnPointsDirections),
    append(StepPointsDirections, TurnPointsDirections, NeighboursUnsorted),
    sort(NeighboursUnsorted, Neighbours).

parse_graph(Array, GraphRBTree) :-
    findall(Vertex, graph_vertex(Array, Vertex), Vertices),
    maplist({Array}/[Vertex, Vertex-Neighbours]>>vertex_edges(Array, Vertex, Neighbours), Vertices, Graph),
    list_to_rbtree(Graph, GraphRBTree).

solve_graph(InputFile, Result) :-
    read_array(InputFile, Array),
    parse_graph(Array, GraphRBTree),
    get_letter(Array, Start, 'S'),
    get_letter(Array, End, 'E'),
    dijkstra(GraphRBTree, Start-right, DistanceRBTree, _),
    !,
    findall(Value, (direction(Direction), rb_lookup(End-Direction, Value, DistanceRBTree)), Values),
    min_list(Values, Result).

inverse(up, down).
inverse(down, up).
inverse(left, right).
inverse(right, left).

insert_if_not_observed(Vertex, [OldQueue, OldObserved], [NewQueue, NewObserved]) :-
    (   (   rb_lookup(Vertex, _, OldQueue)
        ;   rb_lookup(Vertex, _, OldObserved)
        )
    ->  [NewQueue, NewObserved] = [OldQueue, OldObserved]
    ;   rb_insert(OldQueue, Vertex, Vertex, NewQueue),
        rb_insert(OldObserved, Vertex, Vertex, NewObserved)
    ).

go_back(Array, DistanceRBTree, QueueRBTree, ObservedRBTree, OptimalVertices) :-
    (   rb_empty(QueueRBTree)
    ->  rb_keys(ObservedRBTree, Observed),
        maplist([Point-_, Point]>>true, Observed, OptimalUnsorted),
        sort(OptimalUnsorted, OptimalVertices)
    ;   rb_del_min(QueueRBTree, Point-Direction, _, RestOfQueueRBTree),
        rb_insert(ObservedRBTree, Point-Direction, Point-Direction, ObservedRBTreeWithOneAdded),
        rb_lookup(Point-Direction, PointDistance, DistanceRBTree),
        inverse(Direction, InvDirection),
        vertex_edges(Array, Point-InvDirection, Neighbours),
        convlist({Point-InvDirection, PointDistance, DistanceRBTree}/[NBPoint-NBDirection-Weight, NBPoint-InvNBDirection]>>(
            inverse(NBDirection, InvNBDirection),
            rb_lookup(NBPoint-InvNBDirection, NBDistance, DistanceRBTree),
            NBDistance #= PointDistance - Weight
        ), Neighbours, MinNeighbours),
        foldl(insert_if_not_observed, MinNeighbours, [RestOfQueueRBTree, ObservedRBTreeWithOneAdded], [NewQueueRBTree, NewObservedRBTree]),
        go_back(Array, DistanceRBTree, NewQueueRBTree, NewObservedRBTree, OptimalVertices)
    ).

count_best_tiles(InputFile, Result) :-
    read_array(InputFile, Array),
    parse_graph(Array, GraphRBTree),
    get_letter(Array, Start, 'S'),
    get_letter(Array, End, 'E'),
    dijkstra(GraphRBTree, Start-right, DistanceRBTree, _),
    !,
    findall(Value, (direction(Direction), rb_lookup(End-Direction, Value, DistanceRBTree)), Values),
    min_list(Values, MinDistance),
    findall(EndPoint, (direction(Direction), EndPoint = End-Direction, rb_lookup(EndPoint, MinDistance, DistanceRBTree)), MinEndPoints),
    maplist([P, P-P]>>true, MinEndPoints, MinEndKVs),
    list_to_rbtree(MinEndKVs, QueueRBTree),
    rb_empty(ObservedRBTree),
    go_back(Array, DistanceRBTree, QueueRBTree, ObservedRBTree, OptimalVertices),
    length(OptimalVertices, Result).

main :-
    % solve_graph("inputs/day16a_test.txt", Result),
    % writeln(Result).
    % solve_graph("inputs/day16a.txt", Result),
    % writeln(Result).
    % count_best_tiles("inputs/day16a_test.txt", Result2),
    % writeln(Result2).
    count_best_tiles("inputs/day16a.txt", Result2),
    writeln(Result2).
