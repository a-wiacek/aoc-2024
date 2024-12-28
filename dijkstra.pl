:- use_module(library(clpfd)).
:- use_module(library(heaps)).
:- use_module(library(rbtrees)).

reduce_distance(
    Vertex-MinDistance,
    Neighbour-Weight,
    [DistanceRBTree, PreviousRBTree, PreviousQueueHeap],
    [NewDistanceRBTree, NewPreviousRBTree, NewQueueHeap]
) :-
    rb_lookup(Neighbour, OldDistance, DistanceRBTree),
    NewDistance #= MinDistance + Weight,
    (   NewDistance < OldDistance
    ->  rb_update(DistanceRBTree, Neighbour, NewDistance, NewDistanceRBTree),
        rb_update(PreviousRBTree, Neighbour, Vertex, NewPreviousRBTree),
        add_to_heap(PreviousQueueHeap, NewDistance, Neighbour, NewQueueHeap)
    ;   [NewDistanceRBTree, NewPreviousRBTree, NewQueueHeap] = [DistanceRBTree, PreviousRBTree, PreviousQueueHeap]
    ).

dijkstra_(GraphRBTree, QueueHeap, DistanceRBTree, PreviousRBTree, FinalDistanceRBTree, FinalPreviousRBTree) :-
(   empty_heap(QueueHeap)
->  FinalDistanceRBTree = DistanceRBTree, FinalPreviousRBTree = PreviousRBTree
;   get_from_heap(QueueHeap, MinDistance, Vertex, PoppedQueueHeap),
    (   rb_lookup(Vertex, DistanceInRBTree, DistanceRBTree),
        DistanceInRBTree < MinDistance
    ->  dijkstra_(GraphRBTree, PoppedQueueHeap, DistanceRBTree, PreviousRBTree, FinalDistanceRBTree, FinalPreviousRBTree)
    ;   rb_lookup(Vertex, Neighbours, GraphRBTree),
        foldl(
            reduce_distance(Vertex-MinDistance),
            Neighbours,
            [DistanceRBTree, PreviousRBTree, PoppedQueueHeap],
            [NewDistanceRBTree, NewPreviousRBTree, NewQueueHeap]
        ),
        dijkstra_(GraphRBTree, NewQueueHeap, NewDistanceRBTree, NewPreviousRBTree, FinalDistanceRBTree, FinalPreviousRBTree)
    )
).

% Graph is represented as red-black tree, where key are vertices
% and values are lists [vertices connected by edge-edge weight]
dijkstra(GraphRBTree, Source, DistanceRBTree, PreviousRBTree) :-
    rb_keys(GraphRBTree, Vertices),
    maplist([V, V-inf]>>true, Vertices, UninitDistanceArray),
    maplist([V, V-undefined]>>true, Vertices, InitPreviousArray),
    select(Source-inf, UninitDistanceArray, Source-0, InitDistanceArray),
    list_to_rbtree(InitDistanceArray, InitDistanceRBTree),
    list_to_rbtree(InitPreviousArray, InitPreviousRBTree),
    singleton_heap(QueueHeap, 0, Source),
    dijkstra_(GraphRBTree, QueueHeap, InitDistanceRBTree, InitPreviousRBTree, DistanceRBTree, PreviousRBTree).
