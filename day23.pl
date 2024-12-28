:- use_module(library(rbtrees)).
:- [utils].

parse_input(InputFile, Terms) :-
    read_file_to_string(InputFile, String, []),
    split_to_lines(String, Lines),
    maplist(term_string, Terms, Lines).

add_edge(From, To, GraphRBTree, NewGraphRBTree) :-
    (   rb_apply(
           GraphRBTree,
           From,
           {To} / [Edges, NewEdges] >> rb_insert(Edges, To, x, NewEdges),
           NewGraphRBTree
        )
    ;   list_to_rbtree([To-x], ToTree),
        rb_insert(GraphRBTree, From, ToTree, NewGraphRBTree)
    ),
    !.

add_connection(X-Y) -->
    add_edge(X, Y),
    add_edge(Y, X).

create_graph(Terms, GraphRBTree) :-
    rb_empty(RBTree),
    foldl(add_connection, Terms, RBTree, GraphRBTree).

connected(GraphRBTree, From, To) :-
    rb_lookup(From, Edges, GraphRBTree),
    rb_lookup(To, x, Edges).

triangle_in_graph(GraphRBTree, X, Y, Z) :-
    connected(GraphRBTree, X, Y),
    connected(GraphRBTree, Y, Z),
    connected(GraphRBTree, Z, X).

triangles_in_graph(GraphRBTree, Triangles) :-
    findall(
        triangle(X, Y, Z),
        (   rb_keys(GraphRBTree, Vertices),
            append(_, [X | XRest], Vertices),
            append(_, [Y | YRest], XRest),
            connected(GraphRBTree, X, Y),
            append(_, [Z | _], YRest),
            connected(GraphRBTree, Y, Z),
            connected(GraphRBTree, Z, X)
        ),
        Triangles
    ).

t_vertex(X) :- atom_chars(X, ['t' | _]).
t_triangle(triangle(X, _, _)) :- t_vertex(X).
t_triangle(triangle(_, Y, _)) :- t_vertex(Y).
t_triangle(triangle(_, _, Z)) :- t_vertex(Z).

count_all_t_triangles -->
    parse_input,
    create_graph,
    triangles_in_graph,
    include(t_triangle),
    length.

intersect_trees(Tree1, Tree2, Tree3) :-
    rb_empty(EmptyTree),
    rb_keys(Tree1, Keys),
    foldl(
        {Tree2} / [Key, Tree, NewTree] >>
            (   rb_lookup(Key, x, Tree2)
            ->  rb_insert(Tree, Key, x, NewTree)
            ;   NewTree = Tree
            ),
        Keys,
        EmptyTree,
        Tree3
    ).

branch_find_maximal_clique(
    GraphRBTree,
    [InCliqueRBTree, MaybeInCliqueRBTree, NotInCliqueRBTree],
    Clique
) :-
    % Prepare for next recur calls
    rb_del_min(MaybeInCliqueRBTree, Vertex, x, NewMaybeInCliqueRBTree),
    rb_insert(NotInCliqueRBTree, Vertex, x, NewNotInCliqueRBTree),
    % Prepare for this recur call
    rb_insert(InCliqueRBTree, Vertex, x, RecurInCliqueRBTree),
    rb_lookup(Vertex, Neighbours, GraphRBTree),
    intersect_trees(MaybeInCliqueRBTree, Neighbours, RecurMaybeInCliqueRBTree),
    intersect_trees(NotInCliqueRBTree, Neighbours, RecurNotInCliqueRBTree),
    (   find_maximal_clique(
            GraphRBTree,
            [RecurInCliqueRBTree, RecurMaybeInCliqueRBTree, RecurNotInCliqueRBTree],
            Clique
        )
    ;   branch_find_maximal_clique(
            GraphRBTree,
            [InCliqueRBTree, NewMaybeInCliqueRBTree, NewNotInCliqueRBTree],
            Clique
        )
    ).

find_maximal_clique(
    GraphRBTree,
    [InCliqueRBTree, MaybeInCliqueRBTree, NotInCliqueRBTree],
    Clique
) :-
    (   (   rb_empty(MaybeInCliqueRBTree),
            rb_empty(NotInCliqueRBTree),
            rb_keys(InCliqueRBTree, Clique)
        )
    ;   branch_find_maximal_clique(
            GraphRBTree,
            [InCliqueRBTree, MaybeInCliqueRBTree, NotInCliqueRBTree],
            Clique
        )
    ).

% Bron–Kerbosch
find_maximal_clique(GraphRBTree, Clique) :-
    rb_empty(T),
    rb_map(GraphRBTree, [_, x]>>true, Vertices),
    find_maximal_clique(GraphRBTree, [T, Vertices, T], Clique).

print_largest_clique(InputFile, Result) :-
    parse_input(InputFile, Terms),
    create_graph(Terms, GraphRBTree),
    findall(Clique, find_maximal_clique(GraphRBTree, Clique), Cliques),
    maplist(length, Cliques, CliquesLengths),
    max_list(CliquesLengths, MaxCliqueLength),
    include(
        {MaxCliqueLength} / [Clique] >> (length(Clique, MaxCliqueLength)),
        Cliques,
        [TheLargestClique | _]
    ),
    atomic_list_concat(TheLargestClique, ",", Result).

main :-
    % count_all_t_triangles("inputs/day23a_test.txt", Result),
    % writeln(Result).
    % count_all_t_triangles("inputs/day23a.txt", Result),
    % writeln(Result).
    % print_largest_clique("inputs/day23a_test.txt", Result),
    % writeln(Result).
    print_largest_clique("inputs/day23a.txt", Result),
    writeln(Result).
