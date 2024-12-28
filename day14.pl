:- [utils].

parse_line(Row, [X-Y, VelX-VelY]) :-
    re_matchsub("p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)", Row, ParsedRow),
    _{1:XAtom, 2:YAtom, 3:VelXAtom, 4:VelYAtom} :< ParsedRow,
    atom_number(XAtom, X),
    atom_number(YAtom, Y),
    atom_number(VelXAtom, VelX),
    atom_number(VelYAtom, VelY).
    
parse_input(InputFile, Robots) :-
    read_file_to_string(InputFile, Input, []),
    split_to_lines(Input, Lines),
    maplist(parse_line, Lines, Robots).

position(BoundX-BoundY, Times, [X-Y, VelX-VelY], FinalX-FinalY) :-
    FinalX is (X + VelX * Times) mod BoundX,
    FinalY is (Y + VelY * Times) mod BoundY.

in_bounds(XMin-YMin, XMax-YMax, X-Y) :-
    between(XMin, XMax, X),
    between(YMin, YMax, Y).

bound(L, [B1, B2, B3, B4]) :-
    B1 is 0,
    B2 is (L - 3) div 2,
    B3 is B2 + 2,
    B4 is L - 1.    

quadrant_count(BoundX-BoundY, Positions, SafetyFactor) :-
    % Q1 | Q2
    % ---+---
    % Q3 | Q4
    bound(BoundX, [X1, X2, X3, X4]),
    bound(BoundY, [Y1, Y2, Y3, Y4]),
    include(in_bounds(X1-Y1, X2-Y2), Positions, Q1),
    length(Q1, LQ1),
    include(in_bounds(X3-Y1, X4-Y2), Positions, Q2),
    length(Q2, LQ2),
    include(in_bounds(X1-Y3, X2-Y4), Positions, Q3),
    length(Q3, LQ3),
    include(in_bounds(X3-Y3, X4-Y4), Positions, Q4),
    length(Q4, LQ4),
    SafetyFactor is LQ1 * LQ2 * LQ3 * LQ4.

run_sim(Bounds, Times) -->
    parse_input,
    maplist(position(Bounds, Times)),
    quadrant_count(Bounds).

block(X-Y, FinalX-FinalY) :-
    XMin is X - 1,
    XMax is X + 1,
    YMin is Y - 1,
    YMax is Y + 1,
    between(XMin, XMax, FinalX),
    between(YMin, YMax, FinalY).

tree_at_n(Bounds, Robots, N) :- % look for 3x3 block
    maplist(position(Bounds, N), Robots, RobotsAtN),
    member(P, RobotsAtN),
    findall(InBlock, block(P, InBlock), Block),
    forall(member(PP, Block), member(PP, RobotsAtN)).

look_for_tree(Bounds, Robots, N, Result) :-
    writeln(N),
    (   tree_at_n(Bounds, Robots, N)
    ->  Result = N
    ;   Next is N + 1,
        look_for_tree(Bounds, Robots, Next, Result)
    ).

% Problems like that should never appear
where_is_tree(Bounds, InputFile, Result) :-
    parse_input(InputFile, Parsed),
    look_for_tree(Bounds, Parsed, 0, Result).

main :-
    % run_sim(11-7, 100, "inputs/day14a_test.txt", Result),
    % writeln(Result).
    % run_sim(101-103, 100, "inputs/day14a.txt", Result),
    % writeln(Result).
    where_is_tree(101-103, "inputs/day14a.txt", Result2),
    writeln(Result2).
