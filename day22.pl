:- [utils].

mix(MathOp, Secret, NewSecret) :-
    call(MathOp, Secret, Value),
    MidSecret is Secret xor Value,
    NewSecret is MidSecret mod 16777216.
mul(Times, Secret, NewSecret) :-
    NewSecret is Secret * Times.
div(Times, Secret, NewSecret) :-
    NewSecret is Secret // Times.
step -->
    mix(mul(64)),
    mix(div(32)),
    mix(mul(2048)). 

solve(InputFile, Result) :-
    read_file_to_string(InputFile, Input, []),
    split_to_lines(Input, InputLines),
    maplist(atom_number, InputLines, Numbers),
    maplist(iterateN(step, 2000), Numbers, SecretNumbers),
    sum_list(SecretNumbers, Result).

sequence_selling_costs([_, _, _, _], PriceTree, PriceTree).
sequence_selling_costs([P1, P2, P3, P4, P5 | Ps], PriceTree, NewPriceTree) :-
    Rest = [P2, P3, P4, P5 | Ps],
    D1 is P2 - P1,
    D2 is P3 - P2,
    D3 is P4 - P3,
    D4 is P5 - P4,
    Diff = s(D1, D2, D3, D4),
    (   rb_insert_new(PriceTree, Diff, P5, MidPriceTree)
    ->  sequence_selling_costs(Rest, MidPriceTree, NewPriceTree)
    ;   sequence_selling_costs(Rest, PriceTree, NewPriceTree)
    ).

get_sequence_selling_costs(SecretList, PriceTree) :-
    rb_empty(T),
    sequence_selling_costs(SecretList, T, PriceTree).

diff_sequence(s(X, Y, Z, T)) :-
    between(-9, 9, X),
    between(-9, 9, Y),
    between(-9, 9, Z),
    between(-9, 9, T).

lookup(Sequence, PriceTree, Value) :-
    rb_lookup(Sequence, Value, PriceTree).

sequence_selling_value(PriceTrees, Sequence, Value) :-
    convlist(lookup(Sequence), PriceTrees, Values),
    sum_list(Values, Value).

prepare_single_sequence -->
    atom_number,
    iterate_all_n(step, 2000),
    maplist([Price, P]>>(P is Price mod 10)),
    get_sequence_selling_costs.

add_at(Key-Value, RBTree, NewRBTree) :-
    rb_update(RBTree, Key, OldValue, NewValue, NewRBTree),
    NewValue is OldValue + Value.

add_trees_by_coordinates(RBTree1, RBTree2, RBTree3) :-
    rb_fold(add_at, RBTree2, RBTree1, RBTree3).

prepare_and_add_single_sequence(InputLine, RBTree, NewRBTree) :-
    prepare_single_sequence(InputLine, PriceRBTree),
    add_trees_by_coordinates(RBTree, PriceRBTree, NewRBTree).

sell(InputFile, Result) :-
    set_prolog_flag(stack_limit, 4_294_967_296),
    read_file_to_string(InputFile, Input, []),
    split_to_lines(Input, InputLines),
    findall(Seq, (diff_sequence(Sequence), Seq = Sequence-0), Seqs),
    list_to_rbtree(Seqs, EmptySequenceRBTree),
    foldl(prepare_and_add_single_sequence, InputLines, EmptySequenceRBTree, FilledRBTree),
    rb_fold([_-X, Y, Z]>>(Z is max(X, Y)), FilledRBTree, 0, Result),
    !.

main :-
    % solve("inputs/day22a_test.txt", Result),
    % writeln(Result).
    % solve("inputs/day22a.txt", Result),
    % writeln(Result).
    % sell("inputs/day22b_test.txt", Result),
    % writeln(Result).
    sell("inputs/day22a.txt", Result),
    writeln(Result).
