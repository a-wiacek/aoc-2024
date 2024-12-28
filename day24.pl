:- use_module(library(rbtrees)).
:- [utils].

xor(0, 0, 0).
xor(0, 1, 1).
xor(1, 0, 1).
xor(1, 1, 0).

and(1, 1, 1).
and(_, _, 0).

or(0, 0, 0).
or(_, _, 1).

parse_var_row(Row, init(Var, Value)) :-
    re_matchsub("(.+): (\\d)", Row, ParsedRow),
    _{1:Var, 2:StringValue} :< ParsedRow,
    atom_number(StringValue, Value).

parse_rule_row(Row, op(Var1, Op, Var2, Var3)) :-
    re_matchsub("(.+) (.+) (.+) -> (.+)", Row, ParsedRow),
    _{1:Var1, 2:OpUnparsed, 3:Var2, 4:Var3} :< ParsedRow,
    string_lower(OpUnparsed, OpLowerCase),
    atom_string(Op, OpLowerCase).

parse_row(Row, Result) :-
    (   parse_var_row(Row, Result)
    ;   parse_rule_row(Row, Result)
    ),
    !.

swap_outputs(
    [op(Var11, Op1, Var12, Var13), op(Var21, Op2, Var22, Var23)],
    [op(Var11, Op1, Var12, Var23), op(Var21, Op2, Var22, Var13)]
).

calc_circuit(op(Var1, Op, Var2, Var3), CircuitRBTree, NewCircuitRBTree) :-
    rb_lookup(Var1, Value1, CircuitRBTree),
    rb_lookup(Var2, Value2, CircuitRBTree),
    call(Op, Value1, Value2, Value3),
    rb_insert(CircuitRBTree, Var3, Value3, NewCircuitRBTree).

int_to_var(Prefix, Counter, Var) :-
    format(string(Var), "~s~|~`0t~d~2+", [Prefix, Counter]).

read_answer(VarPrefix, CircuitRBTree, Counter, Answer) :-
    int_to_var(VarPrefix, Counter, Var),
    (   rb_lookup(Var, Bit, CircuitRBTree)
    ->  NewCounter is Counter + 1,
        read_answer(CircuitRBTree, NewCounter, NewAnswer),
        Answer is 2 * NewAnswer + Bit
    ;   Answer = 0
    ).

read_answer(VarPrefix, CircuitRBTree, Answer) :-
    read_answer(VarPrefix, CircuitRBTree, 0, Answer).

sort_by_existence(_, [], []).
sort_by_existence(KnownVerticesRBTree, UnsortedOps, [Op | RemainingSorted]) :-
    append(Before, [Op | After], UnsortedOps),
    Op = op(Var1, _, Var2, Var3),
    rb_lookup(Var1, known, KnownVerticesRBTree),
    rb_lookup(Var2, known, KnownVerticesRBTree),
    !,
    append(Before, After, RemainingOps),
    rb_insert(KnownVerticesRBTree, Var3, known, NewKnownVerticesRBTree),
    sort_by_existence(NewKnownVerticesRBTree, RemainingOps, RemainingSorted).

process_circuit(InputFile, Result) :-
    read_file_to_string(InputFile, String, []),
    split_to_lines(String, Lines),
    maplist(parse_row, Lines, ParsedRows),
    partition([init(_, _)]>>true, ParsedRows, Inits, Ops),
    rb_empty(EmptyRBTree),
    foldl(
        [init(Var, Value), [Known, Circuit], [NewKnown, NewCircuit]] >>
            (   rb_insert(Known, Var, known, NewKnown),
                rb_insert(Circuit, Var, Value, NewCircuit)
            ),
        Inits,
        [EmptyRBTree, EmptyRBTree],
        [KnownVerticesRBTree, InitiallyFilledCircuitRBTree]
    ),
    sort_by_existence(KnownVerticesRBTree, Ops, SortedOps),
    foldl(calc_circuit, SortedOps, InitiallyFilledCircuitRBTree, FilledCircuitRBTree),
    read_answer("z", FilledCircuitRBTree, Result),
    !.

main :-
    process_circuit("inputs/day24a_test.txt", Result),
    writeln(Result).
    % process_circuit("inputs/day24a.txt", Result),
    % writeln(Result).

% day24b done by hand, see inputs/day24b.txt
