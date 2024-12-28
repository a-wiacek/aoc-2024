:- [utils].

:- table drop/3.

drop(List, 0, List).
drop(List, N, NewList) :-
    N > 0,
    M is N - 1,
    drop(List, M, [_ | NewList]).

combo(Arg, _, Arg) :- between(0, 3, Arg).
combo(4, [A, _, _], A).
combo(5, [_, B, _], B).
combo(6, [_, _, C], C).

interpret_instruction(Op, Arg, [A, B, C, Ptr, RevOutput], [NewA, NewB, NewC, NewPtr, NewRevOutput]) :-
    LikelyNewRevOutput = RevOutput,
    LikelyNewPtr is Ptr + 2,
    (   Op = 0 % adv
    ->  combo(Arg, [A, B, C], Combo),
        Den is 2^Combo,
        NewA is A div Den,
        [NewB, NewC, NewPtr, NewRevOutput] = [B, C, LikelyNewPtr, LikelyNewRevOutput]
    ;   Op = 1 % bxl
    ->  NewB is B xor Arg,
        [NewA, NewC, NewPtr, NewRevOutput] = [A, C, LikelyNewPtr, LikelyNewRevOutput]
    ;   Op = 2 % bst
    ->  combo(Arg, [A, B, C], Combo),
        NewB is Combo mod 8,
        [NewA, NewC, NewPtr, NewRevOutput] = [A, C, LikelyNewPtr, LikelyNewRevOutput]
    ;   Op = 3 % jnz
    ->  [NewA, NewB, NewC, NewRevOutput] = [A, B, C, LikelyNewRevOutput],
        (   A = 0
        ->  NewPtr = LikelyNewPtr
        ;   NewPtr = Arg
        )
    ;   Op = 4 % bxc
    ->  NewB is B xor C,
        [NewA, NewC, NewPtr, NewRevOutput] = [A, C, LikelyNewPtr, LikelyNewRevOutput]
    ;   Op = 5 % out
    ->  combo(Arg, [A, B, C], Combo),
        NewOutput is Combo mod 8,
        [NewA, NewB, NewC, NewPtr, NewRevOutput] = [A, B, C, LikelyNewPtr, [NewOutput | LikelyNewRevOutput]]
    ;   Op = 6 % bdv
    ->  combo(Arg, [A, B, C], Combo),
        Den is 2^Combo,
        NewB is A div Den,
        [NewA, NewC, NewPtr, NewRevOutput] = [A, C, LikelyNewPtr, LikelyNewRevOutput]
    ;   Op = 7 % cdv
    ->  combo(Arg, [A, B, C], Combo),
        Den is 2^Combo,
        NewC is A div Den,
        [NewA, NewB, NewPtr, NewRevOutput] = [A, B, LikelyNewPtr, LikelyNewRevOutput]
    ).

interpreter_step(Program, InitState, FinalState) :-
    [_, _, _, Ptr, _] = InitState,
    drop(Program, Ptr, [Op, Arg | _]),
    interpret_instruction(Op, Arg, InitState, FinalState).

interpret_program(Program, [A, B, C], FinalState) :-
    iterate1(interpreter_step(Program), [A, B, C, 0, []], FinalState).

get_output([_, _, _, _, RevOutput], OutputStr) :-
    reverse(RevOutput, Output),
    atomics_to_string(Output, ",", OutputStr).

parse_register(Row, X) :-
    re_matchsub("Register .: (-?\\d+)", Row, ParsedRow),
    _{1:XAtom} :< ParsedRow,
    atom_number(XAtom, X).

parse_program(Row, Program) :-
    re_matchsub("Program: (.*)", Row, ParsedRow),
    _{1:ProgramAtom} :< ParsedRow,
    atomics_to_string(Atoms, ",", ProgramAtom),
    maplist(atom_number, Atoms, Program).

simulate(InputFile, Result) :-
    read_file_to_string(InputFile, Input, []),
    split_to_lines(Input, [ALine, BLine, CLine, ProgramLine]),
    parse_register(ALine, A),
    parse_register(BLine, B),
    parse_register(CLine, C),
    parse_program(ProgramLine, Program),
    interpret_program(Program, [A, B, C], FinalState),
    get_output(FinalState, Result).

main :-
    % simulate("inputs/day17a_test.txt", Result),
    % writeln(Result).
    simulate("inputs/day17a.txt", Result),
    writeln(Result).
