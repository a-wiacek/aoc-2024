:- [utils].

pairs([H, T], [[H, T]]).
pairs([H, T | Rest], [[H, T] | PairsRest]) :-
    pairs([T | Rest], PairsRest).

between2(Bound1, Bound2, Value) :-
    (   Bound1 < Bound2
    ->  between(Bound1, Bound2, Value)
    ;   between(Bound2, Bound1, Value)
    ).

arrow_path(Board, CurrentX-CurrentY, FinalX-FinalY, Steps) :-
    (   FinalX < CurrentX
    ->  StepsX is CurrentX - FinalX,
        replicate('<', StepsX, HorizontalSteps)
    ;   StepsX is FinalX - CurrentX, % may be 0
        replicate('>', StepsX, HorizontalSteps)
    ),
    (   FinalY < CurrentY
    ->  StepsY is CurrentY - FinalY,
        replicate('^', StepsY, VerticalSteps)
    ;   StepsY is FinalY - CurrentY, % may be 0
        replicate('v', StepsY, VerticalSteps)
    ),
    get_letter(Board, EmptyX-EmptyY, '*'),
    (   VerticalSteps = []
    ->  Steps = HorizontalSteps
    ;   HorizontalSteps = []
    ->  Steps = VerticalSteps
    ;   (   EmptyX = CurrentX,
            between2(CurrentY, FinalY, EmptyY)
        ;   EmptyY = FinalY,
            between2(CurrentX, FinalX, EmptyX)
        )
    ->  append(HorizontalSteps, VerticalSteps, Steps)
    ;   (   EmptyY = CurrentY,
            between2(CurrentX, FinalX, EmptyX)
        ;   EmptyX = FinalX,
            between2(CurrentY, FinalY, EmptyY)
        )
    ->  append(VerticalSteps, HorizontalSteps, Steps)
    ;   % either way is fine, try both
        permutation([HorizontalSteps, VerticalSteps], [FirstSteps, SecondSteps]),
        append(FirstSteps, SecondSteps, Steps)
    ).

:- table key_sequence_length/4.

key_sequence_length(GetBoard, Depth, [From, To], Length) :-
    (   Depth = 0
    ->  Length = 1
    ;   call(GetBoard, Depth, Board),
        get_letter(Board, PointFrom, From),
        get_letter(Board, PointTo, To),
        findall(
            CaseLength,
            (   arrow_path(Board, PointFrom, PointTo, Steps),
                append([['A'], Steps, ['A']], StepsWithA),
                pairs(StepsWithA, PairsNextLevel),
                NewDepth is Depth - 1,
                maplist(key_sequence_length(GetBoard, NewDepth), PairsNextLevel, ThisCaseLengths),
                sum_list(ThisCaseLengths, CaseLength)
            ),
            CaseLengths
        ),
        min_list(CaseLengths, Length)
    ).

unwrap_layers(Sequence, LayersCount, Length) :-
    NumericKeyboard = [['7', '8', '9'],
                       ['4', '5', '6'],
                       ['1', '2', '3'],
                       ['*', '0', 'A']],
    ArrowKeyboard = [['*', '^', 'A'],
                     ['<', 'v', '>']],
    append(['A'], Sequence, FixedSequence),
    pairs(FixedSequence, FixedSequenceOfPairs),
    GetBoard = {NumericKeyboard, ArrowKeyboard}
             / [Layer, Board]
            >> (   Layer = LayersCount
               ->  Board = NumericKeyboard
               ;   Board = ArrowKeyboard
               ),
    maplist(key_sequence_length(GetBoard, LayersCount), FixedSequenceOfPairs, Lengths),
    sum_list(Lengths, Length).

path_complexity(Layers, SequenceString, Result) :-
    string_chars(SequenceString, Sequence),
    unwrap_layers(Sequence, Layers, Length),
    re_matchsub("(\\d+)", SequenceString, ParsedRow),
    _{1:NumberString} :< ParsedRow,
    atom_number(NumberString, Number),
    Result is Number * Length.

solve(InputFile, Layers, Result) :-
    read_file_to_string(InputFile, String, []),
    split_to_lines(String, Lines),
    maplist(path_complexity(Layers), Lines, Results),
    sum_list(Results, Result),
    !.

main :-
    % solve("inputs/day21a_test.txt", 3, Result),
    % writeln(Result).
    % solve("inputs/day21a.txt", 3, Result),
    % writeln(Result).
    % solve("inputs/day21a_test.txt", 26, Result),
    % writeln(Result).
    solve("inputs/day21a.txt", 26, Result),
    writeln(Result).
