:- [utils].

:- table split_stone_length/3.

split_stone_length(Times, Stone, Length) :-
    NewTimes is Times - 1,
    (   Times = 0
    ->  Length = 1
    ;   Stone = 0
    ->  split_stone_length(NewTimes, 1, Length)
    ;   atom_number(AtomStone, Stone),
        string_length(AtomStone, AtomLength),
        mod(AtomLength, 2) =:= 0
    ->  string_chars(AtomStone, Digits),
        append(LeftDigits, RightDigits, Digits),
        HalfLength is AtomLength / 2,
        length(LeftDigits, HalfLength),
        string_chars(LeftAtomStone, LeftDigits),
        atom_number(LeftAtomStone, LeftStone),
        string_chars(RightAtomStone, RightDigits),
        atom_number(RightAtomStone, RightStone),
        split_stone_length(NewTimes, LeftStone, LeftLength),
        split_stone_length(NewTimes, RightStone, RightLength),
        Length is LeftLength + RightLength
    ;   NewStone is Stone * 2024,
        split_stone_length(NewTimes, NewStone, Length)
    ).

simulate_stones(InputFile, Times, Result) :-
    get_raw_number_lists(InputFile, [Stones]),
    maplist(split_stone_length(Times), Stones, Lengths),
    sumlist(Lengths, Result).

main :-
    % simulate_stones("inputs/day11a_test.txt", 6, Result),
    % writeln(Result).
    % simulate_stones("inputs/day11a.txt", 25, Result),
    % writeln(Result).
    simulate_stones("inputs/day11a.txt", 75, Result2),
    writeln(Result2).
