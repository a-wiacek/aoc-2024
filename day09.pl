:- [utils].

parse_file(InputFile, Numbers) :-
    read_file_to_string(InputFile, File, []),
    split_string(File, "\n", "\n", [String]),
    string_chars(String, Chars),
    maplist(atom_number, Chars, Numbers).

parse_block(Length, [ParsedBlocks, Now-Then], [[NewBlock | ParsedBlocks], Then-Then2]) :-
    (   number(Now)
    ->  NewBlock = Now-Length-notMoved,
        Then2 is Now + 1
    ;   Now = '.'
    ->  NewBlock = '.'-Length,
        Then2 = '.'
    ).

line_to_disk_structure(BlockList, DiskStructure) :-
    foldl(parse_block, BlockList, [[], 0-'.'], [ReverseDiskStructure, _]),
    reverse(ReverseDiskStructure, DiskStructure).

% moved tag is ignored here
fill_hole(List, ListWithOneHoleFilled) :-
    append(ListWithoutHoles, ['.'-HoleLength | RestOfDisk], List),
    \+ member(['.'-_], ListWithoutHoles),
    (   RestOfDisk = [FileID-FileLength-notMoved] % last hole to fill
    ->  append(ListWithoutHoles, [FileID-FileLength-moved], ListWithOneHoleFilled)
    ;   append(AfterFirstHole, [LastHole, FileID-FileLength-notMoved], RestOfDisk),
        (   HoleLength < FileLength
        ->  RemainingFileLength is FileLength - HoleLength,
            append([ListWithoutHoles,
                    [FileID-HoleLength-moved],
                    AfterFirstHole,
                    [LastHole, FileID-RemainingFileLength-notMoved]],
                   ListWithOneHoleFilled)
        ;   HoleLength = FileLength
        ->  append([ListWithoutHoles,
                    [FileID-HoleLength-moved],
                    AfterFirstHole],
                   ListWithOneHoleFilled)
        ;   HoleLength > FileLength
        ->  RemainingHoleLength is HoleLength - FileLength,
            append([ListWithoutHoles,
                    [FileID-FileLength-moved, '.'-RemainingHoleLength],
                    AfterFirstHole],
                   ListWithOneHoleFilled)
        )
    ).

inc_up_to(Max, N, IncN) :-
    IncN is N + 1,
    IncN < Max.

mul(X, Y, Z) :-
    (   number(Y)
    ->  Z is X * Y
    ;   Z = 0
    ).

zip_product(Xs, Ys, Result) :-
    maplist(mul, Xs, Ys, Zs),
    sum_list(Zs, Result).

replicate3(Elem-Times-_, List) :-
    replicate(Elem, Times, List).
replicate3(Elem-Times, List) :-
    replicate(Elem, Times, List).

unpack_and_fill_holes(HoleStepGoal, List, Numbers) :-
    line_to_disk_structure(List, DiskStructure),
    iterate1(HoleStepGoal, DiskStructure, DenseDiskStructure),
    maplist(replicate3, DenseDiskStructure, UnpackedDiskStructure),
    append(UnpackedDiskStructure, Numbers).

checksum(InputFile, Result) :-
    parse_file(InputFile, PackedNumbers),
    unpack_and_fill_holes(fill_hole, PackedNumbers, Numbers),
    length(Numbers, Length),
    iterate_all(inc_up_to(Length), 0, Xs),
    zip_product(Xs, Numbers, Result).

move_file(List, ListWithOneHoleFilled) :-
    append(LeftBlock, [FileID-FileLength-notMoved | RestOfDisk], List),
    \+ member(_-_-notMoved, RestOfDisk),
    (   append(LeftLeftBlock, ['.'-HoleLength | LeftRightBlock], LeftBlock),
        HoleLength >= FileLength,
        \+ (member('.'-AnotherHoleLength, LeftLeftBlock),
            AnotherHoleLength >= FileLength)
    ->  RemainingHoleLength is HoleLength - FileLength,
        append(LeftLeftBlock, [FileID-FileLength-moved, '.'-RemainingHoleLength | LeftRightBlock], NewLeftBlock),
        append(NewLeftBlock, ['.'-FileLength | RestOfDisk], ListWithOneHoleFilled)
    ;   append(LeftBlock, [FileID-FileLength-moved | RestOfDisk], ListWithOneHoleFilled)
    ).

checksum2(InputFile, Result) :-
    parse_file(InputFile, PackedNumbers),
    unpack_and_fill_holes(move_file, PackedNumbers, Numbers),
    length(Numbers, Length),
    iterate_all(inc_up_to(Length), 0, Xs),
    zip_product(Xs, Numbers, Result).

main :-
    % checksum("inputs/day09a_test.txt", Result),
    % writeln(Result).
    % checksum("inputs/day09a.txt", Result),
    % writeln(Result).
    % checksum2("inputs/day09a_test.txt", Result2),
    % writeln(Result2).
    checksum2("inputs/day09a.txt", Result2),
    writeln(Result2).
