:- use_module(library(clpfd)).
:- [utils].

diff(X, Y, Z) :-
    Z is abs(X - Y).

get_lists(InputFile, IDList1, IDList2) :-
    get_raw_number_lists(InputFile, SplitInputLines),
    transpose(SplitInputLines, IDLists),
    maplist(sort(0, @=<), IDLists, SortedIDLists),
    [IDList1, IDList2] = SortedIDLists.

distance(InputFile, Result) :-
    get_lists(InputFile, IDList1, IDList2),
    maplist(diff, IDList1, IDList2, DiffList),
    sum_list(DiffList, Result).

sim(LFreqs, RFreqs, InitResult, FinalResult) :-
    (   LFreqs = []
    ->  InitResult = FinalResult
    ;   RFreqs = []
    ->  InitResult = FinalResult
    ;   LFreqs = [L-LFreq | LRest],
        RFreqs = [R-RFreq | RRest],
        (   L = R
        ->  IntermediateResult is InitResult + L * LFreq * RFreq,
            sim(LRest, RRest, IntermediateResult, FinalResult)
        ;   L < R
        ->  sim(LRest, [R-RFreq | RRest], InitResult, FinalResult)
        ;   L > R
        ->  sim([L-LFreq | LRest], RRest, InitResult, FinalResult)
        )
    ).

similarity(InputFile, Result) :-
    get_lists(InputFile, IDList1, IDList2),
    clumped(IDList1, Freq1List),
    clumped(IDList2, Freq2List),
    sim(Freq1List, Freq2List, 0, Result).

main :-
    % distance("inputs/day01a_test.txt", Distance),
    % writeln(Distance).
    % distance("inputs/day01a.txt", Distance),
    % writeln(Distance).
    % similarity("inputs/day01a_test.txt", Similarity),
    % writeln(Similarity).
    similarity("inputs/day01a.txt", Similarity),
    writeln(Similarity). 
