:- [utils].

safe_increasing([]).
safe_increasing([_ | []]).
safe_increasing([X | [Y | Rest]]) :-
    Low is X + 1,
    High is X + 3,
    between(Low, High, Y),
    safe_increasing([Y | Rest]).

safe_decreasing([]).
safe_decreasing([_ | []]).
safe_decreasing([X | [Y | Rest]]) :-
    Low is X - 3,
    High is X - 1,
    between(Low, High, Y),
    safe_decreasing([Y | Rest]).

safe_sequence(List) :- safe_increasing(List).
safe_sequence(List) :- safe_decreasing(List).

count_safe_sequences(InputFile, Result) :-
    get_raw_number_lists(InputFile, Reports),
    include(safe_sequence, Reports, SafeReports),
    length(SafeReports, Result).

remove_single_element(List, Rest) :-
    append(Left, [_ | Right], List),
    append(Left, Right, Rest).

almost_safe_sequence(List) :-
    remove_single_element(List, SubList),
    safe_sequence(SubList).

count_almost_safe_sequences(InputFile, Result) :-
    get_raw_number_lists(InputFile, Reports),
    include(almost_safe_sequence, Reports, SafeReports),
    length(SafeReports, Result).

main :-
    % count_safe_sequences("inputs/day02a_test.txt", SafeCount),
    % writeln(SafeCount).
    % count_safe_sequences("inputs/day02a.txt", SafeCount),
    % writeln(SafeCount).
    % count_almost_safe_sequences("inputs/day02a_test.txt", AlmostSafeCount),
    % writeln(AlmostSafeCount).
    count_almost_safe_sequences("inputs/day02a.txt", AlmostSafeCount),
    writeln(AlmostSafeCount). 
