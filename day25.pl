:- [utils].

count(Char, Lists, Counts) :-
    (   Lists = [[] | _]
    ->  Counts = []
    ;   include({Char} / [[Char | _]] >> true, Lists, WithChar),
        length(WithChar, WithCharCount),
        maplist([[_ | X], X] >> true, Lists, ListsRests),
        count(Char, ListsRests, CountsRests),
        Counts = [WithCharCount | CountsRests]
    ).

parse_lock(Array, lock(Height, Heights)) :-
    append([[First], Inside, [Last]], Array),
    exclude(=('#'), First, []),
    include(=('#'), Last, []),
    length(Inside, Height),
    count('#', Inside, Heights).

parse_key(Array, key(Height, Heights)) :-
    append([[First], Inside, [Last]], Array),
    include(=('#'), First, []),
    exclude(=('#'), Last, []),
    length(Inside, Height),
    count('#', Inside, Heights).

every_other([], []).
every_other([X], [X]).
every_other([X, _ | Z], [X | RestZ]) :- every_other(Z, RestZ).

fit_lock_key(lock(Height, LockHeights), key(_, KeyHeights)) :-
    maplist(plus, LockHeights, KeyHeights, TotalHeights),
    include(<(Height), TotalHeights, []).

count_matching_pairs(InputFile, Result) :-
    read_file_to_string(InputFile, String, []),
    re_split("\n\n", String, Strings),
    every_other(Strings, ArrayStrings),
    maplist(split_to_lines, ArrayStrings, ArraysRows),
    maplist(maplist(string_chars), ArraysRows, Arrays),
    convlist(parse_lock, Arrays, Locks),
    convlist(parse_key, Arrays, Keys),
    findall(
        Lock-Key,
        (   member(Lock, Locks),
            member(Key, Keys),
            fit_lock_key(Lock, Key)
        ),
        Pairs
    ),
    length(Pairs, Result),
    !.

main :-
    % count_matching_pairs("inputs/day25a_test.txt", Result),
    % writeln(Result).
    count_matching_pairs("inputs/day25a.txt", Result),
    writeln(Result).

% There is no 25b
