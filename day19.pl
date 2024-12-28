:- [utils].

parse_input(InputFile, Patterns, Designs) :-
    read_file_to_string(InputFile, String, []),
    split_to_lines(String, [AllPatterns | DesignsStrings]),
    split_string(AllPatterns, ",", " ", SplitPatterns),
    maplist(string_chars, SplitPatterns, PatternsUnsorted),
    sort(PatternsUnsorted, Patterns),
    maplist(string_chars, DesignsStrings, Designs).

:- table valid_design/3.

valid_design(_, [], 1).
valid_design(Patterns, Design, Count) :-
    convlist({Design}/[Pattern, Rest]>>append(Pattern, Rest, Design), Patterns, Rests),
    convlist(valid_design(Patterns), Rests, Counts),
    sum_list(Counts, Count).

count_length(InputFile, Result) :-
    parse_input(InputFile, Patterns, Designs),
    maplist(valid_design(Patterns), Designs, CountDesigns),
    include(<(0), CountDesigns, ValidDesigns),
    length(ValidDesigns, Result).

count_total(InputFile, Result) :-
    parse_input(InputFile, Patterns, Designs),
    maplist(valid_design(Patterns), Designs, CountDesigns),
    sum_list(CountDesigns, Result).

main :-
    % count_length("inputs/day19a_test.txt", Result),
    % writeln(Result).
    % count_length("inputs/day19a.txt", Result),
    % writeln(Result).
    % count_total("inputs/day19a_test.txt", Result),
    % writeln(Result).
    count_total("inputs/day19a.txt", Result),
    writeln(Result).
