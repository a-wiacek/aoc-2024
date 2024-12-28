:- use_module(library(clpfd)).
:- [utils].

parse_rule(Rule, NumberBefore | NumberAfter) :-
    split_string(Rule, "|", "", [Before, After]),
    atom_number(Before, NumberBefore),
    atom_number(After, NumberAfter).

parse_order(UnparsedOrder, Order) :-
    split_string(UnparsedOrder, ",", "", SplitOrder),
    maplist(atom_number, SplitOrder, Order).

invalid_list(Rules, PageOrder) :-
    member(Y | X, Rules),
    append(_, [X | Rest], PageOrder),
    member(Y, Rest).

get_middle(List, Elem) :-
    length(List, Length),
    Index is (Length - 1) / 2,
    nth0(Index, List, Elem).

parse_input(InputFile, Rules, Orders) :-
    read_file_to_string(InputFile, Input, []),
    split_string(Input, "\n", "", InputLines),
    append(UnparsedRules, ["" | UnparsedOrdersN], InputLines),
    append(UnparsedOrders, [""], UnparsedOrdersN),
    maplist(parse_rule, UnparsedRules, Rules),
    maplist(parse_order, UnparsedOrders, Orders).

valid_sequences(InputFile, Result) :-
    parse_input(InputFile, Rules, Orders),
    exclude(invalid_list(Rules), Orders, ValidOrders),
    maplist(get_middle, ValidOrders, PartialResults),
    sum_list(PartialResults, Result).

rule_match(X, _ | X).
rule_match(X, X | _).

relevant_rule(List, X | Y) :-
    member(X, List),
    member(Y, List).

reordered_sequence(_, [], []).
reordered_sequence(Rules, UnsortedList, SortedList) :-
    append(LeftSplit, [X | RightSplit], UnsortedList),
    \+ member(_ | X, Rules),
    append(LeftSplit, RightSplit, UnsortedRemainder),
    exclude(rule_match(X), Rules, ShorterRules),
    reordered_sequence(ShorterRules, UnsortedRemainder, SortedRemainder),
    SortedList = [X | SortedRemainder].

reordered_sequence2(AllRules, UnsortedList, SortedList) :-
    include(relevant_rule(UnsortedList), AllRules, Rules),
    reordered_sequence(Rules, UnsortedList, SortedList).

invalid_sequences(InputFile, Result) :-
    parse_input(InputFile, Rules, Orders),
    include(invalid_list(Rules), Orders, InvalidOrders),
    maplist(reordered_sequence2(Rules), InvalidOrders, ValidOrders),
    maplist(get_middle, ValidOrders, PartialResults),
    sum_list(PartialResults, Result).

main :-
    % valid_sequences("inputs/day05a_test.txt", Result),
    % writeln(Result).
    % valid_sequences("inputs/day05a.txt", Result),
    % writeln(Result).
    % invalid_sequences("inputs/day05a_test.txt", Result2),
    % writeln(Result2).
    invalid_sequences("inputs/day05a.txt", Result2),
    writeln(Result2).

    

