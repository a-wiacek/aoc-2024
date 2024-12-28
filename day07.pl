:- [utils].

my_eval(plus, X, Y, R) :- R is X + Y.
my_eval(times, X, Y, R) :- R is X * Y.
my_eval(concat, X, Y, R) :-
    atom_number(AtomX, X),
    atom_number(AtomY, Y),
    atom_concat(AtomX, AtomY, AtomR),
    atom_number(AtomR, R).

eval_list(_, [X], X).
eval_list(Ops, [X, Y | T], Result) :-
    member(Op, Ops),
    my_eval(Op, X, Y, Z),
    eval_list(Ops, [Z | T], Result).

is_calibrated([Result, Parts]) :-
    eval_list([plus, times], Parts, Result).
is_new_calibrated([Result, Parts]) :-
    eval_list([plus, times, concat], Parts, Result).

parse_line(String, [Result, Parts]) :-
    split_string(String, ":", "", [Before, After]),
    atom_number(Before, Result),
    split_string(After, " ", " ", UnparsedParts),
    maplist(atom_number, UnparsedParts, Parts).

parse_file(String, Lines) :-
    split_to_lines(String, UnparsedLines),
    maplist(parse_line, UnparsedLines, Lines).

calibrate(InputFile, Result) :-
    read_file_to_string(InputFile, Input, []),
    parse_file(Input, Lines),
    include(is_calibrated, Lines, CalibrableLines),
    maplist(head, CalibrableLines, Values),
    sumlist(Values, Result).

new_calibrate(InputFile, Result) :-
    read_file_to_string(InputFile, Input, []),
    parse_file(Input, Lines),
    include(is_new_calibrated, Lines, CalibrableLines),
    maplist(head, CalibrableLines, Values),
    sumlist(Values, Result).
            
main :-
    % calibrate("inputs/day07a_test.txt", Result),
    % writeln(Result).
    % calibrate("inputs/day07a.txt", Result),
    % writeln(Result).
    % new_calibrate("inputs/day07a_test.txt", Result2),
    % writeln(Result2).
    new_calibrate("inputs/day07a.txt", Result2),
    writeln(Result2).
