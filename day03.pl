:- [utils].

parse_and_mul(Dict, InitResult, FinalResult) :-
    X = Dict.1,
    Y = Dict.2,
    atom_number(X, NumX),
    atom_number(Y, NumY),
    FinalResult is InitResult + NumX * NumY.

process_sequence(InputFile, Result) :-
    read_file_to_string(InputFile, String, []),
    re_foldl(parse_and_mul, "mul\\((\\d+),(\\d+)\\)", String, 0, Result, []).

parse_and_maybe_mul(Dict, InitResult-InitMul, FinalResult-FinalMul) :-
    (   Dict.0 = "do()"
    ->  FinalResult = InitResult,
        FinalMul = 1
    ;   Dict.0 = "don't()"
    ->  FinalResult = InitResult,
        FinalMul = 0
    ;   X = Dict.2,
        Y = Dict.3,
        atom_number(X, NumX),
        atom_number(Y, NumY),
        FinalMul = InitMul,
        FinalResult is InitResult + NumX * NumY * InitMul
    ).

process_sequence_2(InputFile, Result) :-
    read_file_to_string(InputFile, String, []),
    re_foldl(parse_and_maybe_mul, "(mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\))", String, 0-1, Result-_, []).

main :-
    % process_sequence("inputs/day03a_test.txt", MulResult),
    % writeln(MulResult).
    % process_sequence("inputs/day03a.txt", MulResult),
    % writeln(MulResult).
    % process_sequence_2("inputs/day03b_test.txt", MulResult2),
    % writeln(MulResult2).
    process_sequence_2("inputs/day03a.txt", MulResult2),
    writeln(MulResult2).
