% Numbers are interpreted as bit arrays, where head bit is the least significant.

% bit(0).
% bit(1).

% Empty list is valid representation of 0.
% It is not enforced that the last element of the list is 1.
% Unused, written only for demonstrations.
% Backtracking all bitnums for fixed length gives numbers in ascending order.
% bitnum(Length, Bitnum) :-
%     (   Length = 0
%     ->  Bitnum = []
%     ;   NewLength is Length - 1,
%         bitnum(NewLength, TailN),
%         bit(HeadN),
%         Bitnum = [HeadN | TailN]
%     ).

bitnum_to_number(Bitnum, Number) :-
    reverse(Bitnum, RevBitnum),
    foldl([Bit, Accum, NewAccum]>>(NewAccum is 2 * Accum + Bit), RevBitnum, 0, Number).

number_to_3_bit_bitnum(0, [0, 0, 0]).
number_to_3_bit_bitnum(1, [1, 0, 0]).
number_to_3_bit_bitnum(2, [0, 1, 0]).
number_to_3_bit_bitnum(3, [1, 1, 0]).
number_to_3_bit_bitnum(4, [0, 0, 1]).
number_to_3_bit_bitnum(5, [1, 0, 1]).
number_to_3_bit_bitnum(6, [0, 1, 1]).
number_to_3_bit_bitnum(7, [1, 1, 1]).

% Unit list representation is list of length N with values ignored.
% Unused, written only for demonstration.
% unit_list(N, List) :-
%     (   N = 0
%     ->  List = []
%     ;   NewN is N - 1,
%         unit_list(NewN, Tail),
%         List = [x | Tail]
%     ).

% Number modulo 2^n is only n first bits.
bit_mod([], _, []).
bit_mod(_, [], []).
bit_mod([HeadBitnum | TailBitnum], [_ | NewModPowerNumberUnitList], [HeadBitnum | TailBitnumMod]) :-
    bit_mod(TailBitnum, NewModPowerNumberUnitList, TailBitnumMod).

% Number div 2^n is all but n first bits.
bit_div([], _, []).
bit_div(Bitnum, [], Bitnum).
bit_div([_ | Bitnum], [_ | NewDivPowerNumberUnitList], BitnumDiv) :-
    bit_div(Bitnum, NewDivPowerNumberUnitList, BitnumDiv).

bxor(0, 0, 0).
bxor(0, 1, 1).
bxor(1, 0, 1).
bxor(1, 1, 0).
% Bitwise xor result array has length equal to maximal length of arguments.
bitwise_xor([], X, X).
bitwise_xor(X, [], X).
bitwise_xor([HA | TA], [HB | TB], [HX | TX]) :-
    bxor(HA, HB, HX),
    bitwise_xor(TA, TB, TX).

bitnum_to_unit_number_match([0, 0, 0], []).
bitnum_to_unit_number_match([1, 0, 0], [_]).
bitnum_to_unit_number_match([0, 1, 0], [_, _]).
bitnum_to_unit_number_match([1, 1, 0], [_, _, _]).
bitnum_to_unit_number_match([0, 0, 1], [_, _, _, _]).
bitnum_to_unit_number_match([1, 0, 1], [_, _, _, _, _]).
bitnum_to_unit_number_match([0, 1, 1], [_, _, _, _, _, _]).
bitnum_to_unit_number_match([1, 1, 1], [_, _, _, _, _, _, _]).

shift(Bitnum, BitnumShiftBy, BitnumShift) :-
    bitnum_to_unit_number_match(BitnumShiftBy, ShiftBy),
    bit_div(Bitnum, ShiftBy, BitnumShift).

% Whenever program 2,4,1,1,7,5,1,5,4,1,5,5,0,3,3,0 loops,
% and at the start of the loop it has register state [A, B, C],
% after one loop it has state [NewA, NewB, NewC]
% and outputs NewB mod 8.
program_loop([A, _, _], [NewA, NewB, NewC]) :-
    bit_div(A, [_, _, _], NewA),
    bit_mod(A, [_, _, _], ModA),
    bitwise_xor(ModA, [1, 0, 0], BitnumShiftBy),
    shift(A, BitnumShiftBy, NewC),
    bitwise_xor([0, 0, 1], ModA, X),
    bitwise_xor(X, NewC, NewB).

verify_program_output(BitnumOutput, FinalState, State) :-
    program_loop(State, FinalState),
    FinalState = [_, B, _],
    bit_mod(B, [_, _, _], BitnumOutput).

guess(ANumber) :-
    Program = [2,4,1,1,7,5,1,5,4,1,5,5,0,3,3,0],
    maplist(number_to_3_bit_bitnum, Program, ProgramBitForm),
    reverse(ProgramBitForm, ProgramBitFormReverse),
    foldl(verify_program_output, ProgramBitFormReverse, [[0, 0, 0], _, _], [A, _, _]),
    bitnum_to_number(A, ANumber),
    writeln(ANumber).

main :-
    findall(X, guess(X), Xs),
    writeln(Xs).

% 164278764924605
% 164278764924861
% not sure if there are any other
