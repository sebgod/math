%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: roman.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon 10 Nov 17:51:53 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Module for computing roman numeras
%----------------------------------------------------------------------------%

:- module roman.

:- interface.

:- import_module string.

%----------------------------------------------------------------------------%

:- func to_roman(string::in) = (string::out) is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.

%----------------------------------------------------------------------------%

to_roman(Number) = from_char_list(build_roman(reverse(to_char_list(Number)))).

:- func build_roman(list(char)) = list(char).
:- mode build_roman(in)         = out is semidet.
build_roman([]) = [].
build_roman([D|R]) = Roman :-
    map(promote, build_roman(R), Interim),
    Roman = Interim ++ digit_to_roman(D).

:- func digit_to_roman(char) = list(char).
:- mode digit_to_roman(in)   = out is semidet.
digit_to_roman('0') = [].
digit_to_roman('1') = ['I'].
digit_to_roman('2') = ['I','I'].
digit_to_roman('3') = ['I','I','I'].
digit_to_roman('4') = ['I','V'].
digit_to_roman('5') = ['V'].
digit_to_roman('6') = ['V','I'].
digit_to_roman('7') = ['V','I','I'].
digit_to_roman('8') = ['V','I','I','I'].
digit_to_roman('9') = ['I','X'].

:- pred promote(char::in, char::out) is semidet.
promote('I', 'X').
promote('V', 'L').
promote('X', 'C').
promote('L', 'D').
promote('C', 'M').

%----------------------------------------------------------------------------%
:- end_module roman.
%----------------------------------------------------------------------------%
