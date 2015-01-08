%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: factorial.m
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: 2014-02-27
% Stability: high
%----------------------------------------------------------------------------%

:- module factorial.

:- interface.

:- import_module integer.

%----------------------------------------------------------------------------%

:- func factorial(integer) = integer.

%----------------------------------------------------------------------------%

:- implementation.

:- pragma memo(factorial/1).

factorial(N) =
    (   N =< integer(0)
    ->  integer(1)
    ;   factorial(N - integer(1)) * N
    ).

%----------------------------------------------------------------------------%
:- end_module factorial.
%----------------------------------------------------------------------------%
