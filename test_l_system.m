%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: test_lsystem.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Fri Jan  9 11:19:47 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Testing the L-system module.
%----------------------------------------------------------------------------%

:- module test_l_system.

:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module l_system.

:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

main(!IO) :-
    OrigGrammar =
        grammar(['A', 'B'], [], ['A'],
            (func(V) =
                ( V = 'A' -> ['A', 'B']
                ; V = 'B' -> ['A']
                ; unexpected($file, $pred, "unknown variable")
                )
            )
        ),
    Symbols = produce_next(OrigGrammar),
    foldl(io.print, Symbols, !IO).

%----------------------------------------------------------------------------%
:- end_module test_l_system.
%----------------------------------------------------------------------------%
