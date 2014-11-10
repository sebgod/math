%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: test_roman.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon 10 Nov 17:50:07 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Testing roman numerals from the roman module
%----------------------------------------------------------------------------%

:- module test_roman.

:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module roman.
:- import_module string.

%----------------------------------------------------------------------------%

main(!IO) :-
    command_line_arguments(Args, !IO),
    filter(is_all_digits, Args, CleanArgs),
    foldl((pred(Arg::in, !.IO::di, !:IO::uo) is det :-
                ( Roman = to_roman(Arg) ->
                    format("%s => %s\n", [s(Arg), s(Roman)], !IO)
                ;
                    format("%s cannot be converted.\n", [s(Arg)], !IO)
                )
          ),
          CleanArgs,
          !IO).

%----------------------------------------------------------------------------%
:- end_module test_roman.
%----------------------------------------------------------------------------%
