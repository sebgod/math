%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: integer_math.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon 10 Nov 18:16:44 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module integer_math.

:- interface.

:- import_module integer.
:- import_module io.
:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

:- pred eval_fun(string, func(integer) = integer, io, io).
:- mode eval_fun(in, func(in) = out is det, di, uo) is det.

:- pred eval_fun2(string, func(integer, integer) = integer, io, io).
:- mode eval_fun2(in, func(in, in) = out is det, di, uo) is det.

:- func format_integer(integer) = string.poly_type.

:- func arg_to_integer(list(string), int) = integer.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module generic_poly.
:- import_module std_util.

%----------------------------------------------------------------------------%

eval_fun(Name, Fun, !IO) :-
    command_line_arguments(Args, !IO),
    Number = arg_to_integer(Args, 0),
    Result = Fun(Number),
    io.format("%s(%s) = %s\n",
        [s(Name)] ++ list.map(format_integer, [Number, Result]), !IO).

eval_fun2(Name, Fun, !IO) :-
    command_line_arguments(Args, !IO),
    Arg0 = arg_to_integer(Args, 0),
    Arg1 = arg_to_integer(Args, 1),
    Result = Fun(Arg0, Arg1),
    io.format("%s(%s, %s) = %s\n",
        [s(Name)] ++ list.map(format_integer, [Arg0, Arg1, Result]), !IO).

format_integer(Integer) =
    (to_poly_type `compose` integer.to_string)(Integer).

arg_to_integer(Args, Index) =
    (integer.det_from_string `compose` list.det_index0(Args))(Index).

%----------------------------------------------------------------------------%
:- end_module integer_math.
%----------------------------------------------------------------------------%
