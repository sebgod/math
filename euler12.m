%------------------------------------------------------------------------------%
% File: euler12.m
% Main author:  Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Fri May  2 23:13:48 WEST 2014
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%
%------------------------------------------------------------------------------%

:- module euler12.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module int, math.

%------------------------------------------------------------------------------%

:- func factorCount(int) = int.

factorCount(N) = Count :-
    Sqrt = math.sqrt(N).

main(!IO) :-
    

%------------------------------------------------------------------------------%
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%------------------------------------------------------------------------------%
