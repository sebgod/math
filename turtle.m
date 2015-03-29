%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: turtle.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Jan 13 17:11:10 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% An implementation of turtle graphics in Mercury.
%----------------------------------------------------------------------------%

:- module turtle.

:- interface.

:- import_module bool.
:- import_module list.

%----------------------------------------------------------------------------%

:- type turtle
    --->    turtle.

:- type turtle_cmds == list(turtle_cmd).

:- type turtle_cmd
    --->    turn(angle)
    ;       move(length)
    ;       pen(drawing)
    ;       colour(colour).

:- type length  == float. % 0.0 .. 1.0
:- type angle   == float. % in radians
:- type drawing == bool.
:- type colour  ---> colour.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

% TODO: include/import/use modules

%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
:- end_module turtle.
%----------------------------------------------------------------------------%
