%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: l_system.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Fri Jan  9 11:19:44 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% An implementation of L-systems (Lindenmayer systems).
% c.f. https://en.wikipedia.org/wiki/L-system
%----------------------------------------------------------------------------%

:- module l_system.

:- interface.

:- import_module list.

%----------------------------------------------------------------------------%

:- type grammar(A)
    ---> grammar(
            grammar_vars    ::  list(A),
            grammar_consts  ::  list(A),
            grammar_axiom   ::  list(A),
            grammar_rules   ::  rules(A)
        ).

:- inst grammar(V, C)
    ---> grammar(
            list(V),
            list(C),
            symbols,
            rules(V)
        ).

:- type rules(A) == (func(A) = list(A)).

:- inst rules(V) == (func(in(V)) = out(symbols) is det).

:- inst symbols == list(ground).

:- func produce_next(grammar(A)) = list(A).
:- mode produce_next(in(grammar(V, C))) = out(symbols) is det.

:- func produce_next(grammar(A), list(A)) = list(A).
:- mode produce_next(in(grammar(V, C)), in(symbols)) = out(symbols) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

produce_next(G) = G ^ grammar_axiom.

produce_next(G, L) = condense(map(G ^ grammar_rules, L)).

%----------------------------------------------------------------------------%
:- end_module l_system.
%----------------------------------------------------------------------------%
