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
            result,
            rules(V)
        ).

:- type rules(A) == (func(A) = list(A)).

:- inst rules(V) == (func(in(V)) = out(result) is det).

:- type results(A) == list(result(A)).

:- type result(A) == list(A).

:- inst results == list(result).
:- inst result == list(ground).

:- func produce_next(grammar(A), result(A)) = result(A).
:- mode produce_next(in(grammar(V, C)), in(result)) = out(result) is det.

:- pred produce_all(grammar(A), int, results(A), results(A)).
:- mode produce_all(in(grammar(V, C)), in, in(results), out(results)) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%----------------------------------------------------------------------------%

produce_next(Grammar, LastStep) =
    condense(map(Grammar ^ grammar_rules, LastStep)).

produce_all(Grammar, StepsLeft, [], Results) :-
    produce_all(Grammar, StepsLeft, [Grammar ^ grammar_axiom], Results).

produce_all(Grammar, StepsLeft, [LastStep | LastRest], Results) :-
    NewStep = produce_next(Grammar, LastStep),
    NewResults = [NewStep, LastStep | LastRest],
    ( if StepsLeft = 1 then
        Results = NewResults
    else if StepsLeft > 1 then
        produce_all(Grammar, StepsLeft - 1, NewResults, Results)
    else
       unexpected($file, $pred, "invalid number of steps, must be > 0")
    ).

%----------------------------------------------------------------------------%
:- end_module l_system.
%----------------------------------------------------------------------------%
