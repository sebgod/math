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
    --->    grammar(
                grammar_vars    ::  list(A),
                grammar_consts  ::  list(A),
                grammar_axiom   ::  list(A),
                grammar_rules   ::  rules(A)
            ).

:- inst grammar(V, C)
    --->    grammar(
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

:- pred produce(grammar(A), result(A)).
:- mode produce(in(grammar(V, C)), out(result)) is multi.

:- pred produce_next(grammar(A), result(A), result(A)).
:- mode produce_next(in(grammar(V, C)), in(result), out(result)) is det.

:- func produce_nth0(grammar(A), int) = result(A).
:- mode produce_nth0(in(grammar(V, C)), in) = out(result) is det.

:- pred produce_all(grammar(A), int, results(A), results(A)).
:- mode produce_all(in(grammar(V, C)), in, in(results), out(results)) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module solutions.

%----------------------------------------------------------------------------%

produce_next(Grammar, LastStep, Next) :-
    Next = condense(map(Grammar ^ grammar_rules, LastStep)).

%----------------------------------------------------------------------------%

produce(Grammar, Result) :-
    produce_loop(Grammar, Grammar ^ grammar_axiom, Result).

:- pred produce_loop(grammar(A), result(A), result(A)).
:- mode produce_loop(in(grammar(V, C)), in(result), out(result)) is multi.

produce_loop(Grammar, !Result) :-
    produce_next(Grammar, !Result),
    produce_loop(Grammar, !Result).

%----------------------------------------------------------------------------%

produce_nth0(Grammar, Nth) = Solution :-
    Next = (pred(_::in,!.R::in(result), !:R::out(result)) is det :-
        produce_next(Grammar, !R)),
    fold_up(Next, 0, Nth-1, Grammar ^ grammar_axiom, Solution).

%----------------------------------------------------------------------------%

produce_all(Grammar, StepsLeft, [], Results) :-
    produce_all(Grammar, StepsLeft, [Grammar ^ grammar_axiom], Results).

produce_all(Grammar, StepsLeft, [LastStep | LastRest], Results) :-
    produce_next(Grammar, LastStep, NewStep),
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
