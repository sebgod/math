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
:- import_module turtle.

:- import_module char.
:- import_module int.
:- import_module float.
:- import_module math.
:- import_module list.
:- import_module string.
:- import_module require.
:- import_module solutions.

%----------------------------------------------------------------------------%
%
%   Example grammars from Wikipedia
%
:- type l_turtle_func(T) == (func(int, T) = turtle_cmds).
:- inst l_turtle_func    == (func(in, in) = (out) is det).

:- some [T] pred example(int, string, grammar(T), l_turtle_func(T)).
:- mode example(in, out, out(unique), out(l_turtle_func)) is semidet.
:- mode example(out, out, out(unique), out(l_turtle_func)) is multi.

%----------------------------------------------------------------------------%

example(1, "Algae",
    grammar(['A', 'B'], [], ['A'],
        (func(V) =
            ( V = 'A' -> ['A', 'B']
            ; V = 'B' -> ['A']
            ; [V] % this is implied by L-systems
            )
        )
    ),
    (func(_, _) = [])
).

%----------------------------------------------------------------------------%

example(2, "Pythagoras Tree",
    grammar(['0', '1'], ['[', ']'], ['0'],
        (func(V) =
            ( V = '1' -> ['1', '1']
            ; V = '0' -> ['1', '[', '0', ']', '0']
            ; [V] % this is implied by L-systems
            )
        )
    ),
    (func(_, _) = [])
).

%----------------------------------------------------------------------------%

example(3, "Cantor dust",
    grammar(['A', 'B'], [], ['A'],
        (func(V) =
            ( V = 'A' -> ['A', 'B', 'A']
            ; V = 'B' -> ['B', 'B', 'B']
            ; [V] % this is implied by L-systems
            )
        )
    ),
    (func(_, _) = [])
).

%----------------------------------------------------------------------------%

example(4, "Koch curve",
    grammar(['F'], ['+', '-'], ['F', '+', '+', 'F', '+', '+', 'F'],
        (func(V) =
            ( V = 'F' -> ['F', '-', 'F', '+', '+', 'F', '-', 'F']
            ; [V] % this is implied by L-systems
            )
        )
    ),
    (func(N, A) =
        ( A = 'F' -> [move(1.0/(float(N) + 1.0))]
        ; A = (+) -> [turn(pi/3.0)]
        ; A = (-) -> [turn(-pi/3.0)]
        ; unexpected($file, $pred, format("%c not in alphabet", [c(A)]))
        )
    )
).

%----------------------------------------------------------------------------%

example(5, "Sierpinksi triangle",
    grammar(['A', 'B'], ['+', '-'], ['A'],
        (func(V) =
            ( V = 'A' -> ['B', '-', 'A', '-', 'B']
            ; V = 'B' -> ['A', '+', 'B', '+', 'A']
            ; [V] % this is implied by L-systems
            )
        )
    ),
    (func(_, _) = [])
).

%----------------------------------------------------------------------------%

example(6, "Dragon curve",
    grammar(['X', 'Y'], ['F', '+', '-'], ['F', 'X'],
        (func(V) =
            ( V = 'X' -> ['X', '+', 'Y', 'F', '+']
            ; V = 'Y' -> ['-', 'F', 'X', '-', 'Y']
            ; [V] % this is implied by L-systems
            )
        )
    ),
    (func(_, _) = [])
).

%----------------------------------------------------------------------------%

main(!IO) :-
    ExampleNumbers =
        solutions((pred(Number::out) is multi :- example(Number, _, _, _))),
    foldl(print_example, ExampleNumbers, !IO).

:- pred print_example(int::in, io::di, io::uo) is det.

print_example(Number, !IO) :-
    ( if
        example(Number, Name, Grammar, LSystemToTurtle)
    then
        Depth = 3,
        io.format("https://en.wikipedia.org/wiki/L-system#Example_%d:_%s\n",
            [i(Number), s(replace_all(Name, " ", "_"))], !IO),
        produce_all(Grammar, Depth, [], RevResults),
        reverse(RevResults, Results),
        foldl(print_as_line, Results, !IO),
        nl(!IO),
        T = (pred(A::in, B::out) is det :- B = LSystemToTurtle(Depth, A)),
        map(T, produce_nth0(Grammar, Depth), Turtle0),
        condense(Turtle0, Turtle)
    else
        sorry($file, $pred, format("Example %d", [i(Number)]))
    ).

%----------------------------------------------------------------------------%
%
% Printing utilities
%

:- pred print_as_line(list(T)::in, io::di, io::uo) is det.

print_as_line(List, !IO) :-
    write_string("> ", !IO),
    ( dynamic_cast(List, Chars) ->
        write_string(from_char_list(Chars), !IO)
    ;
        write(List, !IO)
    ),
    nl(!IO).

%----------------------------------------------------------------------------%
:- end_module test_l_system.
%----------------------------------------------------------------------------%
