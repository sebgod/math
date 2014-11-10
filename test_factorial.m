:- module test_factorial.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module factorial.
:- import_module generic_poly.
:- import_module integer.
:- import_module list.
:- import_module std_util.
:- import_module string.

main(!IO) :-
    command_line_arguments(Args, !IO),
    ToInteger = integer.det_from_string `compose` list.det_index0(Args),
    Number = ToInteger(0),
    Fmt = to_poly_type `compose` integer.to_string,
    Result = factorial(Number),
    io.format("factorial(%s) = %s\n", list.map(Fmt, [Number, Result]), !IO).
