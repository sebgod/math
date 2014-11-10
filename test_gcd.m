:- module test_gcd.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module gcd.
:- import_module generic_poly.
:- import_module integer.
:- import_module list.
:- import_module std_util.
:- import_module string.

main(!IO) :-
    command_line_arguments(Args, !IO),
    ToInteger = integer.det_from_string `compose` list.det_index0(Args),
    A = ToInteger(0),
    B = ToInteger(1),
    Fmt = to_poly_type `compose` integer.to_string,
    GCD = gcd(A, B),
    io.format("gcd(%s, %s) = %s\n", list.map(Fmt, [A, B, GCD]), !IO).
