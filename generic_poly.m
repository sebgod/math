:- module generic_poly.

:- interface.

:- import_module string.

:- func to_poly_type(T) = poly_type.

:- implementation.

:- import_module require.
:- import_module univ.

to_poly_type(T) =
    ( if dynamic_cast(T, Int) then
        i(Int)
      else if dynamic_cast(T, String) then
        s(String)
      else if dynamic_cast(T, Float) then
        f(Float)
      else if dynamic_cast(T, Char) then
        c(Char)
      else if dynamic_cast(T, Univ) then
        to_poly_type(univ_value(Univ))
      else
        unexpected($file, $pred, "cannot format type")
    ).

:- end_module generic_poly.
