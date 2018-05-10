(* Comment
 * on
 * multiple
 * lines
 *)

(** Different style *)

open! AModule
module F = Format

type a_b =
  { foo_bar: float
  ; bar: float }

type c_d = {a: b; c: d}

type e_f = G of A.b * C.d_f | H | I

let a = b

let some_function = function
  | Constructor _ ->
      Module.some_other_function
  | OtherConstructor _ ->
      OtherModule.other_function

let string_of_something = function
  | Constructor _ ->
      "some_string"
