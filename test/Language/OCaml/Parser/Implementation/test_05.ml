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
