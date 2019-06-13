(* Signatures are types for modules (structures) *)

signature MATHLIB =
sig
  val double : int -> int
end

structure MyMathModule :> MATHLIB =
struct
  (* This is not in the MATHLIB signature so it can not be used outside of
   * MyMathModule *)
  val two =
    2

  fun double x =
    x * two
end

val a1 = MyMathModule.double 10
