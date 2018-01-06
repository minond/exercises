val one = 1

val two = [2]

val numbers = one :: two

val more_mumbers = 1::2::3::4::[5]

(* null : 'a list -> bool    *)
(* hd   : 'a list -> 'a      *)
(* tl   : 'a list -> 'a list *)

fun second (l : 'a list): 'a =
  hd (tl l)

fun third (l : 'a list): 'a =
  hd (tl (tl l))

fun empty (l : 'a list): bool =
  null l

