(*

  f : T1 * T2 * T3 -> T4

  T4 = T1 * T2 * T3
  T4 = T2 * T1 * T3
     v
     v
  T1 = T2
  T2 = T1

  T4 : 'a * 'a * 'b

  f : T4 -> T4
  f : 'a * 'a * 'b -> 'a * 'a * 'b

 *)

fun f (x, y, z) =
  if true
  then (x, y, z)
  else (y, x, z)


(*

  len : T1 -> T2
  xs : T1 list

  x : T1
  xs' : T1 list

  T2 = int

  len : T1 list -> int
  len : 'a list -> int

 *)

fun len xs =
  case xs
   of [] => 0
    | x::xs' => 1 + len xs'


(*

  compose1 : T1 * T2 -> T3
  f : T1
  g : T2
  x : T4

  f : T2 * T4 -> T3
  compose1 : (T2 * T4 -> T3) * T2 -> T4 -> T3
  compose1 : ('a * 'b -> 'c) * 'a -> 'b -> 'c

 *)

fun compose1 (f, g) =
  fn x =>
    f (g, x)


(*

  compose2 : T1 * T2 -> T3
  x : T4
  g : T4 -> T5
  f : T5 -> T3

  compose2 : (T5 -> T3) * (T4 -> T5) -> T4 -> T3
  compose2 : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b

 *)

fun compose2 (f, g) =
  fn x =>
    f (g x)
