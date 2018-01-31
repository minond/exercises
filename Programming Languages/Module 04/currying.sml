fun testing (a : int) (b : int) (c : int) =
  a + b + c

val res = testing 1 2 3


fun fold f acc xs =
  case xs
   of [] => acc
    | x::xs' => fold f (f(acc, x)) xs'

val adder = fold (fn (total, n) => n + total) 0

val total = adder [1, 2, 3]
