fun fold (f, acc, xs) =
  case xs
   of [] => acc
    | x::xs' => fold(f, f(acc, x), xs')

val strSizeSum =
  fold(fn (sum, s) => sum + String.size s, 0, ["Marcos", "Minond"])
