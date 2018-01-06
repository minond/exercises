fun map(f, xs) =
  case xs
   of [] => []
    | h::t => (f h) :: map (f, t)

fun filter(f, xs) =
  case xs
   of [] => []
    | h::t =>
        if f h then
          h :: filter(f, t)
        else
          filter(f, t)

val plusOne = map((fn x => x + 1), [1, 2, 3, 4, 5, 6, 7, 8, 9])
val evens = filter((fn x => x mod 2 = 0), [1, 2, 3, 4, 5, 6, 7, 8, 9])
