fun map(f, xs) =
  case xs
   of [] => []
    | h::t => (f h) :: map (f, t)

val x1 = map((fn x => x + 1), [1, 2, 3, 4, 5, 6, 7, 8, 9])
