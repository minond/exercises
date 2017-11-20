(* (int -> int) * int -> int *)
fun times_until_zero (f, x) =
  if x = 0 then
    0
  else
    1 + times_until_zero(f, f x)

(* 'a list -> int *)
fun len xs =
  case xs
   of [] => 0
    | _::xs' => 1 + len xs'
