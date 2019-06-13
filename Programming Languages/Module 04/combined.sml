fun curry f x y =
  f (x, y)

fun uncurry f (x, y) =
  f x y

(* Each of these tuples and single arguments can be individually curried but
 * tuples cannot be broken down into smaller units. For example, I cannot pass
 * a and b separately nor can I do that with c and d:
 *)
fun someEquation (a, b) (c, d) e f g =
  a + b + c + d + e + f + g
