fun sum_triple triple =
  case triple
   of (x, y, z) => x + y + z

fun sum_triple2 triple =
  let
    val (x, y, z) = triple
  in
    x + y + z
  end

fun sum_triple3 (x, y, z) =
  x + y + x

fun full_name r =
  case r
   of {first=x, middle=y, last=z} => x ^ " " ^ y ^ " " ^ z

fun full_name2 r =
  let
    val {first=x, middle=y, last=z} = r
  in
   x ^ " " ^ y ^ " " ^ z
  end

fun full_name3 {first=x, middle=y, last=z} =
   x ^ " " ^ y ^ " " ^ z

fun rotate_left (x, y, z) =
  (z, y, x)

fun rotate_right t =
  rotate_left(rotate_left t)

(* print sum_triple(1, 2, 3) *)

(* print full_name({first="Marcos", middle="Oscar", last="Minond"}) *)
