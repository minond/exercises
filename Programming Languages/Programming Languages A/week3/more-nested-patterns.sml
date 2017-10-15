fun nondecreasing xs =
  case xs
   of [] => true
    | _::[] => true
    | h::(n::t) => h <= n andalso nondecreasing (n::t)

datatype sgn = P | N | Z

fun multsign (x1, x2) =
  let
    fun sign x =
      if x = 0 then
        Z
      else if x > 0 then
        P
      else
        N
  in
    case (sign x1, sign x2)
     of (Z, _) => Z
      | (_, Z) => Z
      | (N, P) => N
      | (P, N) => N
      | (N, N) => P
      | (P, P) => P
  end

fun len xs =
  case xs
   of [] => 0
    | _::xs' => 1 + len xs'
