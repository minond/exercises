structure Rational1 =
struct
  datatype rational = Whole of int
                    | Frac of int * int

  exception BadFrac

  fun gcd (x, y) =
    if x = y
    then x
    else if x < y
    then gcd(x, y - x)
    else gcd(x - y, y)

  fun reduce r =
    case r
     of Whole _ => r
      | Frac(x, y) =>
          if x = 0
          then Whole 0
          else
            let
              val d = gcd(abs x, y)
            in
              if d = y
              then Whole (x div d)
              else Frac (x div d, y div d)
            end

  fun make_frac (x, y) =
    if y = 0
    then raise BadFrac
    else if y < 0
    then reduce (Frac(~x, ~y))
    else reduce (Frac(x, y))

  fun add (r1, r2) =
    case (r1, r2)
     of (Whole l, Whole r) => Whole (l+r)
      | (Whole l, Frac (n, d)) => Frac (l * d + n, d)
      | (Frac (n, d), Whole r) => Frac (r * d + n, d)
      | (Frac (n1, d1), Frac (n2, d2)) =>
          if d1 = d2
          then reduce (Frac (n1 + n2, d1))
          else reduce (Frac ((n1*d2) + (n2*d1), d1 * d2))

  fun toString r =
    case r
     of Whole i => Int.toString i
      | Frac (n, d) => String.concat [Int.toString n, "/", Int.toString d]
end
