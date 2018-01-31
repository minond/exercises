signature RATIONAL_B =
sig
  (* This defines a `rational` type while not making it publicly available. And *)
  (* since the definition lives outside of the signature (and in the structure), it *)
  (* is private. The only way to 'make' a rational is with make_frac. *)
  type rational
  exception BadFrac

  val make_frac : int * int -> rational
  val add : rational * rational -> rational
  val toString : rational -> string

  (* This is cool. Since the data type in our structure to come will most likely *)
  (* use type constructors, SML allows us to define a val binding in the signature *)
  (* of the same name making the constructor public. *)
  val Whole : int -> rational
end

structure Rational1 :> RATIONAL_B =
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
