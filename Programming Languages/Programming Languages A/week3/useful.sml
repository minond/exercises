datatype suit
  = Club
  | Diamond
  | Heart
  | Spade

datatype rank
  = Jack
  | Queen
  | King
  | Ace
  | Num of int

datatype id
  = StudentNum of int
  | Name of string * (string option) * string

datatype exp
  = Constant of int
  | Negate of exp
  | Add of exp * exp
  | Multiply of exp * exp

(* eval (Add (Constant 32, Negate (Constant 2))); *)
fun eval (e : exp) =
  case e
   of Constant i => i
    | Negate sub => ~ (eval sub)
    | Add (left, right) => (eval left) + (eval right)
    | Multiply (left, right) => (eval left) * (eval right)

fun max_constant_1 (e : exp) =
  let
    fun max_of_two (l : exp, r : exp) =
      let
        val left =
          max_constant_1 l

        val right =
          max_constant_1 r
      in
      Int.max (left, right)
      end
  in
  case e
   of Constant i => i
    | Negate sub => max_constant_1 sub
    | Add (l, r) => max_of_two (l, r)
    | Multiply (l, r) => max_of_two (l, r)
  end

fun max_constant_2 (e : exp) =
  let
    fun max_of_two (l : exp, r : exp) =
      Int.max (max_constant_2 l, max_constant_2 r)
  in
  case e
   of Constant i => i
    | Negate sub => max_constant_2 sub
    | Add (l, r) => max_of_two (l, r)
    | Multiply (l, r) => max_of_two (l, r)
  end
