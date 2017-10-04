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
