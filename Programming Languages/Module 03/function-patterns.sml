datatype exp
  = Constant of int
  | Negate of exp
  | Add of exp * exp
  | Multiply of exp * exp

fun eval0 e =
  case e
   of Constant i => i
    | Negate i => ~ (eval0 i)
    | Add(l, r) => (eval0 l) + (eval0 r)
    | Multiply(l, r) => (eval0 l) * (eval0 r)

fun eval (Constant i) = i
  | eval (Negate e) = ~ (eval e)
  | eval (Add(l, r)) = (eval l) + (eval r)
  | eval (Multiply(l, r)) = (eval l) * (eval r)
