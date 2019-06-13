datatype mytype
  = TwoInts of int * int
  | Str of string
  | Pizza

fun f x =
  case x
   of TwoInts (i1, i2) => i1 + i2
    | Str s => String.size s
    | Pizza => 3

fun f2 x =
  case x
   of TwoInts (i1, i2) => i1 + i2
    | _ => 32
