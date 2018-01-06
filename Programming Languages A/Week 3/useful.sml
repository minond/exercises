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

type card = suit * rank

type name_record =
  { student_num : int option
  , first : string
  , middle : string option
  , last : string
  }

fun isQueenOfSpades (c : card) =
  #1 c = Spade andalso #2 c = Queen

fun isQueenOfSpades (c : card) =
  case c
   of (Spade, Queen) => true
    | _ => false

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Diamond, Ace)
val c3 = (Diamond, Ace)

datatype my_int_list
  = Empty
  | Cons of int * my_int_list

fun append_to_my_int_list (l, h) =
  case l
   of Empty => h
    | Cons (real_h, rest') => Cons(real_h, append_to_my_int_list(rest', h))

fun append (xs : 'a list, x : 'a) =
  case xs
   of [] => [x]
    | h::xs' => h :: append(xs', x)

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
