datatype 'a option2
  = NONE
  | SOME of 'a

datatype 'a list2
  = EMPTY
  | CONS of 'a * 'a list2

datatype ('a, 'b) tree
  = NODE of 'a * ('a, 'b) tree * ('a, 'b) tree
  | LEAF of 'b

fun sum_tree tr =
  case tr
   of LEAF i => i
    | NODE(i, l, r) => i + sum_tree l + sum_tree r

fun sum_leaves tr =
  case tr
   of LEAF i => i
    | NODE(i, l, r) => sum_leaves l + sum_leaves r

fun count_leaves tr =
  case tr
   of LEAF i => 1
    | NODE(i, l, r) => count_leaves l + count_leaves r

fun append (xs, ys) =
  case xs
   of [] => ys
    | x::xs' => x :: append(xs', ys)
