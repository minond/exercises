fun mr f g =
  fn x =>
    if x
    then f (g x)
    else g (f x)


(*

  Standard ML's `and` keyword lets you add multiple functions and datatypes to
  the environement at the same time. This is required when those bindings depend
  on eachother. Notice the `t1` type below, it has a constructor that has a type
  of `t2`, and `t2` has a constructor that has a type of `t1`. These two types
  couldn't be defined separate of eachother. Creating a `t1` and a `t2` can be
  done like this:

    - tt2 Y;
    val it = tt2 Y : t1
    - tt1 X;
    val it = tt1 X : t2

 *)

fun f1 x =
  case x
   of 0 => 1
    | _ => 1 + f2 (x - 1)

and f2 x =
  case x
   of 0 => 1
    | _ => 1 + f1 (x - 1)


datatype t1 = X
            | tt2 of t2

     and t2 = tt1 of t1
            | Y


(*

  What if we didn't have the `and` keyword and mutual recursion in general? What
  could we do then? Well, higher-order functions could help.

  Example below. By the time `gg` is defined `ff` is already in the
  environement, so it can call it. On the other hand, `ff` needs `gg` so we can
  make this a parameter of `ff`, givins us these type signatures and api:

    val ff = fn : (int -> int) * int -> int
    val gg = fn : int -> int

    - ff (gg, 10)
    val it = 11 : int

 *)

fun ff (g, x) =
  case x
   of 0 => 1
    | _ => 1 + g (x - 1)

fun gg (x) =
  case x
   of 0 => 1
    | _ => 1 + ff (gg, x - 1)
