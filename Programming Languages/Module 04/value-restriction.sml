(*

The funciton below results in the following warning which results in an
un-callable curried function being returned:

  val x = List.map (fn x => (x, 1));

  Warning: type vars not generalized because of value restriction are
  instantiated to dummy types (X1,X2,...)

There are a couple of ways around this, like setting an explicit type or adding
an argument which is passed to the curried function:

  fun x y = List.map (fn x => (x, 1)) y;
  val x : string list -> (string * int) list = List.map (fn x => (x, 1))


Both of these functions have a type similar to this one:

  fn : 'a list -> ('a * int) list

 *)
