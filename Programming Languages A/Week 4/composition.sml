(* This is the inferred type: ('a -> 'b) * ('c -> 'a) -> 'c -> 'b *)
fun compose(f: 'a -> 'b, g: 'c -> 'a): 'c -> 'b =
  fn x => f(g x)


(* The three following functions all have the same type *)
fun sqrt_of_abs_1 (i : int) =
  compose(Math.sqrt, compose(Real.fromInt, abs)) i

fun sqrt_of_abs_2 (i : int) =
  (Math.sqrt o Real.fromInt o abs) i

val sqrt_of_abs_3 =
  Math.sqrt o Real.fromInt o abs


(* F#'s operator for left-to-right easyness: *)
infix |>
fun x |> f = f x

fun sqrt_of_abs_4 i = i
  |> abs
  |> Real.fromInt
  |> Math.sqrt


(* Type: ('a -> 'b option) -> ('a -> 'b) -> 'a -> 'b *)
fun getOrElseFn (f : 'a -> 'b option, g : 'a -> 'b) : 'a -> 'b =
  fn x => case f x
   of NONE => g x
    | SOME y => y

fun tryCatchFn (f : 'a -> 'b, g : 'a -> 'b) : 'a -> 'b =
  fn x =>
    f x handle _ => g x
