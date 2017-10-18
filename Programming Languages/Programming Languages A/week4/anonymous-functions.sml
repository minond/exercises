fun n_times (f, n, x) =
  if n = 0 then
    x
  else
    f(n_times(f, n - 1, x))

fun triple_n_times (n, x) =
  n_times(fn x => 3 * n, n, x)

(* but this is poor style *)
val triple_n_times2 = fn (n, x) =>
  n_times(fn x => 3 * n, n, x)
