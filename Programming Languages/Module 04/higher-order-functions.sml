fun n_times(f, n, x) =
  if n = 0 then
    x
  else
    f(n_times(f, n - 1, x))

fun increment x =
  x + 1

fun double x =
  x + x

fun addition(n, x) =
  n_times(increment, n, x)

fun nth_tl(n, l) =
  n_times(tl, n, l)

val x1 = n_times(double, 4, 7)
val x2 = n_times(increment, 4, 7)
val x3 = n_times(tl, 2, [1, 2, 3, 4, 5, 6, 7, 8, 9])
val x4 = nth_tl(2, [1, 2, 3, 4, 5, 6, 7, 8, 9])
