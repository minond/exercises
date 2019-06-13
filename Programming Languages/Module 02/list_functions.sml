fun sum_list (xs : int list) =
  if null xs
  then 0
  else hd xs + sum_lists(tl xs)

fun list_product (xs : int list) =
  if null xs
  then 1
  else hd xs * list_product(tl xs)

fun countdown (x : int) =
  if x = 0
  then []
  else x :: countdown(x - 1)

fun append (xs : 'a list, ys : 'a list) =
  if null xs
  then ys
  (* else append(tl xs, (hd xs)::ys) *)
  else (hd xs) :: append(tl xs, ys)

fun sum_pair_list (xs : (int * int) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

fun firsts (xs : ('a * 'a) list) : 'a list =
  if null xs
  then []
  else #1 (hd xs) :: firsts(tl xs)

fun seconds (xs : ('a * 'a) list) : 'a list =
  if null xs
  then []
  else #2 (hd xs) :: seconds(tl xs)

fun factorial (n : int) =
  list_product(countdown n)
