fun countdown (x : int) =
  if x = 0
  then []
  else x :: countdown(x - 1)

fun countup (from : int, to : int) =
  if from = to
  then []
  else from :: countup(from + 1, to)

fun bad_max (xs : int list) =
  if null xs then
    0
  else if null (tl xs) then
    hd xs
  else if hd xs > bad_max(tl xs) then
    hd xs
  else
    bad_max(tl xs)

fun good_max (xs : int list) =
  if null xs then
    0
  else if null (tl xs) then
    hd xs
  else
    let val v = good_max(tl xs)
    in
      if hd xs > v then
        hd xs
      else
        v
    end
