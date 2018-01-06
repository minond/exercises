fun countdown (x : int) =
  if x = 0
  then []
  else x :: countdown(x - 1)

fun max1 (xs : int list) =
  if null xs then
    NONE
  else
    let val v = max1(tl xs)
    in
      if isSome v andalso valOf v > hd xs then
        v
      else
        SOME (hd xs)
    end

(* int list -> int option *)
fun max2 (xs : int list) =
  if null xs then
    NONE
  else
    let
      (* int list -> int *)
      fun max_nonempty (xs : int list) =
        if null (tl xs) then
          hd xs
        else
          let val v = max_nonempty(tl xs)
          in
            if hd xs > v then
              hd xs
            else
              v
          end
    in
      SOME (max_nonempty xs)
    end
