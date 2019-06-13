fun count (from : int, to : int) =
  if from = to
  then from :: []
  else from :: count(from + 1, to)

fun countup_from1to_v1(x : int) =
  count(1, x)

fun countup_from1to_v2(x : int) =
  let
    fun count (from : int, to : int) =
      if from = to
      then from :: []
      else from :: count(from + 1, to)
  in
    count(1, x)
  end

fun countup_from1to_v3(x : int) =
  let
    fun count (from : int) =
      if from = x
      then from :: []
      else from :: count(from + 1)
  in
    count(1)
  end
