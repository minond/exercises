exception ListLengthMismatch

fun zip3 list_triple =
  case list_triple
   of ([], [], []) => []
    | (h1::t1, h2::t2, h3::t3) => (h1, h2, h3) :: zip3(t1, t2, t3)
    | _ => raise ListLengthMismatch

fun unzip3 lst =
  case lst
   of [] => ([], [], [])
    | (a, b, c)::tl => let
      val (t1, t2, t3) = unzip3 tl
    in
      (a::t1, b::t2, c::t3)
    end
