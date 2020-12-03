structure Op = struct
  fun eq a b =
    a = b
end

structure TextIOExt = struct
  val readAll =
    TextIO.inputAll o TextIO.openIn
end

structure ListExt = struct
  fun first f xs =
    let
      fun aux (x, acc) =
        case acc
          of NONE => f x
           | _ => acc
    in
      foldl aux NONE xs
    end

  fun count f xs =
    let
      fun aux xs n =
        case xs
          of nil => n
           | h :: t => aux t (n + (if f h then 1 else 0))
    in
      aux xs 0
    end
end

structure StringExt = struct
  open String

  fun splitBy char str =
    tokens (Op.eq char) str
end

structure Aoc = struct
  val readAllLines =
    StringExt.splitBy #"\n" o TextIOExt.readAll
end

val eq = Op.eq ;
val first = ListExt.first ;
val count = ListExt.count ;
