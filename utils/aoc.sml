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
end

structure StringExt = struct
  open String

  fun splitBy char str =
    tokens (Op.eq char) str
end

val eq = Op.eq ;
val first = ListExt.first ;
