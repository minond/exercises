datatype set = Set of { insert : int -> set
                      , member : int -> bool
                      , size : unit -> int
                      }

val empty_set =
  let
    fun make_set xs =
      let
        fun contains x =
          List.exists (fn y => x = y) xs
      in
        Set { insert = fn x =>
                if contains x then
                  make_set xs
                else
                  make_set (x :: xs)

            , member = contains
            , size = fn () => List.length xs
            }
      end
  in
    make_set []
  end

val _ = (
  let
    val Set s1 = empty_set
    val Set s2 = (#insert s1) 1
    val Set s3 = (#insert s2) 2
    val Set s4 = (#insert s3) 3
  in
    if (#member s4) 2 then
      print "Yes\n"
    else
      print "No\n"
  end
)
