use "hw2provided.sml";

(* Write a function all_except_option, which takes a string and a string list.
   Return NONE if the string is not in the list, else return SOME lst where lst
   is identical to the argument list except the string is not in it. You may
   assume the string is in the list at most once. Use same_string, provided to
   you, to compare strings. Sample solution is around 8 lines. *)
fun all_except_option(s : string, slist : string list) =
  let
    fun aux(xs, prepend) =
      case xs
       of [] => NONE
        | x::xs' =>
          if same_string(x, s) then
            SOME(prepend @ xs')
          else
            aux(xs', prepend @ [x])
  in
    aux(slist, [])
  end
