(* Your solutions must use pattern-matching. You may not use the functions
   null, hd, tl, isSome, or valOf, nor may you use anything containing a #
   character or features not used in class (such as mutation). Note that list
   order does not matter unless specifically stated in the problem. *)
use "hw2provided.sml";

(* 1a. Write a function all_except_option, which takes a string and a string
   list. Return NONE if the string is not in the list, else return SOME lst
   where lst is identical to the argument list except the string is not in it.
   You may assume the string is in the list at most once. Use same_string,
   provided to you, to compare strings. Sample solution is around 8 lines. *)
fun all_except_option(s : string, slist : string list) =
  let
    fun aux(xs, prepend) =
      case xs
       of [] => NONE
        | x::xs' =>
          if same_string (x, s) then
            SOME(prepend @ xs')
          else
            aux (xs', prepend @ [x])
  in
    aux (slist, [])
  end

(* 1b. Write a function get_substitutions1, which takes a string list list (a
   list of list of strings, the substitutions) and a string s and returns a
   string list. The result has all the strings that are in some list in
   substitutions that also has s, but s itself should not be in the result.

   Assume each list in substitutions has no repeats. The result will have
   repeats if s and another string are both in more than one list in
   substitutions.

   Use part (a) and ML’s list-append (@) but no other helper functions. Sample
   solution is around 6 lines. *)
fun get_substitutions1(subs : string list list, s : string) : string list =
  case subs
   of [] => []
    | x::xs =>
      case all_except_option (s, x)
       of NONE => get_substitutions1 (xs, s)
        | SOME l => l @ get_substitutions1 (xs, s)

(* 1c. Write a function get_substitutions2, which is like get_substitutions1
   except it uses a tail-recursive local helper function. *)
fun get_substitutions2(subs : string list list, s : string) : string list =
  let
    fun aux (subs : string list list, s : string) =
      case subs
       of [] => []
        | x::xs =>
          let val prefix =
            case all_except_option (s, x)
             of NONE => []
              | SOME l => l
          in
            prefix @ get_substitutions2(xs, s)
          end
  in
    aux (subs, s)
  end

(* 1d. Write a function similar_names, which takes a string list list of
   substitutions (as in parts (b) and (c)) and a full name of type
   {first:string,middle:string,last:string} and returns a list of full names
   (type {first:string,middle:string,last:string} list). The result is all the
   full names you can produce by substituting for the first name (and only the
   first name) using substitutions and parts (b) or (c). The answer should
   begin with the original name (then have 0 or more other names) *)
fun similar_names(names : string list list, {first : string, middle : string, last : string}) =
  let
    val subs = get_substitutions2 (names, first)

    fun fullname(first : string) =
      { first = first
      , middle = middle
      , last = last
      }

    fun first_names_to_fullnames(names : string list) =
      case names
       of [] => []
        | name::rest => fullname (name) :: first_names_to_fullnames (rest)
  in
    fullname (first) :: first_names_to_fullnames subs
  end
