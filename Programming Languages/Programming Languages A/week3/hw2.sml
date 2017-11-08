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
    val options = get_substitutions2 (names, first)
    val first_names = first :: options

    fun fullname(first : string) =
      { first = first
      , middle = middle
      , last = last
      }

    fun first_names_to_fullnames(names : string list) =
      case names
       of [] => []
        | first_name::rest => fullname first_name :: first_names_to_fullnames rest
  in
    first_names_to_fullnames first_names
  end

(* 2a. Write a function card_color, which takes a card and returns its color
  (spades and clubs are black, diamonds and hearts are red). Note: One
  case-expression is enough. *)
fun card_color((Spades, _) : card) = Black
  | card_color((Clubs, _) : card) = Black
  | card_color((Diamonds, _) : card) = Red
  | card_color((Hearts, _) : card) = Red

(* 2b. Write a function card_value, which takes a card and returns its value
   (numbered cards have their number as the value, aces are 11, everything else
   is 10). Note: One case-expression is enough. *)
fun card_value((_, Num n) : card) = n
  | card_value((_, Ace) : card) = 11
  | card_value(x : card) = 10

(* 2c. Write a function remove_card, which takes a list of cards cs, a card c,
   and an exception e. It returns a list that has all the elements of cs except
   c.  If c is in the list more than once, remove only the first one. If c is
   not in the list, raise the exception e. You can compare cards with =. *)
fun remove_card(cards : card list, c : card, e : exn) =
  case cards
   of [] => raise e
    | h::t =>
      if h = c then
        t
      else
        h :: remove_card (t, c, e)

(* 2d. Write a function all_same_color, which takes a list of cards and returns
   true if all the cards in the list are the same color. Hint: An elegant
   solution is very similar to one of the functions using nested
   pattern-matching in the lectures. *)
fun all_same_color(cards : card list) =
  let
    fun color_check(col, cards) =
      case cards
       of [] => true
        | h::t =>
          let
            val same = card_color(h) = col
          in
            same andalso color_check(col, t)
          end
  in
    case cards
     of [] => true
      | h::[] => true
      | h::t => color_check (card_color(h), t)
  end

(* 2e. Write a function sum_cards, which takes a list of cards and returns the
   sum of their values. Use a locally defined helper function that is tail
   recursive. (Take “calls use a constant amount of stack space” as a
   requirement for this problem.) *)
fun sum_cards(cards : card list) =
  let
    fun aux (xs, acc) =
      case xs
       of [] => acc
        | x::xs' => aux(xs', acc + card_value (x))
  in
    aux (cards, 0)
  end
