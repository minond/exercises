(* 1. Write a function only_capitals that takes a string list and returns a
 * string list that has only the strings in the argument that start with an
 * uppercase letter. Assume all strings have at least 1 character. Use
 * List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)
fun only_capitals xs =
  List.filter (fn s => Char.isUpper (String.sub (s, 0))) xs

(* 2. Write a function longest_string1 that takes a string list and returns the
 * longest string in the list. If the list is empty, return "". In the case of
 * a tie, return the string closest to the beginning of the list. Use foldl,
 * String.size, and no recursion (other than the implementation of foldl is
 * recursive). *)
fun size_comparison f (a, b) =
  if f (String.size a, String.size b) then
    a
  else
    b

fun longest_string1 xs =
  foldl (size_comparison (fn (a, b) => a > b)) "" xs

(* 3. Write a function longest_string2 that is exactly like longest_string1
 * except in the case of ties it returns the string closest to the end of the
 * list. Your solution should be almost an exact copy of longest_string1.
 * Still use foldl and String.size. *)
fun longest_string2 xs =
  foldl (size_comparison (fn (a, b) => a >= b)) "" xs

(* 4. Write functions longest_string_helper, longest_string3, and
 * longest_string4 such that:
 *
 *   • longest_string3 has the same behavior as longest_string1 and
 *     longest_string4 has the same behavior as longest_string2.
 *
 *   • longest_string_helper has type (int * int -> bool) -> string list ->
 *     string (notice the currying). This function will look a lot like
 *     longest_string1 and longest_string2 but is more general because it takes
 *     a function as an argument.
 *
 *   • If longest_string_helper is passed a function that behaves like > (so it
 *     returns true exactly when its first argument is stricly greater than its
 *     second), then the function returned has the same behavior as
 *     longest_string1.
 *
 *   • longest_string3 and longest_string4 are defined with val-bindings and
 *     partial applications of longest_string_helper. *)
fun longest_string_helper f xs s =
  foldl f s xs

val longest_string3 = longest_string1

val longest_string4 = longest_string2

(* 5. Write a function longest_capitalized that takes a string list and returns
 * the longest string in the list that begins with an uppercase letter, or ""
 * if there are no such strings. Assume all strings have at least 1 character.
 * Use a val-binding and the ML library’s o operator for composing functions.
 * Resolve ties like in problem 2. *)
fun longest_capitalized xs =
  (longest_string1 o only_capitals) xs

(* 6. Write a function rev_string that takes a string and returns the string
 * that is the same characters in reverse order. Use ML’s o operator, the
 * library function rev for reversing lists, and two library functions in the
 * String module. (Browse the module documentation to find the most useful
 * functions.) *)
fun rev_string s =
  (String.implode o rev o String.explode) s

(* The next two problems involve writing functions over lists that will be
 * useful in later problems. *)

(* 7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b
 * (notice the 2 arguments are curried). The first argument should be applied
 * to elements of the second argument in order until the first time it returns
 * SOME v for some v and then v is the result of the call to first_answer. If
 * the first argument returns NONE for all list elements, then first_answer
 * should raise the exception NoAnswer. Hints: Sample solution is 5 lines and
 * does nothing fancy. *)
exception NoAnswer

fun first_answer f xs =
  case xs
   of [] => raise NoAnswer
    | h::t =>
        case f h
         of SOME v => v
          | NONE => first_answer f t

(* 8. Write a function all_answers of type (’a -> ’b list option) ->
 * ’a list -> ’b list option (notice the 2 arguments are curried). The first
 * argument should be applied to elements of the second argument. If it returns
 * NONE for any element, then the result for all_answers is NONE. Else the
 * calls to the first argument will have produced SOME lst1, SOME lst2, ...
 * SOME lstn and the result of all_answers is SOME lst where lst is lst1,
 * lst2, ..., lstn appended together (order doesn’t matter). Hints: The sample
 * solution is 8 lines. It uses a helper function with an accumulator and uses
 * @. Note all_answers f [] should evaluate to SOME []. *)

