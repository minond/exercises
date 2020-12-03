use "../../../utils/aoc.sml";

(* https://adventofcode.com/2020/day/2
 *
 * To try to debug the problem, they have created a list (your puzzle input) of
 * passwords (according to the corrupted database) and the corporate policy when
 * that password was set.
 *
 * For example, suppose you have the following list:
 *
 * 1-3 a: abcde
 * 1-3 b: cdefg
 * 2-9 c: ccccccccc
 *
 * Each line gives the password
 * policy and then the password. The password policy indicates the lowest and
 * highest number of times a given letter must appear for the password to be
 * valid. For example, 1-3 a means that the password must contain a at least 1
 * time and at most 3 times.
 *
 * In the above example, 2 passwords are valid. The middle password, cdefg, is
 * not; it contains no instances of b, but needs at least 1. The first and third
 * passwords are valid: they contain one a or nine c, both within the limits of
 * their respective policies.
 *
 * How many passwords are valid according to their policies? *)
signature PASSWORD_ENTRY = sig
  type policy
  type entry
  val parse : string -> entry option
  val valid : entry -> bool
end

structure PasswordEntry :> PASSWORD_ENTRY = struct
  datatype policy = Policy of int * int * char
  type entry = { password : string, policy : policy }

  fun parse line =
    let
      val parts = StringExt.splitBy #" " line
      val length = List.nth(parts, 0)
      val minmax = StringExt.splitBy #"-" length
      val minS = List.nth(minmax, 0)
      val maxS = List.nth(minmax, 1)
      val letter = String.sub(List.nth(parts, 1), 0)
      val password = List.nth(parts, 2)
    in
      case (Int.fromString minS, Int.fromString maxS)
        of (SOME(min), SOME(max)) =>
             SOME({ password = password
                  , policy = Policy(min, max, letter) })
         | _ => NONE
    end
    handle Subscript => NONE

  fun valid entry =
    case #policy entry
      of Policy(min, max, ch) =>
           let val curr = count (eq ch) (String.explode (#password entry))
           in
             curr >= min andalso curr <= max
           end
end

structure Day02 = struct
  fun run inputFile =
    let
      val lines = Aoc.readAllLines inputFile
      val entries = List.mapPartial PasswordEntry.parse lines
      val valid = count PasswordEntry.valid entries
    in
      print("Found " ^ Int.toString valid ^ " valid entries\n")
    end
end

;;

Day02.run "input.txt"
