(* https://adventofcode.com/2020/day/1
 *
 * Before you leave, the Elves in accounting just need you to fix your expense
 * report (your puzzle input); apparently, something isn't quite adding up.
 *
 * Specifically, they need you to find the two entries that sum to 2020 and then
 * multiply those two numbers together.
 *
 * For example, suppose your expense report contained the following:
 *
 * 1721
 * 979
 * 366
 * 299
 * 675
 * 1456
 *
 * In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
 * them together produces 1721 * 299 = 514579, so the correct answer is 514579. *)
structure Day01 = struct
  val TOTAL = 2020

  val readAll =
    TextIO.inputAll o TextIO.openIn

  fun eq a b =
    a = b

  fun splitTokens char str =
    String.tokens (eq char) str

  fun first f (n, acc) =
    case acc of
         NONE => f n
       | _ => acc

  fun withPartner ns n =
    let
      val partner = TOTAL - n
      val exists = List.exists (eq partner) ns
    in
      if exists
      then SOME([n, partner])
      else NONE
    end

  fun printAnswer results =
    case results of
         SOME(a :: b :: nil) =>
           let
             val answer = Int.toString(a * b)
             val left = Int.toString a
             val right = Int.toString b
           in
             print("Result: " ^ left ^ " * " ^ right ^ " = " ^ answer ^ "\n")
           end
       | _ => print("No results found.")

  fun run inputFile =
    let
      val input = readAll inputFile
      val lines = splitTokens #"\n" input
      val numbs = List.mapPartial Int.fromString lines
      val results = foldl (first (withPartner numbs)) NONE numbs
    in
      printAnswer results
    end
end ;

Day01.run "../input.txt"
