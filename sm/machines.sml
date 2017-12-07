(* Output "1" on a sequence of 0,1,1, otherwise output "0".
   https://www.youtube.com/watch?v=S352lyPZP00&t=113s&list=PL2B0E8ECE54459416&index=4 *)

datatype MooreState
  = Moore00
  | Moore01
  | Moore10
  | Moore11

datatype MealyState
  = Mealy00
  | Mealy01
  | Mealy10

fun mealy_machine (inputs : int list, state : MealyState) =
  case (inputs, state)
   of ([], _) => state
    (* Following the main sequence *)
    | (0::rest, Mealy00) => mealy_machine(rest, Mealy01)
    | (1::rest, Mealy01) => mealy_machine(rest, Mealy10)
    | (1::rest, Mealy10) => mealy_machine(rest, Mealy00)
    (* Correctly hangling every fallback *)
    | (0::rest, Mealy10) => mealy_machine(rest, Mealy01)
    | (_::rest, Mealy00) => mealy_machine(rest, Mealy00)
    | (_::rest, Mealy01) => mealy_machine(rest, Mealy01)
    | (_::rest, Mealy10) => mealy_machine(rest, Mealy00)

fun moore_machine (inputs : int list, state : MooreState) =
  case (inputs, state)
   of ([], _) => state
    (* Following the main sequence *)
    | (0::rest, Moore00) => moore_machine(rest, Moore01)
    | (1::rest, Moore01) => moore_machine(rest, Moore10)
    | (1::rest, Moore10) => moore_machine(rest, Moore11)
    (* Correctly hangling every fallback *)
    | (_::rest, Moore00) => moore_machine(rest, Moore00)
    | (_::rest, Moore01) => moore_machine(rest, Moore01)
    | (_::rest, Moore10) => moore_machine(rest, Moore01)
    | (_::rest, Moore11) => moore_machine(rest, Moore00)

val seqs = (
  [
    moore_machine([1, 1, 1, 1, 1, 0], Moore00),
    moore_machine([1, 1, 1, 1, 0, 1], Moore00),
    moore_machine([1, 1, 1, 0, 1, 1], Moore00),
    moore_machine([1, 1, 0, 1, 1, 1], Moore00),
    moore_machine([1, 0, 1, 1, 1, 1], Moore00),
    moore_machine([0, 1, 1, 1, 1, 1], Moore00)
  ], [
    mealy_machine([1, 1, 1, 1, 1, 0], Mealy00),
    mealy_machine([1, 1, 1, 1, 0, 1], Mealy00),
    mealy_machine([1, 1, 1, 0, 1, 1], Mealy00),
    mealy_machine([1, 1, 0, 1, 1, 1], Mealy00),
    mealy_machine([1, 0, 1, 1, 1, 1], Mealy00),
    mealy_machine([0, 1, 1, 1, 1, 1], Mealy00)
  ]
)
