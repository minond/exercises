(* Output "1" on a sequence of 0,1,1, otherwise output "0". *)
datatype State
  = S00
  | S01
  | S10
  | S11

fun moore_machine (inputs : int list, state : State) =
  case (inputs, state)
   of ([], _) => state
    (* Following the main sequence *)
    | (0::rest, S00) => moore_machine(rest, S01)
    | (1::rest, S01) => moore_machine(rest, S10)
    | (1::rest, S10) => moore_machine(rest, S11)
    (* Correctly hangling every fallback *)
    | (_::rest, S00) => moore_machine(rest, S00)
    | (_::rest, S01) => moore_machine(rest, S01)
    | (_::rest, S10) => moore_machine(rest, S01)
    | (_::rest, S11) => moore_machine(rest, S00)

val seqs = [
  moore_machine([1, 1, 1, 1, 1, 0], S00),
  moore_machine([1, 1, 1, 1, 0, 1], S00),
  moore_machine([1, 1, 1, 0, 1, 1], S00),
  moore_machine([1, 1, 0, 1, 1, 1], S00),
  moore_machine([1, 0, 1, 1, 1, 1], S00),
  moore_machine([0, 1, 1, 1, 1, 1], S00)
]
