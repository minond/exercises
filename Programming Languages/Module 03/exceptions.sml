exception EmptyList
exception UnknownError of string

fun maxlist xs =
  case xs
   of [] => raise EmptyList
    | x::[] => x
    | x::xs' => Int.max(x, maxlist(xs'))

val x = maxlist([1, 2, 3, 4, 5])

val answer = maxlist([]) handle
    UnknownError _ => 0
  | EmptyList => 42
