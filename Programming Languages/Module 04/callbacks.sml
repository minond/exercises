val cbs : (int -> unit) list ref = ref []

fun onKeyEvent f =
  cbs := f :: (!cbs)

fun onEvent i =
  let
    fun loop cbs =
      case cbs
       of [] => ()
        | f::cbs' => (f i; loop cbs')
  in
    loop (!cbs)
  end


val counter = ref 0

val _ = (
  onKeyEvent (fn _ => counter := 1 + (!counter));
  onKeyEvent (fn i => print ("You pressed a key: " ^ (Int.toString i) ^ "\n"));
  onKeyEvent (fn i => print ("You have pressed any key " ^ (Int.toString (!counter)) ^ "times.\n"))
)

val _ = (
  onEvent 40;
  onEvent 41;
  onEvent 42
)
