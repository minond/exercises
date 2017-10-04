datatype mytype
  = TwoInts of int * int
  | Str of string
  | Pizza

val a = Str "hi" (* Str "hi" : mytype *)
val b = Str (* fn : string -> mytype *)
val c = Pizza (* Pizza : mytype *)
val d = TwoInts (1, 2) (* TwoInts (1, 2) : mytype *)
val e = a (* Str "hi" : mytype *)
