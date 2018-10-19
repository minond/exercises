type argcount = int
type value = int
type address = int

datatype Bytecode
  (* = IADD *)
  (* | ISUB *)
  (* | IMUL *)
  (* | ILT *)
  (* | IEQ *)
  (* | BR *)
  (* | BRT of address *)
  (* | BRF of address *)
  = ICONST of value
  (* | LOAD of address *)
  (* | STORE of address *)
  | PRINT
  (* | POP *)
  (* | CALL of address * argcount *)
  (* | RET *)
  (* | HALT *)

fun vm code =
  let
    val calls : int list ref = ref []
    val stack : int list ref = ref []
    val regrs : int list ref = ref []

    (* val ip = ref 0 *)
    val sp = ref 0
    fun eval ip =
      let
        fun tick () =
          eval (ip + 1)
      in
        case List.nth (code, ip)
         of ICONST(i) => (stack := i :: (!stack); tick ())
          | PRINT => (print (Int.toString (List.nth (!stack, !sp))) ; tick ())
      end
  in
    eval 0
  end



val p = [ICONST(42), PRINT]
val x = vm p
