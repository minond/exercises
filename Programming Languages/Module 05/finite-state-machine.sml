fun onetwo xs =
  let
    fun one xs =
      case xs
       of [] => true
        | 1::xs' => two xs'
        | _ => false

    and two xs =
      case xs
       of [] => true
        | 2::xs' => one xs'
        | _ => false
  in
    one xs
  end

val a1 = onetwo [1, 2, 1, 2, 1, 2, 1, 2] (* true *)
val a2 = onetwo [] (* true *)
val a3 = onetwo [1, 2, 3] (* false *)
val a4 = onetwo [1, 1, 1, 1] (* false *)
val a5 = onetwo [1, 2, 2, 1] (* false *)
