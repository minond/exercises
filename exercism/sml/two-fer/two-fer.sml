fun name (input: string option): string =
  case input
   of SOME name => "One for " ^ name ^ ", one for me."
    | NONE => "One for you, one for me."
