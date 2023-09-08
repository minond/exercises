fun response s =
    let
      val chars = List.filter (not o Char.isSpace) (String.explode s)
      val letters = List.filter Char.isAlpha chars
      val last = if not(List.null chars) then List.last chars else #" "

      val hasLetter = not(List.null(letters))
      val allUppercase = List.all Char.isUpper letters

      val isEmpty = List.null(chars)
      val isYelling = allUppercase andalso hasLetter
      val isQuestion = last = #"?"
    in
      case (isEmpty, isYelling, isQuestion)
       of (true, _, _) => "Fine. Be that way!"
        | (_, true, true) => "Calm down, I know what I'm doing!"
        | (_, true, _) => "Whoa, chill out!"
        | (_, _, true) => "Sure."
        | _ => "Whatever."
    end
