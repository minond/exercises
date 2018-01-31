fun silly2 () =
  let
    val x = 3
  in
    (let val x = 2 in x + 1 end) + (let val y = 3 in y + x end)
  end
