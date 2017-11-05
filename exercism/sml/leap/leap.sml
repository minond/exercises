fun divisibleBy (input: int, by: int): bool =
  input mod by = 0

fun leapYear (input: int): bool =
  divisibleBy(input, 4) andalso
  (not(divisibleBy(input, 100)) orelse divisibleBy(input, 400))
