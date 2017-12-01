# The `result` vairlabes is special in that it has an implicit return. So don't
# redeclare it.
proc getAlphabet(): string =
  result = ""
  for letter in 'a'..'z':
    result.add(letter)
