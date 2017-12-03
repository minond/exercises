proc printThings(things: system.varargs[string]) =
  for thing in things:
    echo thing

# varargs can be coerced into strings using the following type declaration:
proc printStrings(strs: system.varargs[string, `$`]) =
  for str in strs:
    echo str

printThings "one", "two", "three"
printStrings 1, 2, 3, "4", "5", '6', @[7], @[8, 9]
