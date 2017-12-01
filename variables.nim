# Nim supports let, var, and const.
proc getAlphabet(): string =
  var accm = ""

  for letter in 'a'..'z':
    accm.add(letter)

  return accm

# This is computed at compile time
const alphabet = getAlphabet()

# `var` is for mutable variables. Variables have zero values. `int` is 0.
var
  a = "foo"
  b = 0

  c: int

# `let` is for immutable variables. Immutable variables must be initialized or
# else you get a compiler error
let
  d = "foo"
  e = 5
  f: int = 99

a.add("bar")
b += 1
c = 3

echo alphabet
echo a
echo b
echo c
echo d
echo e
echo f
