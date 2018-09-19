# Seqs can be created with the `@` operator of the `newSeq[T](n: int)` method.
var
  names = @["Marcos", "Minond", "Estrada", "Oscar"]

echo names

names.add("Another")
echo names

names.delete(4)
echo names

echo names.len
echo names.low
echo names.high

for i, c in names:
  echo c, " is at index ", i

proc foo(s: var seq[string]) =
  s.add("Another")

names.foo
echo names
