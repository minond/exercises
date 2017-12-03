# Nim can iterate over objects. When this is the case, it will call the
# `items(range: T): int` and `pairs(range: T): tuple[int, T2]` iterator
# methods.
type
  CustomRange = object
    low: int
    high: int

# used in `for X in Y` expressions
iterator items(range: CustomRange): int =
  var i = range.low
  while i <= range.high:
    yield i
    inc i

# used in `for X, Y in Z` expressions
iterator pairs(range: CustomRange): tuple[a: int, b: char] =
  for i in range: # uses CustomRange.items
    yield(i, char(i + ord('a')))

for i, c in CustomRange(low: 1, high: 3):
  echo c


# Iterators can also be operators
iterator `...`*[T](a: T, b: T): T =
  var res: T = T(a)
  while res <= b:
    yield res
    inc res

for i in 0...5:
  echo i


# Iterators can be inlined
iterator countTo(n: int): int =
  var i = 0
  while i <= n:
    yield i
    inc i

for i in countTo(5):
  echo i


# Iterators can be returned as closures
proc countTo(n: int): iterator(): int =
  return iterator(): int =
    var i = 0
    while i <= n:
      yield i
      inc i

let countTo10 = countTo(10)
var output = ""

echo countTo10()
echo countTo10()
echo countTo10()
echo countTo10()
echo countTo10()
echo countTo10()
while true:
  let next = countTo10()
  if finished(countTo10):
    break
  output.add($next & " ")

echo output


output = ""
let countTo9 = countTo(9)
for i in countTo9():
  output.add($i & " ")

echo output


