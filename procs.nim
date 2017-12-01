# Procs have uniform function call syntax which make this `foo(a, b)` the same
# as `a.foo(b)`. Exporting is done by added an asterisk after the function
# name (so before the generic type declaration).
proc fibonacci*(n: int): int =
  if n < 2:
    result = n
  else:
    result = fibonacci(n - 1) + fibonacci(n - 2)

echo $10.fibonacci


# Nim has function pragmas.
proc sum(x, y: int): int {. noSideEffect .} =
  x + y


# Nim lets you declare your own operators
proc `$`(a: array[2, array[2, int]]): string =
  result = ""
  for x in a:
    for y in x:
      result.add($y & ", ")
    result.add("\n")

echo $[[1, 2], [3, 4]]

proc `$$$$$`(a, b: string): string =
  return a[0] & b[high(b)]

assert "foo" $$$$$ "bar" == "fr"


# Nim has generics
proc `+`(a, b: string): string =
  a & b

# This is matched. Because it's a more specific signature? Not sure.
proc `***`*(a: string, b: int): string =
  result = ""
  for i in 0..(b * 2):
    result = result + a

proc `***`*[T](a: T, b: int): T =
  result = ""
  for i in 0..b:
    result = result + a

echo "a" *** 10
