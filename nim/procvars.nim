import sequtils

let powersOfTwo = @[1, 2, 4, 8, 16, 32, 64, 128, 256]

# Do notation
echo(powersOfTwo.filter do (x: int) -> bool: x > 32)

# Inline proc
echo powersOfTwo.filter(proc (x: int): bool = x > 32)

# Separate proc
proc greaterThan32(x: int): bool =
  x > 32

echo powersOfTwo.filter(greaterThan32)

echo powersOfTwo
