# Blocks are created with indentations or `()`s.
if true:
  echo "Yes"
else:
  assert false

while false:
  assert false

block:
  echo "Yes, again"


# Don't do this.
proc square(inSeq: seq[float]): seq[float] = (
  result = newSeq[float](len(inSeq));
  for i, v in inSeq: (
    result[i] = v * v;
  )
)

echo square(@[float(1), 2, 3, 4, 5, 6, 7, 9])
