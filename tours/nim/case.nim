# You can use strings; Sets and ranges are usable; It is required that every
# possible case is covered. Case statement are just expressions so they can be
# passed around and used like any other express.
case "charlie":
  of "alfa":
    echo "A"
  of "bravo":
    echo "B"
  of "charlie":
    echo "C"
  else:
    echo "Unrecognized letter"

case 'h':
  of 'a', 'e', 'i', 'o', 'u':
    echo "Vowel"
  of '\127'..'\255':
    echo "Unkown"
  else:
    echo "Consonant"

proc positiveOfNegative(num: int): string =
  result = case num
    of low(int) .. -1:
      "negative"
    of 0:
      "zero"
    of 1 .. high(int):
      "positive"
    else:
      "what?"

echo positiveOfNegative(-11)
