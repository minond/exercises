# Quoted strings
echo "Words fdsafdsafd"
echo """
<html>
  <head>
    <title>Hi</title>
  </head>
</html>"""

# Raw strings
echo r".""."

proc re(s: string): string =
  result = ""
  result.add("hi ")
  result.add(s)
  result.add(" bye")

# Proc strings which turn `foo"123"` into `foo(r"123")`
echo re"Marcos"
