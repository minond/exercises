type
  Dollars = distinct float

  Foo = object
    a: int

  # We need this to access the `a` property.
  AnotherFoo {. borrow: `.` .} = distinct Foo

var
  a = Dollars(20)
  b = Dollars(50)

# Since this is a distinct type, it's doesn't get any of the base type's procs.
# We can declare our own or declare that the parent's are ok with the
# `{.borrow.}` pragma.
proc `$`(a: Dollars): string {. borrow .}
proc `+`(a, b: Dollars): Dollars {. borrow .}

echo $a
echo $(a + b)
