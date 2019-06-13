(*

We're only allowed to `structure Foo :> BAR` given:

- Every non-abstract type in BAR is provided in Foo, as specified.

- Every abstract type in BAR is provided in Foo in some way
  - Can be a datatype of a type synonym

- Every val-binding in BAR is provided in Foo, possibly with a more general
  and/or less abstract internal type

- Every exception in BAR is provided in Foo

*)
