(*

Two functions are equivalent if they have the same "observable behavior" no
matter how they are used anywhere in any program. Given equivalent arguments,
 they:

  - Produce equivalent results
  - Have the same (non-)termination behavior
  - Mutate (non-local) memory in the same way
  - Do the same input/output
  - Raise the same exceptions

It is easier to have equivalence if:

  - There are fewer arguments and abstractions to those arguments
  - We avoid side-effects: mutation, i/o, exceptions

*)
