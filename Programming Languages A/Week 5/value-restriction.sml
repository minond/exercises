(*

  What does the type checker do in a scenario like this:

    val r = ref NONE
    val _ = r := SOME "hi"
    val i = 1 + valOf (!r)


  The first line (`val r = ref NONE`) results in this warning: "Warning: type
  vars not generalized because of value restriction are instantiated to dummy
  types (X1,X2,...)"

  == Value Restriction ==
  Value restriction says that polymorphic types can only be derived from value
  expressions or variables (which are also polymorphic). This prevents the
  example above from type-checking into an unsound program.

  This does make valid statements impossible as far as the type checker is
  concerned: Here's an example

    val pairWithOne =
      List.map (fn x => (x, 1))


  This should result in `fn : 'a list -> ('a * int) list but since the value is
  not a value or variable, it cannot be a polymorphic type. There are two
  solutions. First, we could add type annotations to make it non-polymorphic:

    val pairWithOne =
      List.map (fn (x : int) => (x, 1))


  Second, we could wrap this in a function which will not have the same
  restrictions:

    fun pairWithOne xs =
      List.map (fn x => (x, 1)) xs

 *)
