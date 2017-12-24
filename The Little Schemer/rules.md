# The Five Rules


### The Law of Car

The privitive `car` is defined only for non-empty lists.


### The Law of Cdr

The primitive `cdr` is defined only for non-empty lists. The `cdr` of any
non-empty list is always another list.


### The Law of Cons

The primitive `cons` takes two arguments. The second argument to `const` must
be a list. The result is a list.


### The Law of Null?

The primitive `null?` is defined only for lists.


### The Law of Eq?

The primitive `eq?` takes two arguments. Each must be a non-numeric atom.
