### The First Commandment

When recurring on a list of atoms, _lat_, ask two questions about it: (_null?
lat_) and else.

When recurring on a number, _n_, ask two questions about it: (_zero? n_) and
else.

When recurring on a list of S-expressions, _l_, ask three questions about it:
(_null? l_), (_atom?_ (_car l_)), and else.


### The Second Commandment

Use _cons_ to build lists.


### The Third Commandment

When building a list, describe the first typical element, and the _cons_ it
onto the natural recursion.


### The Fourth Commandment

Always change at least one argument while recurring. When recurring on a list
of atoms, _lat_, use (_cdr lat_). When recurring on a number, _n_, use (_sub1
n_). And when recurring on a list of S-expressions, _l_, use (_car l_) and
(_cdr l_) if neither (_null? l_) nor (_atom?_ (_car l)) are true.

It must be changed to be closer to termination. The changing argument must be
tested in the termination condition: when using _cdr_, test termination with
_null?_ and when using _sub1_, test termination with _zero?_.


### The Fifth Commandment

When building a value with _+_, always use _0_ for the value of the terminating
line, for adding _0_ does not change the value of an addition.

When building a valud with _*_, always use _1_ for the value of the terminating
line, for multiplying by _1_ does not change the value of a multiplication.

When build a value with _cons_, always consider () for the value of the
terminating line.


### The Sixth Commandment

Simplify only after the function is correct.


### The Seventh Commandment

Recur on the _subparts_ that are of the same nature:

- On the sublists of a list.
- On the subexpressions of an arithmetic expression.


### The Eighth Commandment

Use help functions to abstract from representations.


### The Ninth Commandment

_TODO_


### The Tenth Commandment

_TODO_
