Prolog is **homoiconic**. Data is expressed as terms defined in a
knowledgebase. Code is also expressed as terms defined in a knowledgebase. So
code and data are the same thing. We can even say there is no code - only
entries in the knowledgebase.

Prolog does **proof search** by a method called Selective Linear Definite
resolution.

Prolog predicate arguments bynd by unifying. This means that the same argument
can either be used to pass or return a value - a property called multimodality.
The Prolog predicate `append/3`, which appends two lists to make a thirt, can
also be used to check if two lists are parts of a third, to get the prefix, the
suffix, to find all partitions on a list, and so on.
