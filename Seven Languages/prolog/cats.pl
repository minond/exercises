cat(lion).
cat(tiger).

two_cats(X, Y) :- cat(X), cat(Y).
two_different_cats(X, Y) :- cat(X), cat(Y), \+(X = Y).
