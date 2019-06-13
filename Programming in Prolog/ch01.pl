% Exercise 1.3:
%
% Given the folowing facts:
% - father(X, Y)
% - mother(X, Y)
% - male(X)
% - female(X)
% - parent(X, Y)
% - diff(X, Y)
%
% Write the following rules:
% - is_mother(X)
% - is_father(X)
% - is_son(X)
% - sister_of(X, Y) X is sister of Y
% - grandpa_of(X, y) X is grandfather of Y
% - sibling(X, Y) X is sibling of Y

father(mejer, richard).
father(richard, mark).
father(richard, andy).

mother(rebecca, richard).
mother(maggie, mark).
mother(maggie, andy).

male(richard).
male(mark).

female(maggie).
female(andy).

diff(mark, andy).
diff(andy, mark).

is_mother(X) :-
  female(X),
  mother(X, _).

is_father(X) :-
  male(X),
  father(X, _).

is_son(X) :-
  male(X),
  father(_, X).

sister_of(X, Y) :-
  female(X),
  sibling(X, Y).

grandpa_of(X, Y) :-
  father(X, Par),
  father(Par, Y).

% Exercise 1.4:
%
% Using the sister_of rule defined before, explain why it is possible for some
% object to be her own sister. How would you fix this?
%
% Is is possible because sister_of relies on sibling, which simply finds two
% objects with the same mother and father. The solution would be to add another
% goal that asserts the two matching objects are not the same. This could be
% done using fact.
sibling(X, Y) :-
  father(F, X),
  father(F, Y),
  mother(M, X),
  mother(M, Y),
  diff(X, Y),
  diff(X, Y).
