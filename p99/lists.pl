% 1.01 (*) Find the last element of a list.
% Example:
% ?- my_last(X,[a,b,c,d]).
% X = d
last_alt([H|[]], H).
last_alt([_|T], L) :- last_alt(T, L).

% 1.02 (*) Find the last but one element of a list.
second_to_last([STL|[_|[]]], STL).
second_to_last([_|T], STL) :- second_to_last(T, STL).

% 1.03 (*) Find the K'th element of a list.
% The first element in the list is number 1.
% Example:
% ?- element_at(X,[a,b,c,d,e],3).
% X = c
element_at(H, [H|_], 1).
element_at(Val, [_|Tl], Pos) :-
  Pos > 1,
  Next is Pos - 1,
  element_at(Val, Tl, Next).

% 1.04 (*) Find the number of elements of a list.
count([], 0).
count([_|Tl], C) :-
  count(Tl, TailLen),
  C is TailLen + 1.

% 1.05 (*) Reverse a list.
reverse_list([], []).
reverse_list([H|T], R) :-
  reverse_list(T, Rev),
  append(Rev, [H], R).

% 1.06 (*) Find out whether a list is a palindrome.
% A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
palindrome(Ls) :-
  reverse_list(Ls, Rev),
  Ls = Rev.

% 1.07 (**) Flatten a nested list structure.  Transform a list, possibly holding
% lists as elements into a 'flat' list by replacing each list with its elements
% (recursively).
%
% Example:
% ?- my_flatten([a, [b, [c, d], e]], X).
% X = [a, b, c, d, e]
%
% Hint: Use the predefined predicates is_list/1 and append/3
flatten_alt([], []).
flatten_alt([H|T], Flat) :-
  \+(is_list(H)),
  flatten_alt(T, Th),
  append([H], Th, Flat).
flatten_alt([H|T], Flat) :-
  is_list(H),
  flatten_alt(H, Fh),
  flatten_alt(T, Th),
  append(Fh, Th, Flat).
