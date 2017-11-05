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

% 1.08 (**) Eliminate consecutive duplicates of list elements. If a list
% contains repeated elements they should be replaced with a single copy of the
% element. The order of the elements should not be changed.
%
% Example:
% ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [a,b,c,a,d,e]
compress([], []).
compress([H|[H|T]], Out) :- compress([H|T], Out).
compress([H|T], [H|Out]) :- compress(T, Out).

% 1.09 (**) Pack consecutive duplicates of list elements into sublists.  If a
% list contains repeated elements they should be placed in separate sublists.
%
% Example:
% ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]

% 1.10 (*) Run-length encoding of a list.  Use the result of problem 1.09 to
% implement the so-called run-length encoding data compression method.
% Consecutive duplicates of elements are encoded as terms [N,E] where N is the
% number of duplicates of the element E.
%
% Example:
% ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],[1,b],[2,c],[2,a],[1,d],[4,e]]

% 1.11 (*) Modified run-length encoding.  Modify the result of problem 1.10 in
% such a way that if an element has no duplicates it is simply copied into the
% result list. Only elements with duplicates are transferred as [N,E] terms.
%
% Example:
% ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]

% 1.12 (**) Decode a run-length encoded list.  Given a run-length code list
% generated as specified in problem 1.11. Construct its uncompressed version.

% 1.13 (**) Run-length encoding of a list (direct solution).  Implement the
% so-called run-length encoding data compression method directly. I.e. don't
% explicitly create the sublists containing the duplicates, as in problem 1.09,
% but only count them. As in problem 1.11, simplify the result list by replacing
% the singleton terms [1,X] by X.
%
% Example:
% ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]

% 1.14 (*) Duplicate the elements of a list.
% Example:
% ?- dupli([a,b,c,c,d],X).
% X = [a,a,b,b,c,c,c,c,d,d]

% 1.15 (**) Duplicate the elements of a list a given number of times.
% Example:
% ?- dupli([a,b,c],3,X).
% X = [a,a,a,b,b,b,c,c,c]
%
% What are the results of the goal:
% ?- dupli(X,3,Y).
