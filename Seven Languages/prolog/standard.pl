% Run with `trace.` for explination.
concat([], List, List).
concat([H|T1], List, [H|T2]) :-
  concat(T1, List, T2).

head([H|_], H).
tail([_|T], T).

first([H|_], H).
second([_|[S|_]], S).
third([_|[_|[T|_]]], T).

first_alt(Ls, Store) :- head(Ls, Store).
second_alt(Ls, Store) :- head(head(Ls), Store).
third_alt(Ls, Store) :- head(head(head(Ls)), Store).

range(0, [0]).
range(End, [End|Tl]) :-
  End > 0,
  Next is End - 1,
  range(Next, Tl).

nth_item(0, [H|_], H).
nth_item(Pos, [_|Ls], Item) :-
  Pos > 0,
  Next is Pos - 1,
  nth_item(Next, Ls, Item).
