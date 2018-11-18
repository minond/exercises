count(0, []).
count(Count, [_|Tail]) :-
  count(TailCount, Tail),
  Count is TailCount + 1.

sum(0, []).
sum(Sum, [Head|Tail]) :-
  sum(TailSum, Tail),
  Sum is TailSum + Head.

average(Avg, Ls) :-
  sum(Sum, Ls),
  count(Num, Ls),
  Avg is Sum / Num.

square(Num, Squared) :-
  Squared is Num * Num.

cubed(Num, Cubed) :-
  Cubed is Num * Num * Num.

cube_surface_area(Area, EdgeLength) :-
  square(EdgeLength, EdgeSquared),
  Area is EdgeSquared * 6.

cube_volume(Volume, EdgeLength) :-
  cubed(EdgeLength, Volume).

factorial(0, 1).
factorial(Val, Fac) :-
  Val > 0,
  V2 is Val - 1, % Why do I need this variable?? Without it I always get back 'no.'
  factorial(V2, SubFac),
  Fac is Val * SubFac.
