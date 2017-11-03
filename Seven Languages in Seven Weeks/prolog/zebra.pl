% Source https://en.wikipedia.org/wiki/Zebra_Puzzle
% Thanks to https://swish.swi-prolog.org/example/houses_puzzle.pl for the next rule
next(A, B, Ls) :- append(_, [A,B|_], Ls).
next(A, B, Ls) :- append(_, [B,A|_], Ls).

houses(Hs) :-
  % 1. There are five houses.
  length(Hs, 5),
  % 2. The Englishman lives in the red house.
  member(house(red, _, _, englishman, _), Hs),
  % 3. The Spaniard owns the dog.
  member(house(_, dog, _, spaniard, _), Hs),
  % 4. Coffee is drunk in the green house.
  member(house(green, _, coffee, _, _), Hs),
  % 5. The Ukrainian drinks tea.
  member(house(_, _, tea, ukrainian, _), Hs),
  % 6. The green house is immediately to the right of the ivory house.
  next(house(green, _, _, _, _), house(ivory, _, _, _, _), Hs),
  % 7. The Old Gold smoker owns snails.
  member(house(_, snails, _, _, oldgold), Hs),
  % 8. Kools are smoked in the yellow house.
  member(house(yellow, _, _, _, kools), Hs),
  % 9. Milk is drunk in the middle house.
  Hs = [_, _, house(_, _, milk, _, _), _, _],
  % 10. The Norwegian lives in the first house.
  Hs = [house(_, _, _, norwegian, _), _, _, _, _],
  % 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
  next(house(_, _, _, _, chesterfields), house(_, fox, _, _, _), Hs),
  % 12. Kools are smoked in the house next to the house where the horse is kept.
  next(house(_, _, _, _, kools), house(_, horse, _, _, _), Hs),
  % 13.The Lucky Strike smoker drinks orange juice.
  member(house(_, _, orangejuice, _, luckystrike), Hs),
  % 14.The Japanese smokes Parliaments.
  member(house(_, _, _, japanese, parliaments), Hs),
  % 15. The Norwegian lives next to the blue house.
  next(house(_, _, _, norwegian, _), house(blue, _, _, _, _), Hs).


% Now, who drinks water? Who owns the zebra?
who_drinks_water(Nationality) :- houses(Hs), member(house(_, _, water, Nationality, _), Hs).
who_owns_the_zebra(Nationality) :- houses(Hs), member(house(_, zebra, _, Nationality, _), Hs).

% In the interest of clarity, it must be added that each of the five houses is
% painted a different color, and their inhabitants are of different national
% extractions, own different pets, drink different beverages and smoke
% different brands of American cigarets [sic]. One other thing: in statement 6,
% right means your right.

%   - Life International, December 17, 1962
