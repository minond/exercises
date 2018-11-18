different(red, green).
different(red, blue).
different(green, red).
different(green, blue).
different(blue, red).
different(blue, green).

coloring(Alabama, Mississippi, Georgia, Tennessee, Florida) :-
  different(Alabama, Mississippi),
  different(Alabama, Georgia),
  different(Alabama, Tennessee),
  different(Alabama, Florida),
  different(Mississippi, Alabama),
  different(Mississippi, Tennessee),
  different(Georgia, Florida),
  different(Georgia, Tennessee).
