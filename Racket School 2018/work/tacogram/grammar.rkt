#lang brag

taco-program : /ws{0,} taco-leaf+ /ws{0,}

taco-leaf : /open (taco | not-a-taco){7} /close

taco : /"%"
not-a-taco : /"#" /"$"

open : "#"
close : "$"

ws : "\n" | " "