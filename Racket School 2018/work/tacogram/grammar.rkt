#lang brag

taco-program : taco-leaf+

taco-leaf : /ws{0,} /open [taco | not-a-taco]{7} /close /ws{0,}

taco : /"%"
not-a-taco : /"#" /"$"

open : "#"
close : "$"

ws : "\n" | " "