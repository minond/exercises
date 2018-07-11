#lang brag

taco-program : taco-leaf+
taco-leaf : (taco | not-a-taco){7}
taco : /"%"
not-a-taco : /"#$"