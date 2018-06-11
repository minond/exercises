tests
testIncrement
  | c |
  c := Counter new.
  c count: 10.
  c increment ; increment.
  self assert: c count equals: 12