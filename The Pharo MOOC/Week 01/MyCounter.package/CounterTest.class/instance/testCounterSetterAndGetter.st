tests
testCounterSetterAndGetter
  | c |
  c := Counter new.
  c count: 21 * 2.
  self assert: c count equals: 42