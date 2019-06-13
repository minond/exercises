tests
testDecrement
  | c |
  c := Counter new.
  c count: 10.
  c decrement  ; decrement .
  self assert: c count equals: 8