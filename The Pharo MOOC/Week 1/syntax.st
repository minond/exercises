exampleWithNumber: x
  "A method that illustrates every part of Smalltalk method syntax"
  <menu>
  |y|
  true & false not & (nil isNil) isFalse: [self halt].
  y := self size + super size.
  #($a #a 'a' 1 1.0)
    do: [ :each | Transcript
      show: (each class name);
      show: (each printString);
      show: ' '].
  ^ x < y
