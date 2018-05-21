### Syntax

Label                  | Example                | Notes
-----------------------|------------------------|--------------------------------
Comment                | `"a comment"`          |
Character              | `$c`, `$#`, `$@`       |
String                 | `'hi'`, `'hi there'`   |
Symbol                 | `#a`, `#one`, `#+`     |
Array                  | `#(1 2 3)`             |
Integer                | `1`, `2r0001`          |
Real                   | `1.1`, `2.2e7`         |
Point                  | `20@10`                |
Boolean                | `true`, `false`        | instance of `True` and `False`
Undefined              | `nil`                  | instance of `UndefinedObject`


```smalltalk
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
```


Expressions are separated by periods. Periods are separators and not
terminators, as such they do not need to be placed after every line:

```smalltalk
Transcript cr.
Transcript show: 1.
Transcript show: 2
```


Cascades, syntax to send all messages to the same receiver,

```smalltalk
ZnClient new
  url: 'https://...';
  queryAt: 'title' put: 'Pharo';
  queryAt: 'action' put: 'edit';
  get
```

```smalltalk
"These two examples do the same thing"

| c |
c := OrderedCollection new.
c add: 1.
c add: 2

| c |
c := OrderedCollection new
  add: 1;
  add: 2
```


### Local variables

Local variables are defined inside of pipes:

```smalltalk
| a b c |
a := 1
b := 1
c := a + b
```


#### Blocks

Blocks are normal values. Calling a block is done with the `value` message
which takes the parameters that are passed to the block. Blocks that take
multiple arguments are with `value:value:...:value:` messages. Blocks can have
returns, `^`, which exit the method where the block is defined.

```smalltalk
fct := [ :x | x * x + 3 ]

1 to: 4 do: [ :i | Transcript << i ]
> 1
> 2
> 3
> 4

#(1 2 -4 -86)
  do: [ :x | Transcript show: x abs printString ; cr ]
> 1
> 2
> 4
> 86
```

```smalltalk
| add add2 |
add := [ :x :y | x + y ].
add2 := [ :x | x + 2 ].

add value: 2 value: 4.
> 6

add2 value: 5.
> 7
```


#### Classes

- A class is defined by sending a message to its superclass
- Classes are defined inside packages
- Methods are public
- By default a method returns the receiver, `self`
- Class methods are just methods of the class side


```smalltalk
Object subclass: #Point
  instanceVariableNames: 'x y'
  classVariableNames: ''
  package: ''
```


#### Methods

```smalltalk
factorial
  "Answer the factorial of the receiver."

  self = 0 ifTrue: [^ 1].
  self > 0 ifTrue: [^ elf * (self - 1) factorial].
  self error: 'Negative numbers are not allowed'
```


### Messages

Messages can be sent to any object, including classes: `1 class`, `Date today`

Unary messages: `9 squared`, `Date today`
Binary messages: `1+2`, `3@4`. One, two, or three characters taken from `+ - /
\ * ~ < > = @ % | & ! ? ,`

Keyword message: `receiver key1: arg1 key2: arg2`, `2 between: 10 and: 20`.
Where `anObject key1: arg1 key2: arg2` is equivalent to
`anObject.key1key2(arg1, arg2)` in another language.


Message priority: (Msg) > Unary > Binary > Keyword. When at the same level, we
execute from left to right. There is no mathematical precedence.
  - execute () first,
  - then unary,
  - then binary,
  - and finally Keyword messages


```text
Unary: Node new
       |    |
       |    |
       |    +-- Message
       |
       +-- Receiver
```

```text
Binary: 1+2, 3@4
        |||  |||
        |||  ||+-- Argument
        |||  ||
        |||  |+-- Message
        |||  |
        |||  +-- Receiver
        |||
        |||
        |||
        ||+-- Argument
        ||
        |+-- Message
        |
        +-- Receiver
```

```text
Keywords: 2 between: 10 and: 20
          | |---------| |-----|
          |      |         |
          |      |         +-- Keywords
          |      |
          |      +-- Keyword
          |
          +-- Receiver
```

```smalltalk
SmallInteger selectors select: #isUnary
  #(#printString #even #asCharacter #highBitOfMagnitude #sqrt #identityHash
  #lowBit #shallowCopy #hashMultiply #highBitOfPositiveReceiver
  #largeIdentityHash #nextInstance #basicIdentityHash #isLarge
  #decimalDigitLength #digitLength #threeDigitName #highBit #bitStringLength
  #as31BitSmallInt #deepCopy #asFloat #nextObject #sizeInMemory #odd #hash)

SmallInteger selectors select: #isBinary
  #(#'~=' #= #/ #'//' #'\\' #- #+ #'>=' #< #'<=' #> #*)
```
