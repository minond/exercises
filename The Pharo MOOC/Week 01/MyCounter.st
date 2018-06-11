Object subclass: #Counter
	instanceVariableNames: 'count'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MyCounter'!
!Counter commentStamp: 'MarcosMinond 5/6/2018 17:03' prior: 0!
"I'm a simple counter supporting incrementing and decrementing a value."

API:
- increment decrement
- count

|c|
c := Counter new.
c increment ; increment
c count = 2!


!Counter methodsFor: 'printing' stamp: 'MarcosMinond 5/6/2018 18:03'!
printOn: aStream
  super printOn: aStream.
  aStream nextPutAll: ' with a value of: ', count printString ; cr! !


!Counter methodsFor: 'operations' stamp: 'MarcosMinond 5/6/2018 17:53'!
decrement
	count := count - 1.! !

!Counter methodsFor: 'operations' stamp: 'MarcosMinond 5/6/2018 17:52'!
increment
	count := count + 1.! !


!Counter methodsFor: 'accessing' stamp: 'MarcosMinond 5/6/2018 17:20'!
count: aNumber
  count := aNumber! !

!Counter methodsFor: 'accessing' stamp: 'MarcosMinond 5/6/2018 17:19'!
count
  ^ count! !


!Counter methodsFor: 'initialization' stamp: 'MarcosMinond 5/6/2018 18:07'!
initialize
  super initialize.
  count := 0! !


TestCase subclass: #CounterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MyCounter'!

!CounterTest methodsFor: 'as yet unclassified' stamp: 'MarcosMinond 5/6/2018 18:04'!
testInitialValue
  self assert: Counter new count equals: 0! !


!CounterTest methodsFor: 'tests' stamp: 'MarcosMinond 5/6/2018 17:27'!
testCounterSetterAndGetter
  | c |
  c := Counter new.
  c count: 21 * 2.
  self assert: c count equals: 42! !

!CounterTest methodsFor: 'tests' stamp: 'MarcosMinond 5/6/2018 17:54'!
testDecrement
  | c |
  c := Counter new.
  c count: 10.
  c decrement  ; decrement .
  self assert: c count equals: 8! !

!CounterTest methodsFor: 'tests' stamp: 'MarcosMinond 5/6/2018 17:53'!
testIncrement
  | c |
  c := Counter new.
  c count: 10.
  c increment ; increment.
  self assert: c count equals: 12! !
