testing
testIncrementDecrement

	| c |
	c := MyOtherCounter new.
	c count: 10 ;
		increment ;
		increment ;
		decrement .
		
	self assert: c count equals: 11