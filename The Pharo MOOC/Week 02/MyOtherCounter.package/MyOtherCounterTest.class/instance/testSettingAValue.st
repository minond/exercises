testing
testSettingAValue

	| c |
	c := MyOtherCounter new.
	c count: 10.
	self assert: c count equals: 10