tests
testGettersAndSetters
	| post today |
	
	today := Date today.
	post := TBPost new
		title: 'My own post' ;
		text: 'Blog post content' ;
		date: Date today ;
		beVisible .

	self
		assert: post title equals: 'My own post' ;
		assert: post text equals: 'Blog post content' ;
		assert: post date equals: today ;
		assert: post isVisible equals: true .