Object subclass: #TBPost
	instanceVariableNames: 'title text date category visible'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TinyBlog'!
!TBPost commentStamp: 'MarcosMinond 5/6/2018 18:55' prior: 0!
A blog post is described by 5 instance variables:

- title: post title
- text: post text
- date: date of writing
- category: name of the category of the post
- visible: is the post publicly visible or not?!


!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 19:06'!
category
	^ category! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 19:23'!
isUnclassified
	^ self category = TBPost unclassifiedTag .! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 19:06'!
date
	^ date! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 19:07'!
category: categoryObj
	category := categoryObj ! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 19:07'!
beVisible
	visible := true! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 19:03'!
text
	^ text! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 18:59'!
title: aString
	title := aString! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 19:07'!
notVisible
	visible := false! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 18:55'!
title
	^ title! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 19:07'!
isVisible
	^ visible! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 19:03'!
text: aString
	text := aString! !

!TBPost methodsFor: 'accessor' stamp: 'MarcosMinond 5/6/2018 19:06'!
date: aDate
	date := aDate! !


!TBPost methodsFor: 'initialization' stamp: 'MarcosMinond 5/6/2018 19:15'!
initialize
	title := ''.
	text := ''.
	date := Date today.
	category := TBPost unclassifiedTag.
	self notVisible.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

TBPost class
	instanceVariableNames: ''!

!TBPost class methodsFor: 'creating' stamp: 'MarcosMinond 5/6/2018 19:19'!
title: aTitle text: aText category: aCategory
	^ (self title: aTitle text: aText)
		category: aCategory ;
		yourself.! !

!TBPost class methodsFor: 'creating' stamp: 'MarcosMinond 5/6/2018 19:18'!
title: aTitle text: aText
	^ self new
		title: aTitle ;
		text: aText ;
		yourself.! !


!TBPost class methodsFor: 'category' stamp: 'MarcosMinond 5/6/2018 19:14'!
unclassifiedTag
	^ 'Unclassified'! !


TestCase subclass: #TBPostTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TinyBlog'!

!TBPostTest methodsFor: 'tests' stamp: 'MarcosMinond 5/6/2018 19:09'!
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
		assert: post isVisible equals: true .! !
