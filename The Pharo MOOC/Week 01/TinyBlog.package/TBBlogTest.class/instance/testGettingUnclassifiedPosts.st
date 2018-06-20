tests
testGettingUnclassifiedPosts
	blog removeAllPosts .
	self assert: (blog allBlogPosts select: [ :p | p isUnclassified ]) size equals: 0