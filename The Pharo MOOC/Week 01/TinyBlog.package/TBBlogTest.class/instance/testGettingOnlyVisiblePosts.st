tests
testGettingOnlyVisiblePosts
	blog writeBlogPost: first.
	self assert: blog allBlogPosts size equals: 2.
	self assert: blog allVisibleBlogPosts size equals: 1