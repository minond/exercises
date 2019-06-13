tests
testGettingAllPosts
	blog writeBlogPost: post.
	self assert: blog allBlogPosts size equals: 2