tests
testAddBlog
	blog writeBlogPost: post.
	self assert: blog size equals: 2