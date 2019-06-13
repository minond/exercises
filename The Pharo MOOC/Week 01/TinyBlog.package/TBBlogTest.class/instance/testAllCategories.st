tests
testAllCategories
	blog writeBlogPost: first.
	self assert: blog allCategories size equals: 2