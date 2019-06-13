tests
testGettingPostsByCategory
	blog removeAllPosts ;
		writeBlogPost: first ;
		writeBlogPost: first ;
		writeBlogPost: post .
		
	self assert: (blog allBlogPostsFromCategory: 'MyPost') size equals: 2.
	self assert: (blog allBlogPostsFromCategory: 'Cat') size equals: 1