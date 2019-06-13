tests
testGettingVisiblePostsByCategory
	blog removeAllPosts ;
		writeBlogPost: first ;
		writeBlogPost: first ;
		writeBlogPost: post .
		
	self assert: (blog allVisibleBlogPostsFromCategory: 'MyPost') size equals: 0.
	self assert: (blog allVisibleBlogPostsFromCategory: 'Cat') size equals: 1