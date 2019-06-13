categories
allVisibleBlogPostsFromCategory: aString 
	^ self allVisibleBlogPosts select: [ :p | p category = aString ]