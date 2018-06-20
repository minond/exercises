posts
allBlogPostsFromCategory: aString 
	^ posts select: [ :p | p category = aString ]