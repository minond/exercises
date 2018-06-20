categories
allCategories
	^ (self allBlogPosts collect: [ :p | p category ]) asSet