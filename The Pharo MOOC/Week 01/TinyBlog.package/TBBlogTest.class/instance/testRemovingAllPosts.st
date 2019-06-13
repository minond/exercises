tests
testRemovingAllPosts
	blog removeAllPosts.
	self assert: blog size equals: 0