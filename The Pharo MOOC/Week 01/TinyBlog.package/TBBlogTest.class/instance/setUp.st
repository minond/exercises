running
setUp
	blog := TBBlog current.
	blog removeAllPosts.
	
	first := TBPost title: 'My first post' text: 'This is the content for my first post' category: 'MyPost'.

	post := (TBPost title: 'My other post' text: 'Content for second post' category: 'Cat') beVisible.
	blog writeBlogPost: post.