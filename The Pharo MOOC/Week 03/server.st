| books server |

books := Dictionary new.
server := Teapot configure: {
	#defaultOutput -> #json.
	#port -> 8081.
	#debugMode -> true }.

server
	GET: '/' -> '<h1>Hi!!</h1>';
	GET: '/books' -> books;
	GET: '/books/<id:IsInteger>' -> [ :req |
		books at: (req at: #id) asString ];
	POST: '/books/<id>' -> [ :req |
		| book |
		book := {
			'author' -> (req at: #author).
			'title' -> (req at: #title).
		} asDictionary .
		books at: (req at: #id) put: book.
	].

server start.
