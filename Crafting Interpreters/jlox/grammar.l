program        = declaration* EOF ;

declaration    = varDecl
               | statement ;

statement      = printStmt
               | exprStmt ;

varDecl        = "var" IDENTIFIER ( "=" expression )? ";" ;

printStmt      = "print" expression ";" ;

exprStmt       = expression ";" ;

expression     = equality ;

equality       = comparison ( ( "!=" | "==" ) comparison )* ;

comparison     = addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;

addition       = multiplication ( ( "-" | "+" ) multiplication )* ;

multiplication = unary ( ( "/" | "*" ) unary )* ;

unary          = ( "!" | "-" ) unary
               | primary ;

primary        = "true" | "false" | "null" | "this"
               | NUMBER | STRING
               | "(" expression ")"
               | IDENTIFIER ;
