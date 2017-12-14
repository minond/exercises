.PHONY: build run

all: build run

build:
	javac -d build src/java/main/com/craftinginterpreters/lox/*.java
	javac -d build src/java/main/com/craftinginterpreters/tool/*.java

run:
	java -cp ./build/ com/craftinginterpreters/lox/Lox

gen:
	java -cp ./build/ com/craftinginterpreters/tool/GenerateAst "src/java/main/com/craftinginterpreters/lox/"
	java -jar $$HOME/.jars/google-java-format-1.5-all-deps.jar -r src/java/main/com/craftinginterpreters/lox/Expr.java

lint:
	java -jar $$HOME/.jars/google-java-format-1.5-all-deps.jar -r $$(find . -name '*.java' ! -size 0)
