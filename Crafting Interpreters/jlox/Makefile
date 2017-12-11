.PHONY: build run

all: build run

build:
	javac -d build src/java/main/com/craftinginterpreters/lox/*.java

run:
	java -cp ./build/ com/craftinginterpreters/lox/Lox

lint:
	java -jar $$HOME/.jars/google-java-format-1.5-all-deps.jar -r $$(find . -name '*.java' ! -size 0)
