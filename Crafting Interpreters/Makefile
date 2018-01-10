.PHONY: build run

build:
	scalac -deprecation src/main/scala/*.scala

tools:
	scalac -deprecation src/main/scala/tool/GenerateAst.scala

run:
	scala com.craftinginterpreters.lox.Main

gen_ast:
	scala com.craftinginterpreters.lox.tool.GenerateAst src/main/scala
