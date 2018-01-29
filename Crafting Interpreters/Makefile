.PHONY: build run

SCALAFLAGS = -deprecation \
			 -Xfatal-warnings \
			 -Ywarn-unused \
			 -Ywarn-unused-import \
			 -Ywarn-dead-code \
			 -Ywarn-numeric-widen

build:
	scalac $(SCALAFLAGS) src/main/scala/*.scala

tools:
	scalac $(SCALAFLAGS) src/main/scala/tool/GenerateAst.scala

run:
	scala com.craftinginterpreters.lox.Main

gen_ast:
	scala com.craftinginterpreters.lox.tool.GenerateAst src/main/scala
