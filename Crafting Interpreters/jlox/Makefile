.PHONY: build run

SCALAFLAGS = -deprecation \
			 -Xfatal-warnings \
			 -Ywarn-unused \
			 -Ywarn-unused-import \
			 -Ywarn-dead-code \
			 -Ywarn-numeric-widen

build:
	scalac $(SCALAFLAGS) src/main/scala/*.scala

run: build
	scala com.craftinginterpreters.lox.Main

fmt:
	scalafmt src

gen:
	scalac $(SCALAFLAGS) src/main/scala/tool/GenerateAst.scala
	scala com.craftinginterpreters.lox.tool.GenerateAst src/main/scala
