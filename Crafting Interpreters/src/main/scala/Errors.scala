package com.craftinginterpreters.lox

case class RuntimeError(val token: Token, msg: String)
  extends RuntimeException(msg)

case class ParseError(val token: Token)
  extends RuntimeException("Parsing error")
