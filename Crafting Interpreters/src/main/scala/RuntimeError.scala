package com.craftinginterpreters.lox

class RuntimeError(val token: Token, msg: String) extends RuntimeException(msg)
