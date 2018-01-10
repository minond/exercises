package com.craftinginterpreters.lox

import com.craftinginterpreters.lox.TokenType._

case class Environment(var values: Map[String, Any]) {
  def define(name: String, value: Any) = {
    values = values + (name -> value)
  }

  def get(name: Token): Option[Any] = {
    values.get(name.lexeme)
  }
}
