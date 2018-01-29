package com.craftinginterpreters.lox

case class Environment(var values: Map[String, Any]) {
  def define(name: String, value: Any) = {
    values = values + (name -> value)
  }

  def get(name: Token): Option[Any] = {
    values.get(name.lexeme)
  }
}
