package com.craftinginterpreters.lox

import com.craftinginterpreters.lox.TokenType.TokenType

case class Token(ttype: TokenType, lexeme: String, literal: Option[AnyRef], line: Int) {
  override def toString() = {
    s"$ttype $lexeme $literal"
  }
}
