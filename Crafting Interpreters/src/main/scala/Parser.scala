package com.craftinginterpreters.lox

import collection.mutable.MutableList
import scala.util.{Try, Success, Failure}
import com.craftinginterpreters.lox.TokenType._;

class Parser(tokens: MutableList[Token]) {
  class ParseError extends RuntimeException

  var current = 0

  def parse(): Either[ParseError, Expr] = {
    Try { expression() } match {
      case Failure(parseErr: ParseError) => Left(parseErr)
      case Failure(unknownErr) => throw unknownErr
      case Success(expr) => Right(expr)
    }
  }

  // expression = equality ;
  private def expression(): Expr = {
    equality()
  }

  // equality = comparison ( ( "!=" | "==" ) comparison )* ;
  private def equality(): Expr = {
    var expr = comparison()

    while (matches(BANG_EQUAL, EQUAL_EQUAL)) {
      val op = previous()
      val right = comparison()
      expr = new Expr.Binary(expr, op, right)
    }

    expr
  }

  // // comparison = addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
  private def comparison(): Expr = {
    var expr = addition()

    while (matches(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val op = previous()
      val right = addition()
      expr = new Expr.Binary(expr, op, right)
    }

    expr
  }

  // addition = multiplication ( ( "-" | "+" ) multiplication )* ;
  private def addition(): Expr = {
    var expr = multiplication()

    while (matches(MINUS, PLUS)) {
      val op = previous()
      val right = multiplication()
      expr = new Expr.Binary(expr, op, right)
    }

    expr
  }

  // multiplication = unary ( ( "/" | "*" ) unary )* ;
  private def multiplication(): Expr = {
    var expr = unary()

    while (matches(SLASH, STAR)) {
      val op = previous()
      val right = unary()
      expr = new Expr.Binary(expr, op, right)
    }

    expr
  }

  // unary = ( "!" | "-" ) unary
  //       | primary ;
  private def unary(): Expr = {
    if (matches(BANG, MINUS)) {
      new Expr.Unary(previous(), unary())
    } else {
      primary()
    }
  }

  // primary = NUMBER | STRING | "false" | "true" | "nil"
  //         | "(" expression ")" ;
  private def primary(): Expr = {
    if (matches(TRUE)) {
      new Expr.Literal(true)
    } else if (matches(FALSE)) {
      new Expr.Literal(false)
    } else if (matches(NIL)) {
      new Expr.Literal(null)
    } else if (matches(STRING, NUMBER)) {
      new Expr.Literal(previous().literal)
    } else if (matches(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expecting ')' after expression.")
      new Expr.Grouping(expr)
    } else {
      throw error(peek(), "Expecting expression.")
    }
  }

  private def synchronize(): Unit = {
    advance()

    while (!isAtEnd())
      if (previous().ttype == SEMICOLON)
        return

    peek().ttype match {
      case CLASS | FOR | FUN | IF | PRINT | RETURN | VAR | WHILE =>
        return
    }

    advance()
  }

  private def consume(ttype: TokenType, message: String) = {
    if (!matches(ttype))
      throw error(peek(), message)
  }

  private def error(token: Token, message: String): ParseError = {
    Main.error(token, message)
    new ParseError()
  }

  private def matches(types: TokenType*): Boolean = {
    for (ttype <- types)
      if (check(ttype)) {
        advance()
        return true
      }

    false
  }

  private def check(ttype: TokenType): Boolean = {
    if (isAtEnd())
      false
    else
      peek().ttype == ttype
  }

  private def advance() = {
    if (!isAtEnd())
      current += 1
    else
      previous()
  }

  private def isAtEnd() = {
    peek().ttype == EOF
  }

  private def peek(): Token = {
    tokens(current)
  }

  private def previous(): Token = {
    tokens(current - 1)
  }
}
