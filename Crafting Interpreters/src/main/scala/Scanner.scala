package com.craftinginterpreters.lox

import collection.mutable.MutableList
import com.craftinginterpreters.lox.TokenType._

object Scanner {
  val keywords = Map(
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE
  )
}

class Scanner(source: String) {
  var start = 0
  var current = 0
  var line = 0

  val tokens = new MutableList[Token]()

  def scanTokens(): MutableList[Token] = {
    while (!isAtEnd()) {
      start = current
      scanToken()
    }

    tokens += new Token(EOF, "", None, line)
    tokens
  }

  private def scanToken() = {
    advance() match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(RIGHT_BRACE)
      case '}' => addToken(LEFT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' => addToken(if (matches('=')) BANG_EQUAL else BANG)
      case '=' => addToken(if (matches('=')) EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if (matches('=')) LESS_EQUAL else LESS)
      case '>' => addToken(if (matches('=')) GREATER_EQUAL else GREATER)
      case ' ' | '\r' | '\t' =>
      case '\n' => line += 1
      case '"' => string()
      case 'o' => if (peek() == 'r') addToken(OR)
      case c if isDigit(c) => number()
      case c if isAlpha(c) => identifier()

      case '/' =>
        if (matches('/')) {
          while (peek() != '\n' && !isAtEnd()) {
            advance()
          }
        } else {
          addToken(SLASH)
        }

      case c =>
        Main.error(line, s"Unterminated character: $c")
    }
  }

  private def identifier() = {
    while (isAlphaNumeric(peek())) advance()

    val value = source.substring(start, current)

    if (Scanner.keywords.contains(value)) {
      addToken(Scanner.keywords(value))
    } else {
      addToken(IDENTIFIER)
    }
  }

  private def number() = {
    while (isDigit(peek())) advance()

    if (peek() == '.' && isDigit(peekNext())) {
      // Eat the '.'
      advance()

      while (isDigit(peek())) advance()
    }

    addToken(NUMBER, Some(source.substring(start, current).toDouble))
  }

  private def string() = {
    while (!isAtEnd() && peek() != '"') {
      if (peek() == '\n')
        line += 1

      advance()
    }

    if (isAtEnd()) {
      Main.error(line, "Unterminated string")
    } else {
      // Eat the closing '"'
      advance()
      addToken(STRING, Some(source.substring(start + 1, current - 1)))
    }
  }

  private def isAtEnd() = {
    current >= source.length
  }

  private def isDigit(c: Char) = {
    c >= '0' && c <= '9'
  }

  private def isAlpha(c: Char) = {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
  }

  private def isAlphaNumeric(c: Char) = {
    isDigit(c) || isAlpha(c)
  }

  private def addToken(ttype: TokenType): Unit = {
    addToken(ttype, None)
  }

  private def addToken(ttype: TokenType, literal: Option[Any]): Unit = {
    val text = source.substring(start, current)
    tokens += new Token(ttype, text, literal, line)
  }

  private def advance(): Char = {
    current += 1
    source.charAt(current - 1)
  }

  private def matches(expected: Char): Boolean = {
    if (isAtEnd() || source.charAt(current) != expected)
      return false

    current += 1
    true
  }

  private def peek(): Char = {
    if (isAtEnd())
      '\u0000'
    else
      source.charAt(current)
  }

  private def peekNext(): Char = {
    if (current + 1 >= source.length)
      '\u0000'
    else
      source.charAt(current + 1)
  }
}
