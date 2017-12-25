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

  // private void scanToken() {
  //   char c = advance();
  //
  //   switch (c) {
  //     case '(':
  //       addToken(LEFT_PAREN);
  //       break;
  //
  //     case ')':
  //       addToken(RIGHT_PAREN);
  //       break;
  //
  //     case '{':
  //       addToken(LEFT_BRACE);
  //       break;
  //
  //     case '}':
  //       addToken(RIGHT_BRACE);
  //       break;
  //
  //     case ',':
  //       addToken(COMMA);
  //       break;
  //
  //     case '.':
  //       addToken(DOT);
  //       break;
  //
  //     case '-':
  //       addToken(MINUS);
  //       break;
  //
  //     case '+':
  //       addToken(PLUS);
  //       break;
  //
  //     case ';':
  //       addToken(SEMICOLON);
  //       break;
  //
  //     case '*':
  //       addToken(STAR);
  //       break;
  //
  //     case '!':
  //       addToken(match('=') ? BANG_EQUAL : BANG);
  //       break;
  //
  //     case '=':
  //       addToken(match('=') ? EQUAL_EQUAL : EQUAL);
  //       break;
  //
  //     case '<':
  //       addToken(match('=') ? LESS_EQUAL : LESS);
  //       break;
  //
  //     case '>':
  //       addToken(match('=') ? GREATER : GREATER_EQUAL);
  //       break;
  //
  //     case '/':
  //       if (match('/')) {
  //         while (peek() != '\n' && !isAtEnd()) {
  //           advance();
  //         }
  //       } else {
  //         addToken(SLASH);
  //       }
  //
  //       break;
  //
  //     case ' ':
  //     case '\r':
  //     case '\t':
  //       break;
  //
  //     case '\n':
  //       line++;
  //       break;
  //
  //     case '"':
  //       string();
  //       break;
  //
  //     case 'o':
  //       if (peek() == 'r') {
  //         addToken(OR);
  //       }
  //
  //       break;
  //
  //     default:
  //       if (isDigit(c)) {
  //         number();
  //       } else if (isAlpha(c)) {
  //         identifier();
  //       } else {
  //         Lox.error(line, "Unexpected character: " + c);
  //       }
  //
  //       break;
  //   }
  // }
  //
  // private void identifier() {
  //   while (isAlphaNumeric(peek())) {
  //     advance();
  //   }
  //
  //   String text = source.substring(start, current);
  //   TokenType type = keywords.get(text);
  //
  //   addToken(type != null ? type : IDENTIFIER);
  // }
  //
  // private void number() {
  //   while (isDigit(peek())) {
  //     advance();
  //   }
  //
  //   if (peek() == '.' && isDigit(peekNext())) {
  //     // Eat the '.'.
  //     advance();
  //
  //     while (isDigit(peek())) {
  //       advance();
  //     }
  //   }
  //
  //   addToken(NUMBER, Double.parseDouble(source.substring(start, current)));
  // }
  //
  // private void string() {
  //   while (!isAtEnd() && peek() != '"') {
  //     if (peek() == '\n') {
  //       line++;
  //     }
  //
  //     advance();
  //   }
  //
  //   if (isAtEnd()) {
  //     Lox.error(line, "Unterminated string.");
  //     return;
  //   }
  //
  //   // Eat the closing '"'.
  //   advance();
  //
  //   String value = source.substring(start + 1, current - 1);
  //   addToken(STRING, value);
  // }

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

  private def addToken(ttype: TokenType, literal: Option[AnyRef]): Unit = {
    val text = source.substring(start, current)
    tokens += new Token(ttype, text, Some(text), line)
  }

  private def advance(): Char = {
    current += 1
    source.charAt(current - 1)
  }

  private def matches(expected: Char): Boolean = {
    if (isAtEnd() || source.charAt(current) != expected)
      return false

    current += 1
    return true
  }

  private def peek(): Char = {
    if (isAtEnd()) {
      return '\0'
    } else {
      source.charAt(current)
    }
  }

  private def peekNext(): Char = {
    if (current + 1 >= source.length) {
      return '\0'
    } else {
      return source.charAt(current + 1)
    }
  }

  private def scanTokens(): MutableList[Token] = {
    while (!isAtEnd()) {
      start = current
      // scanToken()
    }

    tokens += new Token(EOF, "", None, line)
    return tokens
  }
}
