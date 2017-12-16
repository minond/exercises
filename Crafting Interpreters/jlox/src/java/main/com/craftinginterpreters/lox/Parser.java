package com.craftinginterpreters.lox;

import static com.craftinginterpreters.lox.TokenType.*;

import java.util.List;

class Parser {
  private static class ParseError extends RuntimeException {}

  private final List<Token> tokens;
  private int current = 0;

  Parser(List<Token> tokens) {
    this.tokens = tokens;
  }

  // expression = equality ;
  private Expr expression() {
    return equality();
  }

  // equality = comparison ( ( "!=" | "==" ) comparison )* ;
  private Expr equality() {
    Expr expr = comparison();

    while (match(BANG_EQUAL, EQUAL_EQUAL)) {
      Token operator = previous();
      Expr right = comparison();
      expr = new Expr.Binary(expr, operator, right);
    }

    return expr;
  }

  // comparison = addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
  private Expr comparison() {
    Expr expr = addition();

    while (match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      Token operator = previous();
      Expr right = addition();
      expr = new Expr.Binary(expr, operator, right);
    }

    return expr;
  }

  // addition = multiplication ( ( "-" | "+" ) multiplication )* ;
  private Expr addition() {
    Expr expr = multiplication();

    while (match(MINUS, PLUS)) {
      Token operator = previous();
      Expr right = multiplication();
      expr = new Expr.Binary(expr, operator, right);
    }

    return expr;
  }

  // multiplication = unary ( ( "/" | "*" ) unary )* ;
  private Expr multiplication() {
    Expr expr = unary();

    while (match(SLASH, STAR)) {
      Token operator = previous();
      Expr right = unary();
      expr = new Expr.Binary(expr, operator, right);
    }

    return expr;
  }

  // unary = ( "!" | "-" ) unary
  //       | primary ;
  private Expr unary() {
    if (match(BANG, MINUS)) {
      return new Expr.Unary(previous(), unary());
    } else {
      return primary();
    }
  }

  // primary = NUMBER | STRING | "false" | "true" | "nil"
  //         | "(" expression ")" ;
  private Expr primary() {
    if (match(TRUE)) {
      return new Expr.Literal(true);
    } else if (match(FALSE)) {
      return new Expr.Literal(false);
    } else if (match(NIL)) {
      return new Expr.Literal(null);
    } else if (match(STRING, NUMBER)) {
      return new Expr.Literal(previous().literal);
    } else if (match(LEFT_PAREN)) {
      Expr expr = expression();
      consume(RIGHT_PAREN, "Expecting ')' after expression.");
      return new Expr.Grouping(expr);
    }

    throw new Error("Invalid expression");
  }

  private void consume(TokenType tokenType, String message) {
    if (!match(tokenType)) {
      throw error(peek(), message);
    }
  }

  private ParseError error(Token token, String message) {
    Lox.error(token, message);
    return new ParseError();
  }

  private boolean match(TokenType... types) {
    for (TokenType type : types) {
      if (check(type)) {
        advance();
        return true;
      }
    }

    return false;
  }

  private boolean check(TokenType tokenType) {
    if (isAtEnd()) {
      return false;
    }

    return peek().type == tokenType;
  }

  private Token advance() {
    if (!isAtEnd()) {
      current++;
    }

    return previous();
  }

  private boolean isAtEnd() {
    return peek().type == EOF;
  }

  private Token peek() {
    return tokens.get(current);
  }

  private Token previous() {
    return tokens.get(current - 1);
  }
}
