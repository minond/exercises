package xyz.minond.pti

import Tokenizer.{
  DOT,
  IDENTIFIER,
  POUND,
  INTEGER,
  REAL,
  OPEN_PAREN,
  CLOSE_PAREN,
  QUOTE,
  STRING
}

import org.scalatest._

class TokenizerSpec extends FlatSpec with Matchers {
  def scan(src: String) =
    new Tokenizer(src).toList collect {
      case Right(tok) => tok
      case Left(err) => throw new Exception(err.message)
    }

  "The Tokenizer" should "handle empty input" in {
    scan("") should be(List())
    scan(" ") should be(List())
    scan("          ") should be(List())
    scan("				") should be(List())
    scan("""


    """) should be(List())
  }

  it should "skips comments" in {
    scan("""
    ; comment
    ; comment
    ; comment
    ; comment
    """) should be(List.empty)

    scan("""
    ; comment
    ; comment
    name
    ; comment
    ; comment
    """) should be(List(Token(IDENTIFIER, Some("name"))))
  }

  it should "tokenize identifiers" in {
    scan("name") should be(List(Token(IDENTIFIER, Some("name"))))
    scan("dot.dot") should be(List(Token(IDENTIFIER, Some("dot.dot"))))
    scan("dash-dash") should be(List(Token(IDENTIFIER, Some("dash-dash"))))
    scan("obj->obj") should be(List(Token(IDENTIFIER, Some("obj->obj"))))
  }

  it should "tokenize dots" in {
    scan(".") should be(List(Token(DOT)))
    scan(". . .") should be(List(Token(DOT), Token(DOT), Token(DOT)))
  }

  it should "tokenize strings" in {
    val twostrs = List(
      Token(STRING, Some("1 2 3")),
      Token(STRING, Some("4 5 6"))
    )

    scan(""""1 2 3"""") should be(List(Token(STRING, Some("1 2 3"))))
    scan(""""1 2 3""4 5 6"""") should be(twostrs)
    scan(""""1 2 3"   "4 5 6"""") should be(twostrs)
  }

  it should "tokenize strings with escaped quotes" in {
    scan(""""\"1 \"2 \"3"""") should be(List(Token(STRING, Some("""\"1 \"2 \"3"""))))
  }

  it should "tokenize invalid strings that do not end with quotes" in {
    a[Exception] should be thrownBy {
      scan(""""""")
    }

    a[Exception] should be thrownBy {
      scan(""""123""")
    }
  }

  it should "tokenize pounds" in {
    scan("#t") should be(List(Token(POUND), Token(IDENTIFIER, Some("t"))))
    scan("#f") should be(List(Token(POUND), Token(IDENTIFIER, Some("f"))))
    scan("#123") should be(List(Token(POUND), Token(INTEGER, Some("123"))))
  }

  it should "tokenize quotes" in {
    scan("'abc") should be(List(Token(QUOTE), Token(IDENTIFIER, Some("abc"))))
    scan("'123") should be(List(Token(QUOTE), Token(INTEGER, Some("123"))))

    scan("'()") should be(List(Token(QUOTE), Token(OPEN_PAREN), Token(CLOSE_PAREN)))

    scan("'(abc)") should be(
      List(
        Token(QUOTE),
        Token(OPEN_PAREN),
        Token(IDENTIFIER, Some("abc")),
        Token(CLOSE_PAREN)))
  }

  it should "tokenize integers" in {
    scan("1") should be(List(Token(INTEGER, Some("1"))))
    scan("123") should be(List(Token(INTEGER, Some("123"))))
    scan("0123456789") should be(List(Token(INTEGER, Some("0123456789"))))
    scan("9876543210") should be(List(Token(INTEGER, Some("9876543210"))))
  }

  it should "tokenize negative integers" in {
    scan("-1") should be(List(Token(INTEGER, Some("-1"))))
    scan("-123") should be(List(Token(INTEGER, Some("-123"))))
    scan("-0123456789") should be(List(Token(INTEGER, Some("-0123456789"))))
    scan("-9876543210") should be(List(Token(INTEGER, Some("-9876543210"))))
  }

  it should "tokenize real numbers" in {
    scan("0.0001") should be(List(Token(REAL, Some("0.0001"))))
    scan("1.0") should be(List(Token(REAL, Some("1.0"))))
    scan("9.999") should be(List(Token(REAL, Some("9.999"))))
    scan(".999") should be(List(Token(REAL, Some(".999"))))
  }

  it should "tokenize negative real numbers" in {
    scan("-0.0001") should be(List(Token(REAL, Some("-0.0001"))))
    scan("-1.0") should be(List(Token(REAL, Some("-1.0"))))
    scan("-9.999") should be(List(Token(REAL, Some("-9.999"))))
    scan("-.999") should be(List(Token(REAL, Some("-.999"))))
  }

  it should "tokenize parentheses" in {
    scan("()") should be(List(Token(OPEN_PAREN), Token(CLOSE_PAREN)))
    scan("((()))") should be(
      List(
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN)))
  }

  it should "tokenize content inside of parentheses" in {
    scan("(123)") should be(
      List(Token(OPEN_PAREN), Token(INTEGER, Some("123")), Token(CLOSE_PAREN)))

    scan("(((123)))") should be(
      List(
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(INTEGER, Some("123")),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN)))
  }

  it should "tokenize content separated by spaces that is inside of parentheses" in {
    scan("(+ 1 2)") should be(
      List(
        Token(OPEN_PAREN),
        Token(IDENTIFIER, Some("+")),
        Token(INTEGER, Some("1")),
        Token(INTEGER, Some("2")),
        Token(CLOSE_PAREN)))

    scan("(((+ 1 2)))") should be(
      List(
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(IDENTIFIER, Some("+")),
        Token(INTEGER, Some("1")),
        Token(INTEGER, Some("2")),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN)
      ))

    scan("(((+ 1 (* 2 3))))") should be(
      List(
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(IDENTIFIER, Some("+")),
        Token(INTEGER, Some("1")),
        Token(OPEN_PAREN),
        Token(IDENTIFIER, Some("*")),
        Token(INTEGER, Some("2")),
        Token(INTEGER, Some("3")),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN)
      ))

    scan("(lambda (h . t) t)") should be(
      List(
        Token(OPEN_PAREN),
        Token(IDENTIFIER, Some("lambda")),
        Token(OPEN_PAREN),
        Token(IDENTIFIER, Some("h")),
        Token(DOT),
        Token(IDENTIFIER, Some("t")),
        Token(CLOSE_PAREN),
        Token(IDENTIFIER, Some("t")),
        Token(CLOSE_PAREN)
      ))
  }
}
