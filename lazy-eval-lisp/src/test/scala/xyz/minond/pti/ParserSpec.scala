package xyz.minond.pti

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  def parse(src: String) =
    new Parser(new Tokenizer(src)).toList collect {
      case Right(expr) => expr
      case Left(err) => throw new Exception(err.message)
    }

  "The Parser" should "handle empty input" in {
    parse("") should be(List())
    parse(" ") should be(List())
    parse("          ") should be(List())
    parse("				") should be(List())
    parse("""


    """) should be(List())
  }

  it should "parse valid boolean values" in {
    parse("#f") should be(List(False))
    parse("#t") should be(List(True))
  }

  it should "fail to parse an invalid boolean value" in {
    a[Exception] should be thrownBy {
      parse("#123")
    }
  }

  it should "parse valid integer numbers" in {
    parse("1") should be(List(Integer(1)))
    parse("123") should be(List(Integer(123)))
    parse("1 2 3") should be(List(Integer(1), Integer(2), Integer(3)))
  }

  it should "parse valid real numbers" in {
    parse("1.0") should be(List(Real(1.0)))
    parse("1.23") should be(List(Real(1.23)))
    parse("0.123") should be(List(Real(0.123)))
    parse("0.1 0.2 0.3") should be(List(Real(0.1), Real(0.2), Real(0.3)))
  }

  it should "parse valid identifiers" in {
    parse("+") should be(List(Identifier("+")))
    parse("=") should be(List(Identifier("=")))
    parse("mod") should be(List(Identifier("mod")))
    parse("number->string") should be(List(Identifier("number->string")))
    parse("package.name") should be(List(Identifier("package.name")))
  }

  it should "parse valid s-expressions" in {
    parse("()") should be(List(SExpr(List())))
    parse("(())") should be(List(SExpr(List(SExpr(List())))))
    parse("((()))") should be(List(SExpr(List(SExpr(List(SExpr(List())))))))

    parse("((() () ()))") should be(
      List(
        SExpr(
          List(
            SExpr(
              List(
                SExpr(List()),
                SExpr(List()),
                SExpr(List()),
              ))))))

    parse("(+ 1 2)") should be(List(SExpr(List(Identifier("+"), Integer(1), Integer(2)))))

    parse("(lambda (h . t) t)") should be(
      List(
        SExpr(
          List(
            Identifier("lambda"),
            SExpr(
              List(
                Identifier("h"),
                Identifier("."),
                Identifier("t"),
              )),
            Identifier("t"),
          ))))
  }

  it should "fail to parse an s-expressions with invalid contents" in {
    a[Exception] should be thrownBy {
      parse("(#invalid)")
    }
  }

  it should "parse valid quoted expressions" in {
    parse("'a") should be(List(Quote(Identifier("a"))))
    parse("'abc") should be(List(Quote(Identifier("abc"))))
    parse("'1") should be(List(Quote(Integer(1))))
    parse("'123") should be(List(Quote(Integer(123))))
    parse("'()") should be(List(Quote(SExpr(List()))))
    parse("'#f") should be(List(Quote(False)))
  }

  it should "fail to parse invalid quoted expressions" in {
    a[Exception] should be thrownBy {
      parse("'")
    }

    a[Exception] should be thrownBy {
      parse("'#x")
    }
  }

  it should "parse valid strings" in {
    parse(""""hi"""") should be(List(Str("hi")))
    parse(""""1 2 3"""") should be(List(Str("1 2 3")))
    parse(""""1""2""3"""") should be(List(Str("1"), Str("2"), Str("3")))
    parse(""""1" "2" "3"""") should be(List(Str("1"), Str("2"), Str("3")))
  }
}
