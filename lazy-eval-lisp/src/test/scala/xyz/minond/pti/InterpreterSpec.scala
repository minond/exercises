package xyz.minond.pti

import org.scalatest._

class InterpreterSpec extends FreeSpec with Matchers {
  def eval(src: String) =
    Interpreter.eval(new Parser(new Tokenizer(src)).toList, Environment(Map()))

  "The Interpreter" - {
    "handles empty input" in {
      eval("")._1 should be(List())
      eval(" ")._1 should be(List())
      eval("          ")._1 should be(List())
      eval("				")._1 should be(List())
      eval("""


  """)._1 should be(List())
    }

    "evaluates integer expressions" in {
      eval("0")._1 should be(List(Integer(0)))
      eval("1")._1 should be(List(Integer(1)))
      eval("123")._1 should be(List(Integer(123)))
      eval("1 2 3")._1 should be(List(Integer(1), Integer(2), Integer(3)))
      eval("""
      1
      2
      3""")._1 should be(List(Integer(1), Integer(2), Integer(3)))
    }

    "evaluates real number expressions" in {
      eval("0.0")._1 should be(List(Real(0)))
      eval("1.1")._1 should be(List(Real(1.1)))
      eval("12.3")._1 should be(List(Real(12.3)))
      eval("1.0 2.0 3.0")._1 should be(List(Real(1.0), Real(2.0), Real(3.0)))
      eval("""
      .01
      .02
      .03""")._1 should be(List(Real(0.01), Real(0.02), Real(0.03)))
    }

    "evaluates boolean expressions" in {
      eval("#t")._1 should be(List(True))
      eval("#f")._1 should be(List(False))
      eval("#t #f #t #f")._1 should be(List(True, False, True, False))
      eval("""
      #f
      #t
      #f
      #t""")._1 should be(List(False, True, False, True))
    }

    "evaluates string expressions" in {
      eval(""""hi"""")._1 should be(List(Str("hi")))
      eval(""""1""2""3"""")._1 should be(List(Str("1"), Str("2"), Str("3")))
      eval(""""1" "2" "3"""")._1 should be(List(Str("1"), Str("2"), Str("3")))
    }

    "evaluates quoted expressions" in {
      eval("'abc")._1 should be(List(Quote(Identifier("abc"))))
      eval("'(1 2 3)")._1 should be(
        List(Quote(SExpr(List(Integer(1), Integer(2), Integer(3))))))
      eval("'123")._1 should be(List(Integer(123)))
      eval("'123.456")._1 should be(List(Real(123.456)))
      eval("'#t")._1 should be(List(True))
      eval("'#f")._1 should be(List(False))
    }
  }
}
