package xyz.minond.pti

import xyz.minond.pti.Interpreter.Message._
import xyz.minond.pti.lib.CoreLoader

import org.scalatest._

class CoreLoaderSpec extends FreeSpec with Matchers {
  def eval(src: String) =
    Interpreter.eval(
      new Parser(new Tokenizer(src)).toList,
      CoreLoader.load(Environment(Map())))

  "Scala Core" - {
    "error creates an error" in {
      eval("""(error "1 2 3")""")._1 should be(List(Error("1 2 3")))
    }

    "eval evaluates scalar values" in {
      eval("(eval '123)")._1 should be(List(Integer(123)))
      eval("(eval 123)")._1 should be(List(Integer(123)))
      eval("(eval '123.456)")._1 should be(List(Real(123.456)))
      eval("(eval 123.456)")._1 should be(List(Real(123.456)))
      eval("(eval '#t)")._1 should be(List(True))
      eval("(eval #t)")._1 should be(List(True))
      eval("(eval '#f)")._1 should be(List(False))
      eval("(eval #f)")._1 should be(List(False))
      eval("""(eval '"hi")""")._1 should be(List(Str("hi")))
      eval("""(eval "hi")""")._1 should be(List(Str("hi")))
    }

    "eval evaluates s-expressions" in {
      eval("(eval '(add 2 4))")._1 should be(List(Integer(6)))
    }

    "cond does not evaluate expressions in false conditions" in {
      eval("""
        (cond
          (#f this is not ok)
          (#t 'ok))
      """)._1 should be(List(Quote(Identifier("ok"))))
    }

    "cond does not evaluate exressions in false conditions" in {
      eval("""
        (cond
          (#f 'err)
          (#f 'err))
      """)._1 should be(List(SExpr(List.empty)))
    }

    "cond runs in its own scope" in {
      val env = eval("""
        (cond
          (#f (define a 1))
          (#f (define b 2))
          (#t (define c 3)))
      """)._2.vars

      (env.get("a"), env.get("b"), env.get("c")) should be((None, None, None))
    }

    "let* does not leak definitions" in {
      eval("""
        (let* ((x 1))
          2)
      """)._2.vars.get("x") should be(None)
    }

    "let* allows access to definitions" in {
      eval("""
        (let* ((x 2))
          x)
      """)._1 should be(List(Integer(2)))

      eval("""
        (let* ((x 2))
          (mult x x))
      """)._1 should be(List(Integer(4)))
    }

    "let* allows access to definitions in other definitions" in {
      eval("""
        (let* ((x 2)
               (y x))
          y)
      """)._1 should be(List(Integer(2)))

      eval("""
        (let* ((x 2)
               (y (mult x x)))
          (add y 1))
      """)._1 should be(List(Integer(5)))
    }

    "begin expressions are evaluated in their own scope" in {
      eval("""
        (define x 321)

        (begin
          (define x 1)
          (define x 12)
          (define x 123)
          (add x 1))
      """)._1 should be(List(Integer(321), Integer(124)))
    }

    "apply functions works like Racket's" in {
      eval("""
        (apply add)
      """)._1 should be(List(Integer(0)))

      eval("""
        (apply add 1 2)
      """)._1 should be(List(Integer(3)))

      eval("""
        (apply add 1 '(2))
      """)._1 should be(List(Integer(3)))

      eval("""
        (apply add '(1 2))
      """)._1 should be(List(Integer(3)))

      eval("""
        (define list (lambda (. xs) xs))
        (apply add 1 (list 2))
      """)._1(1) should be(Integer(3))

      eval("""
        (define list (lambda (. xs) xs))
        (apply add (list 1 2))
      """)._1(1) should be(Integer(3))
    }

    "dict builer and helper functions" in {
      eval("""(dict)""")._1(0) should be(Dict(Map()))
      eval("""(dict 'a)""")._1(0) should be(Error(ERR_BAD_ARGS("dict", "symbol", "any")))
      eval("""(dict 'a 'b)""")._1(0) should be(Dict(Map("a" -> Quote(Identifier("b")))))
      eval("""(dict 'a 'b 'c)""")._1(0) should be(
        Error(ERR_BAD_ARGS("dict", "symbol", "any")))
      eval("""(dict 'a "B" 'c "D")""")._1(0) should be(
        Dict(Map("a" -> Str("B"), "c" -> Str("D"))))
      eval("""(dict-get (dict 'a "B") 'a)""")._1(0) should be(Str("B"))
      eval("""(dict-get (dict 'a "B") 'b)""")._1(0) should be(Error("missing key: b"))
      eval("""(dict-set (dict 'a "B") 'c "D")""")._1(0) should be(
        Dict(Map("a" -> Str("B"), "c" -> Str("D"))))
    }
  }

  "PTI Core" - {
    "core tests run" in {
      eval("""
        (load "src/lib/core.pti")
        (define *silent-assert* #t)
        (load "src/lib/test.pti")
      """)
      true should be(true)
    }
  }
}
