package xyz.minond.pti.lib

import xyz.minond.pti._

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.util.{Try, Failure, Success}

object CoreLoader extends Loader {
  import Interpreter._

  def define(
      name: String,
      value: Expression,
      env: Environment): (Expression, Environment) =
    (value, env.define(name, value))

  def load(env: Environment): Environment =
    env
      .define(
        "define",
        Procedure({ (args, env) =>
          args match {
            case SExpr(Identifier(name) :: args) :: body :: Nil =>
              define(name, procDef(args, body, env), env)
            case Identifier(name) :: value :: Nil => define(name, eval(value, env), env)
            case _ => (Error(Message.ERR_BAD_SYNTAX("define")), env)
          }
        })
      )
      .define(
        "set!",
        Procedure({ (args, env) =>
          args match {
            case Identifier(name) :: value :: Nil =>
              env.lookup(name) match {
                case None => (Error(Message.ERR_NO_ASSIGNMENT("set!")), env)
                case _ => define(name, eval(value, env), env)
              }
            case _ => (Error(Message.ERR_BAD_SYNTAX("set!")), env)
          }
        })
      )
      .define(
        "slurp",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Str(path) :: Nil =>
              Try { Files.readAllBytes(Paths.get(path)) } match {
                case Success(bytes) =>
                  (Str(new String(bytes, Charset.defaultCharset())), env)
                case Failure(_) =>
                  (Error(s"Missing file: $path"), env)
              }

            case _ => (Error(Message.ERR_BAD_ARGS("slurp", "string")), env)
          }
        })
      )
      .define(
        "load",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Str(path) :: Nil =>
              Try { Files.readAllBytes(Paths.get(path)) } match {
                case Success(bytes) =>
                  val code = new String(bytes, Charset.defaultCharset())
                  val (_, next) = Interpreter.eval(code, env)

                  // XXX Check for errors
                  (ok(Internal), next)

                case Failure(_) =>
                  (Error(s"Missing file: $path"), env)
              }

            case _ => (Error(Message.ERR_BAD_ARGS("load", "string")), env)
          }
        })
      )
      .define(
        "eval",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case expr :: Nil => eval(Right(expr.unQuote), env)
            case Nil => (Error(Message.ERR_ARITY_MISMATCH(1, 0)), env)
            case exprs => (Error(Message.ERR_ARITY_MISMATCH(1, exprs.size)), env)
          }
        })
      )
      .define("unquote", Procedure({ (args, env) =>
        eval(args, env) match {
          case expr :: Nil => (expr.unQuote, env)
          case _ => (Error(Message.ERR_ARITY_MISMATCH(1, args.size)), env)
        }
      }))
      .define(
        "apply",
        Procedure({ (args, env) =>
          args match {
            case fn :: args =>
              procCall(
                fn,
                eval(args, env) match {
                  case head :: tail =>
                    (head :: tail).lastOption match {
                      case Some(last) =>
                        last.unQuote match {
                          case SExpr(rest) => (head :: tail).init ++ rest
                          case _ => head :: tail
                        }

                      case None => List(head)
                    }

                  case Nil => List.empty
                },
                env
              )

            case _ => (Error(Message.ERR_BAD_SYNTAX("apply")), env)
          }
        })
      )
      .define(
        "cons",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case head :: SExpr(tail) :: Nil => (Quote(SExpr(head.unQuote :: tail)), env)
            case head :: Quote(SExpr(tail), _) :: Nil =>
              (Quote(SExpr(head.unQuote :: tail)), env)
            case head :: tail :: Nil => (Pair(head.unQuote, tail.unQuote), env)
            case _ :: _ :: _ :: Nil =>
              (Error(Message.ERR_ARITY_MISMATCH(2, args.size)), env)
            case _ => (Error(Message.ERR_INTERNAL), env)
          }
        })
      )
      .define(
        "car",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case head :: Nil =>
              head.unQuote match {
                case SExpr(head :: _) => (head, env)
                case Pair(head, _) => (head, env)
                case _ => (Error(Message.ERR_BAD_ARGS("car", "pair", "list")), env)
              }

            case _ => (Error(Message.ERR_ARITY_MISMATCH(1, args.size)), env)
          }
        })
      )
      .define(
        "cdr",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case head :: Nil =>
              head.unQuote match {
                case SExpr(_ :: tail) => (SExpr(tail), env)
                case Pair(_, tail) => (tail, env)
                case _ => (Error(Message.ERR_BAD_ARGS("cdr", "pair", "list")), env)
              }

            case _ => (Error(Message.ERR_ARITY_MISMATCH(1, args.size)), env)
          }
        })
      )
      .define(
        "cond",
        Procedure({ (args, env) =>
          args.find {
            case SExpr(cond :: _) =>
              eval(cond, env) match {
                case False => false
                case _ => true
              }

            case _ => false
          } match {
            case None => (SExpr(List.empty), env)
            case Some(SExpr(_ :: Nil)) => (SExpr(List.empty), env)
            case Some(SExpr(_ :: exprs)) => (exprs.map(eval(_, env)).last, env)
            case Some(expr) => (Error(Message.ERR_EVAL_EXPR(expr)), env)
          }
        })
      )
      .define(
        "add",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Nil => (Integer(0), env)
            case Integer(a) :: Nil => (Integer(a), env)
            case Real(a) :: Nil => (Real(a), env)
            case Real(a) :: Real(b) :: Nil => (Real(a + b), env)
            case Integer(a) :: Integer(b) :: Nil => (Integer(a + b), env)
            case Integer(a) :: Real(b) :: Nil => (Real(a.toDouble + b), env)
            case Real(a) :: Integer(b) :: Nil => (Real(a + b.toDouble), env)
            case _ :: _ :: _ :: Nil =>
              (Error(Message.ERR_ARITY_MISMATCH(args.size, 2)), env)
            case _ => (Error(Message.ERR_BAD_ARGS("add", "real", "interger")), env)
          }
        })
      )
      .define(
        "mult",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Nil => (Integer(1), env)
            case Integer(a) :: Nil => (Integer(a), env)
            case Real(a) :: Nil => (Real(a), env)
            case Real(a) :: Real(b) :: Nil => (Real(a * b), env)
            case Integer(a) :: Integer(b) :: Nil => (Integer(a * b), env)
            case Integer(a) :: Real(b) :: Nil => (Real(a.toDouble * b), env)
            case Real(a) :: Integer(b) :: Nil => (Real(a * b.toDouble), env)
            case _ :: _ :: _ :: Nil =>
              (Error(Message.ERR_ARITY_MISMATCH(args.size, 2)), env)
            case _ => (Error(Message.ERR_BAD_ARGS("mult", "real", "interger")), env)
          }
        })
      )
      .define(
        ">",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Integer(lhs) :: Integer(rhs) :: Nil => (Bool(lhs > rhs), env)
            case Real(lhs) :: Real(rhs) :: Nil => (Bool(lhs > rhs), env)
            case Real(lhs) :: Integer(rhs) :: Nil => (Bool(lhs > rhs), env)
            case Integer(lhs) :: Real(rhs) :: Nil => (Bool(lhs > rhs), env)
            case _ :: _ :: Nil =>
              (Error(Message.ERR_BAD_ARGS(">", "interger", "real")), env)
            case _ => (Error(Message.ERR_ARITY_MISMATCH(2, args.size)), env)
          }
        })
      )
      .define(
        "equal?",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case lhs :: rhs :: Nil => (Bool(lhs.unQuote == rhs.unQuote), env)
            case _ => (Error(Message.ERR_ARITY_MISMATCH(2, args.size)), env)
          }
        })
      )
      .define(
        "parse",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Str(src) :: Nil =>
              new Parser(new Tokenizer(src)).toList collect {
                case Right(expr) => expr
                case Left(err) => Error(Message.ERR_SYNTAX(err))
              } match {
                case value :: Nil => (value, env)
                case values => (SExpr(values), env)
              }

            case _ => (Error(Message.ERR_BAD_ARGS("parse", "string")), env)
          }
        })
      )
      .define(
        "type/proc/source",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Proc(args, body, _, _) :: Nil =>
              (Str(s"""(lambda (${args.mkString(" ")}) $body)"""), env)
            case Procedure(_) :: Nil => (Str("(lambda () builtin)"), env)
            case value :: Nil => (value, env)
            case _ => (Error(Message.ERR_BAD_ARGS("type/proc/source", "procedure")), env)
          }
        })
      )
      .define(
        "type/proc/arity",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Proc(args, _, _, _) :: Nil => (Integer(args.size), env)
            case _ => (Error(Message.ERR_BAD_ARGS("type/arity", "procedure")), env)
          }
        })
      )
      .define(
        "type/proc/vararg",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case (proc: Proc) :: Nil => (Bool(proc.isVariadic), env)
            case _ => (Error(Message.ERR_BAD_ARGS("type/arity", "procedure")), env)
          }
        })
      )
      .define(
        "type/name",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Error(_, _) :: Nil => (Identifier("error").quote, env)
            case False :: Nil => (Identifier("boolean").quote, env)
            case Identifier(_) :: Nil => (Identifier("identifier").quote, env)
            case Integer(_) :: Nil => (Identifier("integer").quote, env)
            case Proc(_, _, _, _) :: Nil => (Identifier("procedure").quote, env)
            case Procedure(_) :: Nil => (Identifier("procedure").quote, env)
            case Pair(_, _) :: Nil => (Identifier("pair").quote, env)
            case Quote(_, _) :: Nil => (Identifier("quote").quote, env)
            case Real(_) :: Nil => (Identifier("real").quote, env)
            case SExpr(_) :: Nil => (Identifier("sexpr").quote, env)
            case Str(_) :: Nil => (Identifier("string").quote, env)
            case True :: Nil => (Identifier("boolean").quote, env)
            case _ :: _ => (Error(Message.ERR_ARITY_MISMATCH(1, args.size)), env)
            case Nil => (Error(Message.ERR_ARITY_MISMATCH(1, 0)), env)
          }
        })
      )
      .define("begin", Procedure({ (args, env) =>
        args
          .foldLeft[(Expression, Environment)]((ok(Internal), env)) {
            case ((_, env), expr) =>
              eval(Right(expr), env)
          }
      }))
      .define("newline", Procedure({ (args, env) =>
        println("")
        (ok(Internal), env)
      }))
      .define(
        "printf",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Str(fmt) :: args =>
              Try { printf(fmt, args map (_.stringify): _*) } match {
                case Failure(ex) => (Error(s"format error: ${ex.getMessage}"), env)
                case _ => (ok(PrintfNl), env)
              }

            case _ => (Error(Message.ERR_BAD_ARGS("printf", "string")), env)
          }
        })
      )
      .define("exit", Procedure({ (args, env) =>
        System.exit(0)
        (ok(Internal), env)
      }))
      .define(
        "halt",
        Procedure({ (args, env) =>
          throw new RuntimeException(eval(args, env) match {
            case Str(msg) :: Nil => msg
            case Quote(Identifier(msg), _) :: Nil => msg
            case Quote(Str(msg), _) :: Nil => msg
            case _ => "Halt"
          })
        })
      )
      .define(
        "real->integer",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Real(num) :: Nil => (Integer(num.toInt), env)
            case _ => (Error(Message.ERR_BAD_ARGS("real->integer", "real")), env)
          }
        })
      )
      .define(
        "integer->real",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Integer(num) :: Nil => (Real(num.toDouble), env)
            case _ => (Error(Message.ERR_BAD_ARGS("integer->real", "integer")), env)
          }
        })
      )
      .define(
        "error",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Str(msg) :: Nil => (Error(msg), env)
            case Quote(Identifier(msg), _) :: Nil => (Error(msg), env)
            case Quote(Str(msg), _) :: Nil => (Error(msg), env)
            case expr => (Error(Message.ERR_INVALID_ERROR(expr)), env)
          }
        })
      )
      .define(
        "let*",
        Procedure({ (args, env) =>
          args match {
            case SExpr(defs) :: body :: Nil =>
              (eval(body, defs.foldLeft(env) {
                case (env, SExpr(Identifier(name) :: body :: Nil)) =>
                  env.define(name, eval(body, env))

                case _ => env
              }), env)

            case _ => (Error(Message.ERR_BAD_SYNTAX("let*")), env)
          }
        })
      )
      .define(
        "lambda",
        Procedure({ (args, env) =>
          args match {
            case Identifier(":lazy") :: Args(args) :: body :: Nil =>
              (procDef(args, body, env, true), env)
            case Args(args) :: body :: Nil => (procDef(args, body, env), env)
            case _ => (Error(Message.ERR_BAD_SYNTAX("lambda")), env)
          }
        })
      )
      .define(
        "dict",
        Procedure({ (args, env) =>
          type DT = Map[String, Expression]
          def joiner(exprs: List[Expression], acc: DT): Expression =
            exprs match {
              case Quote(Identifier(key), _) :: value :: rest =>
                joiner(rest, acc + (key -> value))
              case Nil => Dict(acc)
              case _ => Error(Message.ERR_BAD_ARGS("dict", "symbol", "any"))
            }

          (joiner(eval(args, env), Map()), env)
        })
      )
      .define(
        "dict-get",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Dict(vals) :: Quote(Identifier(key), _) :: Nil =>
              (vals.get(key).getOrElse(Error(s"missing key: $key")), env)
            case _ => (Error(Message.ERR_BAD_ARGS("dict-get", "dict", "symbol")), env)
          }
        })
      )
      .define(
        "dict-set",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Dict(vals) :: Quote(Identifier(key), _) :: value :: Nil =>
              (Dict(vals + (key -> value)), env)
            case _ =>
              (Error(Message.ERR_BAD_ARGS("dict-set", "dict", "symbol", "any")), env)
          }
        })
      )
}
