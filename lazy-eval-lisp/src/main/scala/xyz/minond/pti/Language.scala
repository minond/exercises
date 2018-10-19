package xyz.minond.pti

trait Loader {
  def load(env: Environment): Environment
}

sealed abstract class Expression {
  override final def toString =
    this match {
      case True => "#t"
      case False => "#f"
      case Error(message, _) => s"""(error "$message")"""
      case Identifier(value) => value
      case Integer(value) => value.toString
      case Pair(a, b) => s"'($a . $b)"
      case Quote(value, _) => s"'$value"
      case Real(value) => value.toString
      case SExpr(values) => s"(${values.map(_.toString).mkString(" ")})"
      case Str(value) => s""""$value""""
      case Dict(values) => s"{${values.keys.mkString(" ")}}"
      case Procedure(_) => "#<procedure>"
      case Proc(_, _, _, delayed) =>
        if (delayed) "#<procedure...>"
        else "#<procedure>"
    }

  def stringify: String =
    toString

  def quote: Expression =
    this match {
      case expr: Quote => expr
      case expr => Quote(expr)
    }

  def unQuote: Expression =
    this match {
      case Quote(expr, _) => expr
      case expr => expr
    }
}

case class Identifier(value: String) extends Expression
case class Integer(value: Int) extends Expression
case class Pair(a: Expression, b: Expression) extends Expression
case class Real(value: Double) extends Expression
case class SExpr(values: List[Expression]) extends Expression
case class Dict(values: Map[String, Expression]) extends Expression

case class Str(value: String) extends Expression {
  override def stringify = value
}

sealed trait QuoteInfo
case object UserSpace extends QuoteInfo
case object Internal extends QuoteInfo
case object PrintfNl extends QuoteInfo
case class Quote(value: Expression, info: QuoteInfo = UserSpace) extends Expression

object Bool {
  def apply(value: Boolean): Bool =
    if (value) True
    else False

  def unapply(value: Bool): Option[Boolean] =
    if (value == True) Some(true)
    else Some(false)
}

sealed trait Bool extends Expression
case object True extends Bool
case object False extends Bool

case class Error(message: String, prev: Option[Error] = None) extends Expression {
  def stringify(prefix: String = ""): String = {
    val next = prev match {
      case Some(err) => "\n" + err.stringify(prefix + "  ")
      case None => ""
    }

    s"; ${prefix}${message}${next}"
  }
}

case class Procedure(fn: (List[Expression], Environment) => (Expression, Environment))
    extends Expression

case class Proc(
    args: List[String],
    body: Expression,
    env: Environment,
    delayed: Boolean = false)
    extends Expression {
  def scope(
      vals: List[Expression],
      local: Environment,
      global: Environment): Environment =
    zipArgVals(vals).foldLeft[Environment](local.pushBack(global)) {
      case (env: Environment, (name: String, expr: Expression)) =>
        env.define(name, expr)

      case _ => local
    }

  def zipArgVals(vals: List[Expression]): List[(String, Expression)] = {
    if (!isVariadic)
      args.zip(vals)
    else
      args.filter(_ != ".").zip(vals) ++ List(
        (
          args(args.indexOf(".") + 1),
          SExpr(vals.drop(args.indexOf(".")))
        ))
  }

  def validArity(count: Int): Boolean =
    if (!isVariadic) count == args.size
    else count >= args.indexOf(".") - 1

  def isVariadic =
    args.nonEmpty && args.contains(".") && args.last != "."
}

object Args {
  val Vararg = Identifier(".")

  def unapply(expr: Expression): Option[List[Expression]] =
    expr match {
      case SExpr(ids) => Some(ids)
      case Identifier(id) => Some(List(Vararg, Identifier(id)))
      case _ => None
    }
}

case class Environment(
    vars: Map[String, Expression],
    parent: Option[Environment] = None) {
  def define(name: String, expr: Expression) =
    Environment(vars ++ Map(name -> expr), parent)

  def pushBack(env: Environment): Environment = {
    parent match {
      case Some(par) => Environment(vars, Some(par.pushBack(env)))
      case None => Environment(vars, Some(env))
    }
  }

  def lookup(name: String): Option[Expression] = {
    (vars.get(name), parent) match {
      case (Some(expr), _) => Some(expr)
      case (None, Some(env)) => env.lookup(name)
      case (None, None) => None
    }
  }

  override def toString = {
    val text = vars.keys.toList ++ (parent match {
      case Some(env) => List(env.toString)
      case _ => List.empty
    })

    s"Environment{${text.sorted.mkString(", ")}}"
  }
}
