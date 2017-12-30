package com.craftinginterpreters.lox

import com.craftinginterpreters.lox.TokenType._

class Interpreter extends Expr.Visitor[Any] {
  override def visitBinaryExpr(expr: Expr.Binary): Any = {
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    (expr.operator, left.isInstanceOf[Double], right.isInstanceOf[Double]) match {
      case (MINUS, _, _) => left.asInstanceOf[Double] - right.asInstanceOf[Double]
      case (SLASH, _, _) => left.asInstanceOf[Double] / right.asInstanceOf[Double]
      case (STAR, _, _) => left.asInstanceOf[Double] * right.asInstanceOf[Double]
      case (PLUS, true, true) => left.asInstanceOf[Double] + right.asInstanceOf[Double]
      case (PLUS, _, _) => left.asInstanceOf[String] + right.asInstanceOf[String]

      case (GREATER, _, _) => left.asInstanceOf[Double] > right.asInstanceOf[Double]
      case (GREATER_EQUAL, _, _) => left.asInstanceOf[Double] >= right.asInstanceOf[Double]
      case (LESS, _, _) => left.asInstanceOf[Double] < right.asInstanceOf[Double]
      case (LESS_EQUAL, _, _) => left.asInstanceOf[Double] <= right.asInstanceOf[Double]

      case _ => null
    }
  }

  override def visitGroupingExpr(expr: Expr.Grouping): Any = {
    evaluate(expr.expression)
  }

  override def visitLiteralExpr(expr: Expr.Literal): Any = {
    expr.value match {
      case Some(value) => value
      case None => null
    }
  }

  override def visitUnaryExpr(expr: Expr.Unary): Any = {
    val right = evaluate(expr.right)

    expr.operator.ttype match {
      case MINUS => -right.asInstanceOf[Double]
      case BANG => !isTrouthy(right)
      case _ => null
    }
  }

  private def evaluate(expr: Expr): Any = {
    expr.accept(this)
  }

  private def isTrouthy(value: Any): Boolean = {
    if (value == null)
      return false
    else if (value.isInstanceOf[Boolean])
      return value.asInstanceOf[Boolean]
    else
      return true
  }
}
