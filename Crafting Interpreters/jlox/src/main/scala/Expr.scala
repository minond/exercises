/* AUTO GENERATED - DO NOT EDIT */
package com.craftinginterpreters.lox

import collection.mutable.MutableList

object Expr {
  class Binary(left: Expr, operator: Token, right: Expr) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitBinaryExpr(new Binary(left, operator, right))
    }
  }

  class Grouping(expression: Expr) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitGroupingExpr(new Grouping(expression))
    }
  }

  class Literal(value: Any) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitLiteralExpr(new Literal(value))
    }
  }

  class Unary(operator: Token, right: Expr) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitUnaryExpr(new Unary(operator, right))
    }
  }
}

abstract class Expr {
  trait Visitor[T] {
    def visitBinaryExpr (expr: Expr.Binary): T
    def visitGroupingExpr (expr: Expr.Grouping): T
    def visitLiteralExpr (expr: Expr.Literal): T
    def visitUnaryExpr (expr: Expr.Unary): T
  }
  def accept[T](visitor: Visitor[T]): T
}
