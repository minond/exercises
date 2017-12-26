/* AUTO GENERATED - DO NOT EDIT */
package com.craftinginterpreters.lox

import collection.mutable.MutableList

abstract class Expr {
  trait Visitor[T] {
    def visitBinaryExpr (expr: Binary): T
    def visitGroupingExpr (expr: Grouping): T
    def visitLiteralExpr (expr: Literal): T
    def visitUnaryExpr (expr: Unary): T
  }

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

  class Literal(value: Object) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitLiteralExpr(new Literal(value))
    }
  }

  class Unary(operator: Token, right: Expr) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitUnaryExpr(new Unary(operator, right))
    }
  }

  def accept[T](visitor: Visitor[T]): T
}
