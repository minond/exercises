/* AUTO GENERATED - DO NOT EDIT */
package com.craftinginterpreters.lox


abstract class Expr {
  def accept[T](visitor: Expr.Visitor[T]): T
}

object Expr {
  trait Visitor[T] {
    def visitBinaryExpr (expr: Binary): T
    def visitGroupingExpr (expr: Grouping): T
    def visitLiteralExpr (expr: Literal): T
    def visitUnaryExpr (expr: Unary): T
    def visitVariableExpr (expr: Variable): T
  }

  class Binary(val left: Expr, val operator: Token, val right: Expr) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitBinaryExpr(new Binary(left, operator, right))
    }
  }

  class Grouping(val expression: Expr) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitGroupingExpr(new Grouping(expression))
    }
  }

  class Literal(val value: Any) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitLiteralExpr(new Literal(value))
    }
  }

  class Unary(val operator: Token, val right: Expr) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitUnaryExpr(new Unary(operator, right))
    }
  }

  class Variable(val name: Token) extends Expr {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitVariableExpr(new Variable(name))
    }
  }
}
