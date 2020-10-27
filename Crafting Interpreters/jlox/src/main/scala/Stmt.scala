/* AUTO GENERATED - DO NOT EDIT */
package com.craftinginterpreters.lox

abstract class Stmt {
  def accept[T](visitor: Stmt.Visitor[T]): T
}

object Stmt {
  trait Visitor[T] {
    def visitExpressionStmt(stmt: Expression): T
    def visitPrintStmt(stmt: Print): T
    def visitVarStmt(stmt: Var): T
  }

  case class Expression(val expression: Expr) extends Stmt {
    def accept[T](visitor: Visitor[T]): T =
      visitor.visitExpressionStmt(Expression(expression))
  }

  case class Print(val expression: Expr) extends Stmt {
    def accept[T](visitor: Visitor[T]): T =
      visitor.visitPrintStmt(Print(expression))
  }

  case class Var(val name: Token, val initializer: Option[Expr]) extends Stmt {
    def accept[T](visitor: Visitor[T]): T =
      visitor.visitVarStmt(Var(name, initializer))
  }
}
