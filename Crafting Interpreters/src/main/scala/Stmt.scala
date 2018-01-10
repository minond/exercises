/* AUTO GENERATED - DO NOT EDIT */
package com.craftinginterpreters.lox

import collection.mutable.MutableList

abstract class Stmt {
  def accept[T](visitor: Stmt.Visitor[T]): T
}

object Stmt {
  trait Visitor[T] {
    def visitExpressionStmt (stmt: Expression): T
    def visitPrintStmt (stmt: Print): T
    def visitVarStmt (stmt: Var): T
  }

  class Expression(val expression: Expr) extends Stmt {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitExpressionStmt(new Expression(expression))
    }
  }

  class Print(val expression: Expr) extends Stmt {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitPrintStmt(new Print(expression))
    }
  }

  class Var(val name: Token, val initializer: Expr) extends Stmt {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visitVarStmt(new Var(name, initializer))
    }
  }
}
