package com.craftinginterpreters.lox.tool

import java.io.PrintWriter

object GenerateAst extends App {
  if (args.length != 1) {
    System.err.println("Usage: scala src/main/scala/tool/GenerateAst.scala <directory>")
    System.exit(1)
  }

  defineAst(args(0), "Expr", Array(
    "Binary   - left: Expr, operator: Token, right: Expr",
    "Grouping - expression: Expr",
    "Literal  - value: Object",
    "Unary    - operator: Token, right: Expr"
  ))

  def defineAst(outputDir: String, baseName: String, types: Array[String]) = {
    val path = s"${outputDir}/${baseName}.scala"
    val writer = new PrintWriter(path, "UTF-8")

    println(s"Generating $path.")

    writer.println("/* AUTO GENERATED - DO NOT EDIT */")
    writer.println("package com.craftinginterpreters.lox")
    writer.println("")
    writer.println("import collection.mutable.MutableList")
    writer.println("")

    writer.println(s"abstract class ${baseName} {")

    // Visitor methods
    defineVisitor(writer, baseName, types)

    // AST classes
    for (ttype <- types) {
      val className = ttype.split("-")(0).trim()
      val fields = ttype.split("-")(1).trim()
      writer.println("")
      defineType(writer, baseName, className, fields)
    }

    writer.println("")
    writer.println("  def accept[T](visitor: Visitor[T]): T")
    writer.println("}")
    writer.close()
  }

  def defineType(writer: PrintWriter, baseName: String, className: String, fields: String) = {
    val justArgs = fields
      .split(",")
      .map { field => field.split(":")(0).trim() }
      .mkString(", ")

    // Constructor
    writer.println(s"  class $className($fields) extends $baseName {")

    // Visitor
    writer.println(s"    def accept[T](visitor: Visitor[T]): T = {")
    writer.println(s"      visitor.visit${className}${baseName}(new $className($justArgs))")
    writer.println("    }")
    writer.println("  }")
  }

  def defineVisitor(writer: PrintWriter, baseName: String, types: Array[String]) = {
    writer.println("  trait Visitor[T] {")

    for (ttype <- types) {
      val typeName = ttype.split("-")(0).trim()
      writer.println(s"    def visit${typeName}${baseName} (${baseName.toLowerCase}: ${typeName}): T")
    }

    writer.println("  }")
  }
}
