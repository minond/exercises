package com.craftinginterpreters.lox.tool

import java.io.PrintWriter

object GenerateAst extends App {
  if (args.length != 1) {
    System.err.println("Usage: scala src/main/scala/tool/GenerateAst.scala <directory>")
    System.exit(1)
  }

  defineAst(args(0), "Stmt", Array(
    "Expression - expression: Expr",
    "Print      - expression: Expr",
    "Var        - name: Token, initializer: Expr"
  ))

  defineAst(args(0), "Expr", Array(
    "Binary   - left: Expr, operator: Token, right: Expr",
    "Grouping - expression: Expr",
    "Literal  - value: Any",
    "Unary    - operator: Token, right: Expr",
    "Variable - name: Token"
  ))

  def defineAst(outputDir: String, baseName: String, types: Array[String]) = {
    val path = s"${outputDir}/${baseName}.scala"
    val writer = new PrintWriter(path, "UTF-8")

    println(s"Generating $path.")

    defineHeader(writer)
    writer.println("")

    defineImports(writer)
    writer.println("")

    defineClass(writer, baseName)
    writer.println("")

    defineObject(writer, baseName, types)
    writer.close()
  }

  def defineHeader(writer: PrintWriter) = {
    writer.println("/* AUTO GENERATED - DO NOT EDIT */")
    writer.println("package com.craftinginterpreters.lox")
  }

  def defineImports(writer: PrintWriter) = {
    writer.println("import collection.mutable.MutableList")
  }

  def defineObject(writer: PrintWriter, baseName: String, types: Array[String]) = {
    writer.println(s"object ${baseName} {")
    defineVisitor(writer, baseName, types)

    types.foreach { ttype =>
      val className = ttype.split("-")(0).trim()
      val fields = ttype.split("-")(1).trim()
      writer.println("")
      defineType(writer, baseName, className, fields)
    }

    writer.println("}")
  }

  def defineClass(writer: PrintWriter, baseName: String) = {
    writer.println(s"abstract class ${baseName} {")
    writer.println(s"  def accept[T](visitor: ${baseName}.Visitor[T]): T")
    writer.println("}")
  }

  def defineType(writer: PrintWriter, baseName: String, className: String, fields: String) = {
    val justArgs = fields
      .split(",")
      .map { field => field.split(":")(0).trim() }
      .mkString(", ")

    val accessArgs = fields
      .split(",")
      .map { field => field.trim }
      .map { field => s"val $field" }
      .mkString(", ")

    // Constructor
    writer.println(s"  class $className($accessArgs) extends $baseName {")

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
