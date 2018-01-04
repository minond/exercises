package com.craftinginterpreters.lox

import scala.util.{Try, Success, Failure}
import java.io.{BufferedReader, InputStreamReader};
import java.nio.charset.Charset;
import java.nio.file.{Files, Paths}

object Main {
  val OPT_PRINT_AST = 0
  val OPT_PRINT_TOK = 1

  val interpreter = new Interpreter

  var hadError = false
  var hadRuntimeError = false

  def main(args: Array[String]) = {
    if (args.length > 1) {
      System.out.println("Usage: main [script]");
    } else if (args.length == 1) {
      args(0) match {
        case "--print-ast" => runPrompt(Some(OPT_PRINT_AST))
        case "--print-tokens" => runPrompt(Some(OPT_PRINT_TOK))
        case _ => runFile(args(0))
      }
    } else {
      runPrompt(None);
    }
  }

  private def runFile(path: String) = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset()), None)

    if (hadError)
      System.exit(65)
    else if (hadRuntimeError)
      System.exit(70)
  }

  private def runPrompt(printConfig: Option[Int]) = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    while (true) {
      print("> ")
      run(reader.readLine(), printConfig)
      hadError = false
    }
  }

  private def run(source: String, printConfig: Option[Int]) = {
    val scanner = new Scanner(source)
    val tokens = scanner.scanTokens()

    val printer = new AstPrinter()
    val parser = new Parser(tokens)

    Try { parser.parse() } match {
      case Success(expression) =>
        if (!hadError) {
          printConfig match {
            case Some(OPT_PRINT_TOK) =>
              println(expression)
              println(printTokens(expression))

            case Some(OPT_PRINT_AST) =>
              println(printer.print(expression))

            case _ =>
              interpreter.interpret(expression)
          }
        }

      case Failure(err) =>
    }
  }

  // TODO find a better home for this function
  private def printTokens(expr: Expr): String = {
    expr match {
      case expr: Expr.Binary =>
        printTokens(expr.left) + "\n" + expr.operator + "\n" + printTokens(expr.right)

      case expr: Expr.Grouping =>
        printTokens(expr.expression)

      case expr: Expr.Literal =>
        stringify(expr.value)

      case expr: Expr.Unary =>
        expr.operator + "\n" + printTokens(expr.right)

      case _ => ""
    }
  }

  // TODO find a better home for this function, specially since
  // Interpreter.stringify already exists
  private def stringify(value: Any): String = {
    if (value == null) {
      "nil"
    } else if (value.isInstanceOf[Double]) {
      val txt = value.toString

      if (txt.endsWith(".0"))
        txt.stripSuffix(".0")
      else
        txt
    } else {
      value.toString
    }
  }

  def error(token: Token, message: String) = {
    if (token.ttype == TokenType.EOF) {
      report(token.line, " at end", message)
    } else {
      report(token.line, " at '" + token.lexeme + "'", message)
    }
  }

  def error(line: Int, message: String) = {
    report(line, "", message)
  }

  def runtimeError(err: RuntimeError) = {
    Console.err.println(err.getMessage())
    Console.err.println(s"[line ${err.token.line}]")
    hadRuntimeError = true
  }

  def report(line: Int, where: String, message: String) = {
    printf("[line %s] Error%s: %s\n", line, where, message)
    hadError = true
  }
}
