package com.craftinginterpreters.lox

import scala.util.{Try, Success, Failure}
import java.io.{BufferedReader, InputStreamReader};
import java.nio.charset.Charset;
import java.nio.file.{Files, Paths}

object Main {
  var hadError = false

  def main(args: Array[String]) = {
    if (args.length > 1) {
      System.out.println("Usage: main [script]");
    } else if (args.length == 1) {
      runFile(args(0));
    } else {
      runPrompt();
    }
  }

  private def runFile(path: String) = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset()))

    if (hadError)
      System.exit(65)
  }

  private def runPrompt() = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    while (true) {
      print("> ")
      run(reader.readLine())
      hadError = false
    }
  }

  private def run(source: String) = {
    val scanner = new Scanner(source)
    val tokens = scanner.scanTokens()

    // val printer = new AstPrinter()
    val parser = new Parser(tokens)

    Try { parser.parse() } match {
      case Success(expression) =>
        if (!hadError)
          // println(printer.print(expression))
          println(expression)

      case Failure(err) =>
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

  def report(line: Int, where: String, message: String) = {
    printf("[line %s] Error%s: %s\n", line, where, message)
    hadError = true
  }
}
