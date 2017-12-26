package com.craftinginterpreters.lox

import java.io.{BufferedReader, InputStreamReader};
import java.nio.charset.Charset;
import java.nio.file.{Files, Paths}

object Main {
  var hadError = false

  def main(args: Array[String]) = {
    val tok = Token(TokenType.EQUAL_EQUAL, "==", Some("=="), 3)
    println(tok)
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
    // val scanner = new Scanner(source)
    // val tokens = scanner.scanTokens()
    //
    // val printer = new AstPrinter()
    // val parser = new Parser(tokens)
    // val expression = parser.parse()
    //
    // if (hadError)
    //   return
    //
    // println(printer.print(expression))
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
