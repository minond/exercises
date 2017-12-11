package com.craftinginterpreters.lox;

import java.io.IOException;

public class Lox {
  public static void main(String[] args) throws IOException {
    if (args.length > 1) {
      System.out.println("Usage: jlox [script]");
    } else if (args.length == 1) {
      System.out.println("runFile(args[0]);");
    } else {
      System.out.println("runPrompt();");
    }
  }
}
