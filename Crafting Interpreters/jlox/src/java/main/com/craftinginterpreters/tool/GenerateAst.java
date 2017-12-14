package com.craftinginterpreters.tool;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;

public class GenerateAst {
  public static void main(String[] args) throws IOException {
    if (args.length != 1) {
      System.err.println("Usage: generate_ast <output director>");
      System.exit(1);
    }

    defineAst(
        args[0],
        "Expr",
        Arrays.asList(
            "Binary   : Expr left, Token operator, Expr right",
            "Grouping : Expr expression",
            "Literal  : Object value",
            "Unary    : Token operator, Expr right"));
  }

  private static void defineAst(String outputDir, String baseName, List<String> types)
      throws IOException {
    String path = outputDir + "/" + baseName + ".java";
    PrintWriter writer = new PrintWriter(path, "UTF-8");

    writer.println("package com.craftinginterpreters.lox;");
    writer.println("");
    writer.println("import java.util.List;");
    writer.println("");
    writer.printf("abstract class %s {\n", baseName);

    defineVisitor(writer, baseName, types);

    // AST classes.
    for (String type : types) {
      String className = type.split(":")[0].trim();
      String fields = type.split(":")[1].trim();
      defineType(writer, baseName, className, fields);
    }

    writer.println("\n  abstract <R> R accept(Visitor<R> visitor);");
    writer.println("}");
    writer.close();
  }

  private static void defineType(
      PrintWriter writer, String baseName, String className, String fields) {
    // Start of class definition
    writer.println("");
    writer.printf("  static class %s extends %s {\n", className, baseName);

    // Constructor
    String[] fieldList = fields.split(",");
    writer.printf("    %s (%s) {\n", className, fields);
    for (String field : fieldList) {
      String name = field.trim().split(" ")[1].trim();
      writer.printf("      this.%s = %s;\n", name, name);
    }
    writer.printf("    }\n\n");

    // Visitor
    writer.println("\n    <R> R accept(Visitor<R> visitor) {");
    writer.printf("      return visitor.visit%s%s(this);\n", className, baseName);
    writer.println("    }");

    // Fields
    for (String field : fieldList) {
      writer.printf("    final %s;\n", field);
    }

    // End of class definition
    writer.printf("  }");
  }

  private static void defineVisitor(PrintWriter writer, String baseName, List<String> types) {
    writer.println("    interface Visitor<R> {");

    for (String type : types) {
      String typeName = type.split(":")[0].trim();
      writer.printf(
          "      R visit%s%s (%s %s);\n", typeName, baseName, typeName, baseName.toLowerCase());
    }

    writer.println("    }");
  }
}
