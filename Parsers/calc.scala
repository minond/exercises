case class Token(tok : String, str : String) {
  override def toString() : String = {
    s"""(Token type="$tok" value="$str")"""
  }
}

object Lexer {
  val TOK_NUM = "toknum"
  val TOK_SYM = "toksym"
  val TOK_EOL = "tokeol"
}

class Lexer(str : String) {
  var pntr = 0
  var curr = ""
  var done = false

  nextChar()

  def nextChar() = {
    if (pntr >= str.length) {
      done = true
    } else {
      curr = str.substring(pntr, pntr + 1)
      pntr = pntr + 1
    }
  }

  def nextToken() : Token = {
    if (done) {
      curr = ""
      return Token(Lexer.TOK_EOL, "<eol>")
    }

    ws()

    if (curr.matches("\\d"))
      return num()
    else
      return sym()
  }

  def ws() = {
    while (curr.matches("\\s"))
      nextChar()
  }

  def num() : Token = {
    val buff = new StringBuilder()

    while (!done && curr.matches("\\d")) {
      buff.append(curr)
      nextChar()
    }

    return Token(Lexer.TOK_NUM, buff.toString)
  }

  def sym() : Token = {
    val tok = Token(Lexer.TOK_SYM, curr)
    nextChar()
    return tok
  }
}

object calc {
  def main(args : Array[String]) = {
    val lex = new Lexer("1  + 2 +   (3 + 123 - 543) * 42    / 23 - 12")

    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
    println(lex.nextToken)
  }
}
