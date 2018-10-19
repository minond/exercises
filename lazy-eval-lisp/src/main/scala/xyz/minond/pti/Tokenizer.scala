package xyz.minond.pti

import scala.util.{Try, Success}

case class Token(id: Tokenizer.Id, lexeme: Option[String] = None) {
  import Tokenizer._

  override def toString =
    id match {
      case IDENTIFIER => s"(id ${lexeme.getOrElse("")})"
      case INTEGER => s"(int ${lexeme.getOrElse("")})"
      case REAL => s"(real ${lexeme.getOrElse("")})"
      case STRING => s"""(string "${lexeme.getOrElse("")}")"""
      case OPEN_PAREN => """"(""""
      case CLOSE_PAREN => """")""""
      case POUND => """"#""""
      case QUOTE => """"'""""
      case DOT => """".""""
    }
}

object Tokenizer extends Enumeration {
  type Id = Value

  val OPEN_PAREN, CLOSE_PAREN, IDENTIFIER, STRING, INTEGER, REAL, POUND, QUOTE, DOT =
    Value

  object Message {
    val STR_NO_CLOSING_WITH_CONT =
      """String did not end with a '"' but there is still input."""
    val STR_NO_CLOSING = """String did not end with a '"' character."""
  }

  case class Error(message: String, lexeme: Option[String] = None)
}

class Tokenizer(raw: String) extends Iterator[Either[Tokenizer.Error, Token]] {
  import Tokenizer.{
    DOT,
    IDENTIFIER,
    POUND,
    INTEGER,
    REAL,
    OPEN_PAREN,
    CLOSE_PAREN,
    QUOTE,
    STRING
  }

  type CharComp = Char => Boolean

  val src = raw.trim
    .split("\n")
    .filter(!_.trim.startsWith(";"))
    .flatMap(_.toList)
    .toIterator
    .buffered

  def hasNext(): Boolean =
    src.hasNext

  def next(): Either[Tokenizer.Error, Token] = {
    src.next match {
      case c if c.isWhitespace => next

      case '(' => ok(OPEN_PAREN)
      case ')' => ok(CLOSE_PAREN)
      case '#' => ok(POUND)
      case '\'' => ok(QUOTE)

      case '"' => parseStr
      case n if n.isDigit || is('.')(n) || is('-')(n) => parseNum(n)

      case x =>
        val chars = x :: consume(isIdentifier(_: Char)).toList
        ok(IDENTIFIER, Some(chars.mkString))
    }
  }

  def parseStr: Either[Tokenizer.Error, Token] = {
    val str = Some(lookbehind({
      case ('"', Some('\\')) => true
      case ('"', _) => false
      case _ => true
    }).mkString)

    (src.hasNext, if (src.hasNext) src.head else 0.toChar) match {
      case (true, '"') =>
        src.next
        ok(STRING, str)

      case (true, _) =>
        src.next
        err(Tokenizer.Message.STR_NO_CLOSING_WITH_CONT, str)

      case (false, _) =>
        err(Tokenizer.Message.STR_NO_CLOSING, str)
    }
  }

  def parseNum(n: Char): Either[Tokenizer.Error, Token] = {
    val digits = n :: consume(or(or(_.isDigit, isIdentifier(_)), is('.'))).toList
    val str = digits.mkString
    val num = Some(str)
    val valid = Try(str.toDouble)

    (valid, digits.count(is('.')), num) match {
      case (_, _, Some(".")) => ok(DOT)
      case (Success(_), 1, _) => ok(REAL, num)
      case (Success(_), _, _) => ok(INTEGER, num)
      case (_, _, _) => ok(IDENTIFIER, num)
    }
  }

  def ok(id: Tokenizer.Id, lexeme: Option[String] = None) =
    Right(Token(id, lexeme))

  def err(message: String, lexeme: Option[String] = None) =
    Left(Tokenizer.Error(message, lexeme))

  def lookbehind(f: (Char, Option[Char]) => Boolean) = {
    def aux(buff: List[Char], prev: Option[Char]): List[Char] =
      if (src.hasNext && f(src.head, prev)) {
        val curr = src.head
        src.next
        aux(buff ++ List(curr), Some(curr))
      } else buff

    aux(List[Char](), None)
  }

  def consume(f: CharComp) =
    lookbehind((x: Char, _) => f(x))

  def isIdentifier(c: Char): Boolean =
    c != '(' && c != ')' && !c.isWhitespace

  def is(c: Char): CharComp =
    (x: Char) => c == x

  def not(f: CharComp): CharComp =
    (x: Char) => !f(x)

  def or(f1: CharComp, f2: CharComp): CharComp =
    (x: Char) => f1(x) || f2(x)
}
