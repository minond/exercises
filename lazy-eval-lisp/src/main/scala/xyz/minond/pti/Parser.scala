package xyz.minond.pti

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

import scala.collection.mutable.ListBuffer

object Parser {
  object Message {
    val STR_INVALID_INT = "Cannot parse integer number."
    val STR_INVALID_REAL = "Cannot parse real number."
    val STR_INVALID_SEXPR = "Cannot parse s-expression."

    val STR_INVALID_STR = "Cannot parse string."
    val STR_INVALID_NIL_STR = "Cannot parse empty string."

    val STR_INVALID_QUOTE = "Cannot parse quoted expression."
    val STR_INVALID_NIL_QUOTE = "Cannot parse empty quote expression."

    val STR_INVALID_IDENTIFIER = "Cannot parse identifier."
    val STR_INVALID_NIL_IDENTIFIER = "Empty identifier value."

    val STR_INVALID_BOOL = "Cannot parse boolean value."
    def STR_INVALID_BOOL_TOK(token: Token) =
      s"Expecting either 'f' or 't' but found ${token} instead."

    val STR_UNEXPECTED_EOF = "Unexpected end of input."
    val STR_INVALID_TOK = "Cannot parse invalid token."
    def STR_UNEXPECTED_TOK(token: Token) =
      s"Found unexpected token ${token}."
    def STR_EXPECTING_ONE_OF(ids: Tokenizer.Id*) =
      s"Expecting (one of): ${ids.mkString(", ")}."
  }

  case class Error(message: String, prev: Option[Parser.Error] = None) {
    def stringify(prefix: String = ""): String = {
      val next = prev match {
        case Some(err) => "\n" + err.stringify(prefix + "  ")
        case None => ""
      }

      s"; ${prefix}${message}${next}"
    }
  }
}

/*
 * Grammar:
 *
 * MAIN     = { expr } ;
 * expr     = "'" expr | sexpr | value ;
 * sexpr    = "(" { value } ")" ;
 * value    = IDENTIFIER | NUMBER | boolean ;
 * boolean  = "#" ( "f" | "t" ) ;
 */
class Parser(source: Tokenizer) extends Iterator[Either[Parser.Error, Expression]] {
  val tokens = source.buffered

  var curr =
    if (tokens.hasNext) tokens.head
    else Left(Tokenizer.Error("EOF"))

  def hasNext(): Boolean =
    tokens.hasNext

  def next(): Either[Parser.Error, Expression] = {
    tokens.head match {
      case Right(Token(POUND, _)) => parseBoolean
      case Right(Token(INTEGER, _)) => parseInteger
      case Right(Token(REAL, _)) => parseReal
      case Right(Token(DOT | IDENTIFIER, _)) => parseIdentifier
      case Right(Token(QUOTE, _)) => parseQuote
      case Right(Token(OPEN_PAREN, _)) => parseSExpr
      case Right(Token(STRING, _)) => parseString

      case Right(Token(CLOSE_PAREN, _)) =>
        skip
        Left(Parser.Error(Parser.Message.STR_UNEXPECTED_TOK(Token(CLOSE_PAREN))))

      case Right(token) =>
        skip
        Left(Parser.Error(Parser.Message.STR_UNEXPECTED_TOK(token)))

      case Left(Tokenizer.Error(msg, _)) =>
        skip
        Left(Parser.Error(Parser.Message.STR_INVALID_TOK, Some(Parser.Error(msg))))
    }
  }

  def eat() = {
    curr = tokens.head
    skip
    curr
  }

  def skip() =
    if (tokens.hasNext) tokens.next

  def expect(ids: Tokenizer.Id*): Either[Parser.Error, Token] = {
    if (!tokens.hasNext) Left(Parser.Error(Parser.Message.STR_UNEXPECTED_EOF))
    else
      eat match {
        case Right(token) if ids contains token.id =>
          Right(token)

        case Left(Tokenizer.Error(msg, _)) =>
          Left(Parser.Error(Parser.Message.STR_INVALID_TOK, Some(Parser.Error(msg))))

        case Right(token) =>
          Left(
            Parser.Error(
              Parser.Message.STR_UNEXPECTED_TOK(token),
              Some(Parser.Error(Parser.Message.STR_EXPECTING_ONE_OF(ids: _*)))))
      }
  }

  def parseBoolean() = {
    (expect(POUND), expect(IDENTIFIER)) match {
      case (Left(err), _) =>
        Left(Parser.Error(Parser.Message.STR_INVALID_BOOL, Some(err)))
      case (_, Left(err)) =>
        Left(Parser.Error(Parser.Message.STR_INVALID_BOOL, Some(err)))

      case (Right(_), Right(Token(_, Some("t")))) => Right(True)
      case (Right(_), Right(Token(_, Some("f")))) => Right(False)

      case (Right(_), Right(token)) =>
        Left(
          Parser.Error(
            Parser.Message.STR_INVALID_BOOL,
            Some(Parser.Error(Parser.Message.STR_INVALID_BOOL_TOK(token)))))
    }
  }

  def parseInteger() = {
    (expect(INTEGER), curr.map { _.lexeme.getOrElse("").toInt }) match {
      case (Left(err), _) =>
        Left(Parser.Error(Parser.Message.STR_INVALID_INT, Some(err)))

      case (_, Left(err: Tokenizer.Error)) =>
        Left(
          Parser.Error(Parser.Message.STR_INVALID_INT, Some(Parser.Error(err.message))))

      case (Right(_), Right(value)) =>
        Right(Integer(value))
    }
  }

  def parseReal() = {
    (expect(REAL), curr.map { _.lexeme.getOrElse("").toDouble }) match {
      case (Left(err), _) =>
        Left(Parser.Error(Parser.Message.STR_INVALID_REAL, Some(err)))

      case (_, Left(err: Tokenizer.Error)) =>
        Left(
          Parser.Error(Parser.Message.STR_INVALID_REAL, Some(Parser.Error(err.message))))

      case (Right(_), Right(value)) =>
        Right(Real(value))
    }
  }

  def parseIdentifier() = {
    expect(IDENTIFIER, DOT) match {
      case Right(Token(DOT, _)) =>
        Right(Identifier("."))

      case Right(Token(_, Some(value))) =>
        Right(Identifier(value))

      case Right(Token(_, None)) =>
        Left(
          Parser.Error(
            Parser.Message.STR_INVALID_IDENTIFIER,
            Some(Parser.Error(Parser.Message.STR_INVALID_NIL_IDENTIFIER))))

      case Left(err) =>
        Left(Parser.Error(Parser.Message.STR_INVALID_IDENTIFIER, Some(err)))
    }
  }

  def parseSExpr(): Either[Parser.Error, Expression] = {
    expect(OPEN_PAREN) match {
      case Left(err) =>
        Left(Parser.Error(Parser.Message.STR_INVALID_SEXPR, Some(err)))

      case Right(_) =>
        val values = ListBuffer[Expression]()

        while (tokens.hasNext && tokens.head != Right(Token(CLOSE_PAREN))) {
          next match {
            case Left(err) =>
              return Left(Parser.Error(Parser.Message.STR_INVALID_SEXPR, Some(err)))

            case Right(expr) =>
              values += expr
          }
        }

        expect(CLOSE_PAREN) match {
          case Left(err) =>
            Left(Parser.Error(Parser.Message.STR_INVALID_SEXPR, Some(err)))
          case Right(_) => Right(SExpr(values.toList))
        }
    }
  }

  def parseQuote() = {
    (expect(QUOTE), hasNext) match {
      case (Left(err), _) =>
        Left(Parser.Error(Parser.Message.STR_INVALID_QUOTE, Some(err)))
      case (_, false) => Left(Parser.Error(Parser.Message.STR_INVALID_NIL_QUOTE))

      case (Right(_), true) =>
        next match {
          case Left(err) =>
            Left(Parser.Error(Parser.Message.STR_INVALID_QUOTE, Some(err)))
          case Right(expr) => Right(Quote(expr))
        }
    }
  }

  def parseString() = {
    expect(STRING) match {
      case Left(err) => Left(Parser.Error(Parser.Message.STR_INVALID_STR, Some(err)))
      case Right(Token(_, None)) => Left(Parser.Error(Parser.Message.STR_INVALID_NIL_STR))
      case Right(Token(_, Some(str))) => Right(Str(str))
    }
  }
}
