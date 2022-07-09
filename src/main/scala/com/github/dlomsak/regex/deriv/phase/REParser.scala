package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv._

import scala.collection.mutable
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

class RegexTokenReader(tokens: Seq[RegexToken]) extends Reader[RegexToken] {
  override def first: RegexToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[RegexToken] = new RegexTokenReader(tokens.tail)
}

final class ParseContext {
  private var count = 0
  private var groupStack = List.empty[Int]
  private val bindings = mutable.Map.empty[String, Int]

  def pushGroup(): ParseContext = {
    groupStack = count :: groupStack
    count += 1
    this
  }

  def popGroup(): Int = {
    val head = groupStack.head
    groupStack = groupStack.tail
    head
  }

  // add a name binding for a group
  def bind(name: String, group: Int): Unit = {
    bindings += (name -> group)
  }

  def getBindings: Map[String, Int] = bindings.toMap
}

object REParser extends Parsers {
  override type Elem = RegexToken

  def apply(tokens: Seq[RegexToken]): Either[RegexParserError, (RegexAST, ParseContext)] = {
    val reader = new RegexTokenReader(tokens)
    val ctx = new ParseContext()
    program(ctx)(reader) match {
      case NoSuccess(msg, next) => Left(RegexParserError(msg))
      case Success(result, next) => Right((result, ctx))
      case Error(msg, next) => Left(RegexParserError(msg))
    }
  }

  def program(implicit ctx: ParseContext): Parser[RegexAST] = phrase(opt(regex)) ^^ { _.getOrElse(EmptyAST) }

  def regex(implicit ctx: ParseContext): Parser[RegexAST] =
    term ~ opt((PIPE | AMPER) ~ regex) ^^ {
      case l ~ Some(PIPE ~ r) => OrAST(l, r)
      case l ~ Some(AMPER ~ r) => AndAST(l, r)
      case l ~ None => l
    }

  def term(implicit ctx: ParseContext): Parser[RegexAST] = rep1(factor) ^^ { CatAST(_) }

  def factor(implicit ctx: ParseContext): Parser[RegexAST] =  base ~ opt(STAR | PLUS | HOOK | quantifier) ^^ {
      case r ~ Some(STAR) => StarAST(r)
      case r ~ Some(PLUS) => CatAST(List(r, StarAST(r)))
      case r ~ Some(HOOK) => OrAST(r, EmptyAST)
      case r ~ Some((lower: Int, optUpper: Option[Int])) =>
        val suffix = optUpper map { upper =>
          (lower until upper).foldLeft(EmptyAST: RegexAST) { case (subtree, _) => CatAST(List(subtree, OrAST(r, EmptyAST))) }
        } getOrElse {
          StarAST(r)
        }
        val prefix = (0 until lower).foldLeft(EmptyAST: RegexAST) { case (subtree, _) => CatAST(List(r, subtree)) }
        CatAST(List(prefix, suffix))
      case r ~ None => r
  }

  def identifier(implicit ctx: ParseContext): Parser[String] = literal ~ rep(singleChar) ^^ { case c ~ cs => (c :: cs).map(_.c).mkString("") }

  def base(implicit ctx: ParseContext): Parser[RegexAST] =
    singleChar |
    (BACKSLASH ~> literal flatMap doEscape) |
    DOT ^^ { _ => CharClassAST.sigma } |
    charClass |
    LPAREN ~> opt( HOOK ~> LANGLE ~> identifier <~ RANGLE) ~ regex(ctx.pushGroup()) <~ RPAREN ^^ { case name ~ r =>
      val groupNum = ctx.popGroup()
      name.foreach(n => ctx.bind(n, groupNum))
      //GroupAST(r, groupNum)
      r
    } |
    TILDE ~> regex ^^ { r => ComplementAST(r) }

  private def doEscape(c: CharAST)(implicit ctx: ParseContext): Parser[RegexAST] = c.c match {
    case 't' => success(CharAST('\t'))
    case 'n' => success(CharAST('\n'))
    case 'r' => success(CharAST('\r'))
    case 'd' => success(CharClassAST.digit)
    case 'D' => success(CharClassAST.nonDigit)
    case 'h' => success(CharClassAST.horWS)
    case 'H' => success(CharClassAST.nonHorWS)
    case 's' => success(CharClassAST.ws)
    case 'S' => success(CharClassAST.nonWS)
    case 'v' => success(CharClassAST.vertWS)
    case 'V' => success(CharClassAST.nonVertWS)
    case 'w' => success(CharClassAST.word)
    case 'W' => success(CharClassAST.nonWord)
    case _ => err(s"invalid escape character '${c.c}'")
}

  def charClass(implicit ctx: ParseContext): Parser[RegexAST] =
    LBRACKET ~> opt(CARET) ~ rep1(charRange) <~ RBRACKET ^^
      { case invert ~ chars => CharClassAST(chars.reduce(_ ++ _), invert.isDefined) }

  def charRange(implicit ctx: ParseContext): Parser[Set[Char]] = {
    literal ~ DASH ~ literal ^^ { case start ~ _ ~ stop => Set(start.c to stop.c:_*) } |
    singleChar ^^ { ch => Set(ch.c) }
  }

  def singleChar(implicit ctx: ParseContext): Parser[CharAST] =
    BACKSLASH ~> meta ^^ { x => CharAST(x.asChar) } |
    literal |
    digitLiteral

  def meta(implicit ctx: ParseContext): Parser[RegexToken] =
    LPAREN | RPAREN | LBRACKET | RBRACKET | LANGLE | RANGLE | PLUS | STAR | HOOK | BACKSLASH | DOT | CARET | DASH | LBRACE | RBRACE | COMMA

  def literal(implicit ctx: ParseContext): Parser[CharAST] = accept("character literal", { case _ @ CHARLIT(c) => CharAST(c) })

  def digitLiteral(implicit ctx: ParseContext): Parser[CharAST] = accept("digit lit", { case _ @ DIGITLIT(x) => CharAST(x) })

  def int(implicit ctx: ParseContext): Parser[Int] = {
    rep1(accept("int lit", { case i @ DIGITLIT(x) => x})) ^^ { chars => chars.mkString.toInt }
  }


  def quantifier(implicit ctx: ParseContext): Parser[(Int, Option[Int])] = LBRACE ~> int ~ opt(COMMA) ~ opt(int) <~ RBRACE flatMap {
    case i ~ Some(_) ~ j if i <= j.getOrElse(Int.MaxValue)  => success((i, j))
    case i ~ None ~ None => success((i, Some(i)))
    case i ~ Some(_) ~ Some(j) => err(s"In quantifier {m,n}, m is $i and n is $j, but m cannot be greater than n.")
  }
}
