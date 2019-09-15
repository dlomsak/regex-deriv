package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv.RegexLexerError

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

// carry around char form for escaped tokens when parsing
sealed abstract class RegexToken(val asChar: Char)
case object STAR extends RegexToken('*')
case object PLUS extends RegexToken('+')
case object PIPE extends RegexToken('|')
case object AMPER extends RegexToken('&')
case object TILDE extends RegexToken('~')
case object HOOK extends RegexToken('?')
case object LPAREN extends RegexToken('(')
case object RPAREN extends RegexToken(')')
case object LBRACKET extends RegexToken('[')
case object RBRACKET extends RegexToken(']')
case object CARET extends RegexToken('^')
case object DASH extends RegexToken('-')
case object BACKSLASH extends RegexToken('\\')
case object DOT extends RegexToken('.')
case class CHARLIT(c: Char) extends RegexToken(c)
case class DIGITLIT(c: Char) extends RegexToken(c)
case object LBRACE extends RegexToken('{')
case object RBRACE extends RegexToken('}')
case object COMMA extends RegexToken(',')

class CharacterReader(chars: Seq[Char]) extends Reader[Char] {
  override def first: Char = chars.head
  override def atEnd: Boolean = chars.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Char] = new CharacterReader(chars.tail)
}

object RELexer extends Parsers {
  override type Elem = Char

  def apply(chars: Seq[Char]): Either[RegexLexerError, Seq[RegexToken]] = {
    val reader = new CharacterReader(chars)
    program(reader) match {
      case NoSuccess(msg, next) => Left(RegexLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }

  def program:Parser[List[RegexToken]] = phrase(rep(token))

  def token = star | plus | hook | disj | conj | neg | lparen | rparen | lbracket | rbracket | backslash | dot | caret | dash | lbrace | rbrace | comma | lit

  def star:Parser[RegexToken] = '*' ^^ { _ => STAR }

  def plus:Parser[RegexToken] = '+' ^^ { _ => PLUS }

  def disj:Parser[RegexToken] = '|' ^^ { _ => PIPE }

  def conj:Parser[RegexToken] = '&' ^^ { _ => AMPER }

  def neg:Parser[RegexToken] = '~' ^^ { _ => TILDE }

  def hook:Parser[RegexToken] = '?' ^^ { _ => HOOK }

  def lparen:Parser[RegexToken] = '(' ^^ { _ => LPAREN }

  def rparen:Parser[RegexToken] = ')' ^^ { _ => RPAREN }

  def lbracket:Parser[RegexToken] = '[' ^^ { _ => LBRACKET }

  def rbracket:Parser[RegexToken] = ']' ^^ { _ => RBRACKET }

  def caret:Parser[RegexToken] = '^' ^^ { _ => CARET }

  def dash:Parser[RegexToken] = '-' ^^ { _ => DASH }

  def backslash:Parser[RegexToken] = '\\' ^^ { _ => BACKSLASH }

  def dot:Parser[RegexToken] = '.' ^^ { _ => DOT }

  def lit:Parser[RegexToken] = accept("character literal", {
    case c if c.isDigit => DIGITLIT(c)
    case c => CHARLIT(c)
  })

  def lbrace: Parser[RegexToken] = '{' ^^ { _ => LBRACE}

  def rbrace: Parser[RegexToken] = '}' ^^ { _ => RBRACE}

  def comma: Parser[RegexToken] = ',' ^^ { _ => COMMA}
}

