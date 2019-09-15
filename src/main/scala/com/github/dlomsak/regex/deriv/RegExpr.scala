package com.github.dlomsak.regex.deriv

import com.github.dlomsak.regex.deriv.phase._

object RegExpr {
  def apply(regex: String): Either[RegexCompilationError, DFA[Int]] = for {
    tokens <- RELexer(regex).right
    ast <- REParser(tokens).right
  } yield {
    RE2DFA(ast)
  }
}
