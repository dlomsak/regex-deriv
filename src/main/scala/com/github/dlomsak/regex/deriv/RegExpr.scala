package com.github.dlomsak.regex.deriv

import com.github.dlomsak.regex.deriv.phase._

object RegExpr {
  def apply(regex: String): Either[RegexCompilationError, RegexAST] = for {
    tokens <- RELexer(regex).right
    parseResult <- REParser(tokens).right
  } yield {
    val (ast, ctx) = parseResult
    ast
  }
}
