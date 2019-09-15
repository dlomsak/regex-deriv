package com.github.dlomsak.regex.deriv

sealed trait RegexCompilationError
case class RegexLexerError(msg: String) extends RegexCompilationError
case class RegexParserError(msg: String) extends RegexCompilationError