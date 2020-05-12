package com.github.dlomsak.regex.deriv

sealed trait Pattern {
  def toRegex: RegexAST
}

final case class VarPat(x: Int, range: List[(Int, Int)], p: Pattern) extends Pattern {
  override def toRegex: RegexAST = p.toRegex
}

final case class RegexPat(r: RegexAST) extends Pattern {
  override def toRegex: RegexAST = r
}

final case class OrPat(l: Pattern, r: Pattern) extends Pattern {
  override def toRegex: RegexAST = OrAST(l.toRegex, r.toRegex)
}

final case class StarPat(p: Pattern) extends Pattern {
  override def toRegex: RegexAST = StarAST(p.toRegex)
}
