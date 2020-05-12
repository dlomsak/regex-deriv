package com.github.dlomsak.regex.deriv

import com.github.dlomsak.regex.deriv

final case class MatchContext (
                              newGroups: Set[Int] = Set.empty,
                              openGroups: Set[Int] = Set.empty,
                              matchedGroups: Set[Int] = Set.empty
                              ) {
  def withNew(group: Int) = copy(newGroups = newGroups + group)
  def withOpen(group: Int) = copy(openGroups = openGroups + group)
  def withMatch(group: Int) = copy(matchedGroups = matchedGroups + group)

  def union(other: MatchContext) = MatchContext(newGroups ++ other.newGroups, openGroups ++ other.openGroups, matchedGroups ++ other.matchedGroups)
  def intersect(other:MatchContext) = union(other) // TODO: define
}

object MatchContext {
  val empty = MatchContext()
}

sealed trait RegexAST {
    /**
    * denotes whether the regex matches the empty string (helper function for eval and derive)
    */
  def acceptsEmpty: Boolean

  /**
    * denotes whether the node is EmptyAST (used for convenience in property tests)
    */
  def isEmpty: Boolean = false

  /**
    * denotes whether the node is NullAST (used for convenience in property tests)
    */
  def isNull: Boolean = false

  /**
    * determines whether a given input string matches the given regular expression
    */
  def matches(input: String): Boolean = this(input).acceptsEmpty

  /**
    * Computes the derivative of a regex with respect to a single character. That is, the regex that matches the
    * remainder of the input given c is consumed. The expression is also simplified along the way.
   * Sub-group matching directions are collected along the way to inform the transducer
    */
  def derive(c: Char, openGroups: Set[Int]): (RegexAST, MatchContext)

  /**
    * perform derivation on the expression for a string of characters
    */
  def apply(input: String) = input.foldLeft(this)((r, c) => r.derive(c, Set.empty)._1)

  /*
   * returns the character equivalnce classes per section 4.2
   */
  def getCharClasses: Set[CharClassAST]
}

// AST of regex matching no strings
case object NullAST extends RegexAST {
  def acceptsEmpty = false

  override def isNull: Boolean = true

  def derive(c: Char, openGroups: Set[Int]) = (this, MatchContext.empty)

  val getCharClasses: Set[CharClassAST] = Set(CharClassAST.sigma)
}

// AST of regex matching exactly the empty string
case object EmptyAST extends RegexAST {
  def acceptsEmpty = true

  override def isEmpty: Boolean = true

  def derive(c: Char, openGroups: Set[Int]) = (NullAST, MatchContext.empty)

  val getCharClasses: Set[CharClassAST] = Set(CharClassAST.sigma)
}

// Complement of another AST
final class ComplementAST(val re: RegexAST) extends RegexAST {
  def acceptsEmpty = !re.acceptsEmpty

  def derive(c: Char, openGroups: Set[Int]) = {
    val (r, ctx) = re.derive(c, openGroups)
    (ComplementAST(r), ctx)
  }

  val getCharClasses: Set[CharClassAST] = re.getCharClasses

  override def equals(o: scala.Any): Boolean = o match {
    case ComplementAST(r2) if re == r2 => true
    case _ => false
  }

  override def toString: String = s"ComplementAST($re)"
}

object ComplementAST {
  def apply(re: RegexAST): RegexAST = re match {
    case ComplementAST(r) => r
    case GroupAST(r, g) => GroupAST(ComplementAST(r), g)
    case _ => new ComplementAST(re)
  }

  def unapply(arg: ComplementAST): Option[RegexAST] = Some(arg.re)
}

// a grouping
final class GroupAST(val re: RegexAST, val group: Int) extends RegexAST {
  def acceptsEmpty: Boolean = re.acceptsEmpty

  def derive(c: Char, openGroups: Set[Int]) = {
    val (r, ctx) = re.derive(c, openGroups)
    if (r.isNull) {
      (NullAST, ctx)
    } else if (r.isEmpty) {
      (EmptyAST, ctx.withMatch(group))
    } else if (r.acceptsEmpty) {
      (GroupAST(r, group), (if (openGroups.contains(group)) ctx.withOpen(group) else ctx.withNew(group)).withMatch(group))
    } else {
      (GroupAST(r, group), if (openGroups.contains(group)) ctx.withOpen(group) else ctx.withNew(group))
    }
  }

  val getCharClasses: Set[CharClassAST] = re.getCharClasses

  override def equals(o: scala.Any): Boolean = o match {
    case GroupAST(r2, group2) if re == r2 && group == group2 => true
    case _ => false
  }

  override def toString: String = s"GroupAST($re, $group)"
}

object GroupAST {
  def apply(re: RegexAST, group: Int): RegexAST = re match {
    case NullAST => NullAST
    case GroupAST(_, g) if g == group => re
    case _ => new GroupAST(re, group)
  }

  def unapply(arg: GroupAST): Option[(RegexAST, Int)] = Some(arg.re, arg.group)
}


final class OrAST(val left: RegexAST, val right: RegexAST) extends RegexAST {
  def acceptsEmpty = left.acceptsEmpty || right.acceptsEmpty

  def derive(c: Char, openGroups: Set[Int]) = {
    val (lr, lctx) = left.derive(c, openGroups)
    val (rr, rctx) = right.derive(c, openGroups)
    (OrAST(lr, rr), lctx.union(rctx))
  }

  val getCharClasses: Set[CharClassAST] = CharClassAST.conjunction(left.getCharClasses, right.getCharClasses)

  override def equals(o: Any): Boolean = o match {
    case OrAST(l, r) if (left == l && right == r) || (left == r && right == l) => true
    case _ => false
  }

  override def toString: String = s"OrAST($left, $right)"
}

/*
 * factory methods for operators to ensure ASTs are constructed in normalized form described in section 4.1
 */
object OrAST {
  def apply(left: RegexAST, right: RegexAST): RegexAST = (left, right) match {
    case (NullAST, _) => right
    case (_, NullAST) => left
    case (ComplementAST(NullAST), r) => ComplementAST(NullAST)
    case (r, ComplementAST(NullAST)) => ComplementAST(NullAST)
    case (OrAST(r, s), t) => new OrAST(r, OrAST(s, t))
    case (l, r) if l==r => l
    case _ => new OrAST(left, right)
  }

  def unapply(arg: OrAST): Option[(RegexAST, RegexAST)] = Some(arg.left, arg.right)
}

final class AndAST(val left: RegexAST, val right: RegexAST) extends RegexAST {
  def acceptsEmpty = left.acceptsEmpty && right.acceptsEmpty

  def derive(c: Char, openGroups: Set[Int]) = {
    // TODO: revisit correctness of intersecting actions
    val (lr, lctx) = left.derive(c, openGroups)
    val (rr, rctx) = right.derive(c, openGroups)
    (AndAST(lr, rr), lctx.intersect(rctx))
  }

  val getCharClasses: Set[CharClassAST] = CharClassAST.conjunction(left.getCharClasses, right.getCharClasses)

  override def equals(o: Any): Boolean = o match {
    case AndAST(l, r) if (left == l && right == r) || (left == r && right == l) => true
    case _ => false
  }

  override def toString: String = s"AndAST($left, $right)"
}

object AndAST {
  def apply(left: RegexAST, right: RegexAST): RegexAST = (left, right) match {
    case (NullAST, _) => NullAST
    case (_, NullAST) => NullAST
    case (ComplementAST(NullAST), r) => r
    case (r, ComplementAST(NullAST)) => r
    case (AndAST(r, s), t) => new AndAST(r, AndAST(s, t))
    case (l, r) if l==r => l
    case _ => new AndAST(left, right)
  }

  def unapply(arg: AndAST): Option[(RegexAST, RegexAST)] = Some(arg.left, arg.right)
}

final class CatAST(val left: RegexAST, val right: RegexAST) extends RegexAST {
  def acceptsEmpty = left.acceptsEmpty && right.acceptsEmpty

  def derive(c: Char, openGroups: Set[Int]) = {
    val (lr, lctx) = left.derive(c, openGroups)
    val dLeft = CatAST(lr, right)
    if (left.acceptsEmpty) {
      val (rr, rctx) = right.derive(c, openGroups)
      (OrAST(dLeft, rr), lctx.union(rctx))
    } else {
      (dLeft, lctx)
    }
  }

  val getCharClasses: Set[CharClassAST] = if (!left.acceptsEmpty) {
    left.getCharClasses
  } else {
    CharClassAST.conjunction(left.getCharClasses, right.getCharClasses)
  }

  override def equals(o: Any): Boolean = o match {
    case CatAST(l, r) if left == l && right == r => true
    case _ => false
  }

  override def toString: String = s"CatAST($left, $right)"
}

object CatAST {
  def apply(left: RegexAST, right: RegexAST): RegexAST = (left, right) match {
    case (NullAST, _) => NullAST
    case (_, NullAST) => NullAST
    case (EmptyAST, _) => right
    case (_, EmptyAST) => left
    case (CatAST(r, s), t) => new CatAST(r, CatAST(s, t))
    case _ => new CatAST(left, right)
  }

  def unapply(arg: CatAST): Option[(RegexAST, RegexAST)] = Some(arg.left, arg.right)
}


final class StarAST(val re: RegexAST) extends RegexAST {
  def acceptsEmpty = true

  def derive(c: Char, openGroups: Set[Int]) = {
    val (r, ctx) = re.derive(c, openGroups)
    (CatAST(r, this), ctx)
  }

  val getCharClasses: Set[CharClassAST] = re.getCharClasses

  override def equals(o: scala.Any): Boolean = o match {
    case StarAST(r2) if re == r2 => true
    case _ => false
  }

  override def toString: String = s"StarAST($re)"
}

object StarAST {
  def apply(re: RegexAST): RegexAST = re match {
    case NullAST => EmptyAST
    case EmptyAST => EmptyAST
    case StarAST(r) => re
    case _ => new StarAST(re)
  }

  def unapply(arg: StarAST): Option[RegexAST] = Some(arg.re)
}


final case class CharAST(c: Char) extends RegexAST {
  def acceptsEmpty = false

  def derive(cin: Char, openGroups: Set[Int]) = (if (c == cin) EmptyAST else NullAST, MatchContext.empty)

  val getCharClasses: Set[CharClassAST] = Set(CharClassAST(Set(c), inverted = false), CharClassAST(Set(c), inverted = true))
}

final case class CharClassAST(chars: Set[Char], inverted: Boolean) extends RegexAST {
  def acceptsEmpty = false

  def derive(c: Char, openGroups: Set[Int]) = {
    val isMember = chars.contains(c)
    val isMatch = if (inverted) !isMember else isMember
    (if (isMatch) EmptyAST else NullAST, MatchContext.empty)
  }

  def getCharClasses: Set[CharClassAST] = {
    if (chars.isEmpty) {
      Set(CharClassAST.sigma)
    } else {
      Set(this, this.copy(inverted = !inverted))
    }
  }

  def acceptsChar(c: Char): Boolean = chars.contains(c) && !inverted || !chars.contains(c) && inverted

  def intersect(other: CharClassAST): CharClassAST = {
    if (!inverted && !other.inverted) {
      CharClassAST(chars.intersect(other.chars), inverted = false)
    } else if (!inverted && other.inverted) {
      CharClassAST(chars.diff(other.chars), inverted = false)
    } else if (inverted && !other.inverted) {
      CharClassAST(other.chars.diff(chars), inverted = false)
    } else {
      CharClassAST(chars.union(other.chars), inverted = true)
    }
  }
}

object CharClassAST {
  val sigma: CharClassAST = CharClassAST(Set.empty, inverted = true)
  val digit: CharClassAST = CharClassAST(('0' to '9').toSet, false)
  val nonDigit = digit.copy(inverted = true)
  val horWS = deriv.CharClassAST("\t\u1680\u180e\u202f\u205f\u3000".toSet ++ ('\u2000' to '\u200a'), false) //\xA0
  val nonHorWS = horWS.copy(inverted = true)
  val ws = CharClassAST(" \t\n\f\r".toSet, false) // x0B
  val nonWS = ws.copy(inverted = true)
  val vertWS = deriv.CharClassAST("\n\f\r\u2028\u2029".toSet, false)// x0B x85
  val nonVertWS = vertWS.copy(inverted = true)
  val word = CharClassAST(digit.chars ++ ('a' to 'z').toSet ++ ('A' to 'Z').toSet + '_', false)
  val nonWord = word.copy(inverted = true)

  def conjunction(left: Set[CharClassAST], right: Set[CharClassAST]): Set[CharClassAST] = left flatMap { x =>
    right.map(_.intersect(x))
  }
}
