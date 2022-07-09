package com.github.dlomsak.regex.deriv

import com.github.dlomsak.regex.deriv

sealed trait RegexAST {
    /**
    * denotes whether the regex matches the empty string (helper function for eval and derive)
    */
  val acceptsEmpty: Boolean

  /**
    * denotes whether the node is EmptyAST (used for convenience in property tests)
    */
  val isEmpty: Boolean = false

  /**
    * denotes whether the node is NullAST (used for convenience in property tests)
    */
  val isNull: Boolean = false

  /**
   * Computes the derivative of a regex with respect to a single character. That is, the regex that matches the
   * remainder of the input given c is consumed. The expression is also simplified along the way via "smart constructors".
   */
  def derive(c: Char): RegexAST

  /**
    * determines whether a given input string matches the given regular expression
    */
  def matches(input: String): Boolean = this(input).acceptsEmpty

  /**
    * perform derivation on the expression for a string of characters
    */
  def apply(input: String) = input.foldLeft(this)((r, c) => r.derive(c))

  /*
   * returns the character equivalnce classes per section 4.2
   */
  def getCharClasses: Set[CharClassAST]

  // determine whether two expressions are equivalent
  def equivalent(other: RegexAST): Boolean
}

// AST of regex matching no strings
case object NullAST extends RegexAST {

  override val acceptsEmpty: Boolean = false

  override val isNull: Boolean = true

  val getCharClasses: Set[CharClassAST] = Set(CharClassAST.sigma)

  override def derive(c: Char): RegexAST = NullAST

  override def equivalent(other: RegexAST): Boolean = other match {
    case NullAST => true
    case _ => false
  }
}

// AST of regex matching exactly the empty string
case object EmptyAST extends RegexAST {
  override val acceptsEmpty: Boolean = true

  override val isEmpty: Boolean = true

  val getCharClasses: Set[CharClassAST] = Set(CharClassAST.sigma)

  override def derive(c: Char): RegexAST = EmptyAST

  override def equivalent(other: RegexAST): Boolean = other match {
    case EmptyAST => true
    case _ => false
  }
}

// Complement of another AST
final class ComplementAST(val re: RegexAST) extends RegexAST {
  override val acceptsEmpty: Boolean = !re.acceptsEmpty

  val getCharClasses: Set[CharClassAST] = re.getCharClasses

  override def equals(o: scala.Any): Boolean = o match {
    case ComplementAST(r2) => r2.equivalent(r2)
    case _ => false
  }

  override def toString: String = s"ComplementAST($re)"

  override def derive(c: Char): RegexAST = ComplementAST(re.derive(c))

  override def equivalent(other: RegexAST): Boolean = other match {
    case ComplementAST(r) => r.equivalent(re)
    case _ => false
  }
}

object ComplementAST {
  def apply(re: RegexAST): RegexAST = re match {
    case ComplementAST(r) => r
    case _ => new ComplementAST(re)
  }

  def unapply(arg: ComplementAST): Option[RegexAST] = Some(arg.re)
}

final class OrAST(val left: RegexAST, val right: RegexAST) extends RegexAST {
  override val acceptsEmpty: Boolean = left.acceptsEmpty || right.acceptsEmpty

  val getCharClasses: Set[CharClassAST] = CharClassAST.conjunction(left.getCharClasses, right.getCharClasses)

  override def equals(o: Any): Boolean = o match {
    case o: RegexAST => this.equivalent(o)
    case _ => false
  }

  override def toString: String = s"OrAST($left, $right)"

  override def derive(c: Char): RegexAST = OrAST(left.derive(c), right.derive(c))

  override def equivalent(other: RegexAST): Boolean = other match {
    case OrAST(l, r) => left.equivalent(l) && right.equivalent(r) || left.equivalent(r) && right.equivalent(l)
    case _ => false
  }
}

/*
 * smart constructors to ensure ASTs are constructed in normalized form described in section 4.1
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
  override val acceptsEmpty: Boolean = left.acceptsEmpty && right.acceptsEmpty

  val getCharClasses: Set[CharClassAST] = CharClassAST.conjunction(left.getCharClasses, right.getCharClasses)

  override def equals(o: Any): Boolean = o match {
    case o: RegexAST => this.equivalent(o)
    case _ => false
  }

  override def toString: String = s"AndAST($left, $right)"

  override def derive(c: Char): RegexAST = AndAST(left.derive(c), right.derive(c))

  override def equivalent(other: RegexAST): Boolean = other match {
    case AndAST(l, r) => left.equivalent(l) && right.equivalent(r) || left.equivalent(r) && right.equivalent(l)
  }
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
  override val acceptsEmpty: Boolean = left.acceptsEmpty && right.acceptsEmpty

  val getCharClasses: Set[CharClassAST] = if (!left.acceptsEmpty) {
    left.getCharClasses
  } else {
    CharClassAST.conjunction(left.getCharClasses, right.getCharClasses)
  }

  override def equals(o: Any): Boolean = o match {
    case o: RegexAST => this.equivalent(o)
    case _ => false
  }

  override def toString: String = s"CatAST($left, $right)"

  override def derive(c: Char): RegexAST = {
    val dLeft = left.derive(c)
    if (left.acceptsEmpty) {
      OrAST(CatAST(dLeft, right), right.derive(c))
    } else {
      CatAST(dLeft, right)
    }
  }

  override def equivalent(other: RegexAST): Boolean = other match {
    case CatAST(l, r) => left.equivalent(l) && right.equivalent(r)
    case _ => false
  }
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
  override val acceptsEmpty: Boolean = true

  val getCharClasses: Set[CharClassAST] = re.getCharClasses

  override def equals(o: scala.Any): Boolean = o match {
    case StarAST(r2) => re.equivalent(r2)
    case _ => false
  }

  override def toString: String = s"StarAST($re)"

  override def derive(c: Char): RegexAST = CatAST(re.derive(c), this)

  override def equivalent(other: RegexAST): Boolean = other match {
    case StarAST(r) => r.equivalent(re)
    case _ => false
  }
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
  override val acceptsEmpty: Boolean = false

  val getCharClasses: Set[CharClassAST] = Set(CharClassAST(Set(c), inverted = false), CharClassAST(Set(c), inverted = true))

  override def derive(cin: Char): RegexAST = if (c==cin) EmptyAST else NullAST

  override def equivalent(other: RegexAST): Boolean = this == other
}

final case class CharClassAST(chars: Set[Char], inverted: Boolean) extends RegexAST {
  override val acceptsEmpty: Boolean = false

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

  override def derive(c: Char): RegexAST = {
    val isMember = chars.contains(c)
    val isMatch = if (inverted) !isMember else isMember
    if (isMatch) EmptyAST else NullAST
  }

  override def equivalent(other: RegexAST): Boolean = this == other
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
