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
}

// AST of regex matching no strings
case object NullAST extends RegexAST {

  override val acceptsEmpty: Boolean = false

  override val isNull: Boolean = true

  val getCharClasses: Set[CharClassAST] = Set(CharClassAST.sigma)

  override def derive(c: Char): RegexAST = NullAST
}

// AST of regex matching exactly the empty string
case object EmptyAST extends RegexAST {
  override val acceptsEmpty: Boolean = true

  override val isEmpty: Boolean = true

  val getCharClasses: Set[CharClassAST] = Set(CharClassAST.sigma)

  override def derive(c: Char): RegexAST = EmptyAST
}

// Complement of another AST
final class ComplementAST(val re: RegexAST) extends RegexAST {
  override val acceptsEmpty: Boolean = !re.acceptsEmpty

  val getCharClasses: Set[CharClassAST] = re.getCharClasses

  override def equals(o: scala.Any): Boolean = o match {
    case ComplementAST(r2) => re == r2
    case _ => false
  }

  override def toString: String = s"ComplementAST($re)"

  override def derive(c: Char): RegexAST = ComplementAST(re.derive(c))
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
    case OrAST(l, r) => left == l && right == r || left == r && right == l
    case _ => false
  }

  override def toString: String = s"OrAST($left, $right)"

  override def derive(c: Char): RegexAST = OrAST(left.derive(c), right.derive(c))
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
    case AndAST(l, r) => left == l && right == r || left == r && right == l
    case _ => false
  }

  override def toString: String = s"AndAST($left, $right)"

  override def derive(c: Char): RegexAST = AndAST(left.derive(c), right.derive(c))
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

final class CatAST(val children: List[RegexAST]) extends RegexAST {
  override val acceptsEmpty: Boolean = children.forall(_.acceptsEmpty)

  val getCharClasses: Set[CharClassAST] = {
    var curr = children.head
    var rest = children.tail
    var ccls = curr.getCharClasses
    while (curr.acceptsEmpty && rest.nonEmpty) {
      curr = rest.head
      rest = rest.tail
      ccls = CharClassAST.conjunction(ccls, curr.getCharClasses)
    }
    ccls
  }

  override def equals(o: Any): Boolean = o match {
    case CatAST(otherChildren) if children.size == otherChildren.size =>
      children.zip(otherChildren).forall { case (l, r) => l == r }
    case _ => false
  }

  override def toString: String = s"CatAST(${children.mkString("")})"

  override def derive(c: Char): RegexAST = {
    var curr = children.head
    var rest = children.tail
    var drvs = List(CatAST(curr.derive(c) :: rest))
    while (curr.acceptsEmpty && rest.nonEmpty) {
      curr = rest.head
      rest = rest.tail
      drvs = CatAST(curr.derive(c) :: rest) :: drvs
    }
    drvs.foldLeft(NullAST: RegexAST)((acc, r) => OrAST(r, acc))
  }
}

object CatAST {
  def unapply(arg: CatAST): Option[List[RegexAST]] = Some(arg.children)

  def apply(child: RegexAST*): RegexAST = apply(child.toList)

  def apply(children: List[RegexAST]): RegexAST = {
    // if any child is null, the whole catenation is null
    if (children.exists(_.isNull)) {
      NullAST
    } else {
      // EmptyAST children have no effect in a catenation
      val neChildren = children.filterNot(_.isEmpty)
      if (neChildren.isEmpty) {
        EmptyAST
      } else if (neChildren.size == 1) {
        // unwrap catenation when single child
        neChildren.head
      } else {
        // flatten any child CatASTs
        new CatAST(neChildren.flatMap {
          case CatAST(otherChildren) => otherChildren
          case x => List(x)
        })
      }
    }
  }
}


final class StarAST(val re: RegexAST) extends RegexAST {
  override val acceptsEmpty: Boolean = true

  val getCharClasses: Set[CharClassAST] = re.getCharClasses

  override def equals(o: scala.Any): Boolean = o match {
    case StarAST(r2) => re == r2
    case _ => false
  }

  override def toString: String = s"StarAST($re)"

  override def derive(c: Char): RegexAST = CatAST(List(re.derive(c), this))
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
