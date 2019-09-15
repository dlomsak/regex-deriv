package com.github.dlomsak.regex.deriv

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}


trait BaseSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  def genBase: Gen[RegexAST] = Gen.oneOf(NullAST, EmptyAST)

  def genLeaf: Gen[RegexAST] = Gen.oneOf(genBase, genChar)

  def genChar: Gen[RegexAST] = for (c <- Gen.alphaNumChar) yield CharAST(c)

  def genOr(depth: Int): Gen[RegexAST] = for {
    r1 <- genRegex(depth)
    r2 <- genRegex(depth)
  } yield OrAST(r1, r2)

  def genCat(depth: Int): Gen[RegexAST] = for {
    r1 <- genRegex(depth)
    r2 <- genRegex(depth)
  } yield CatAST(r1, r2)

  def genStar(depth: Int): Gen[RegexAST] = for (r <- genRegex(depth)) yield StarAST(r)

  def genRegex(depth: Int): Gen[RegexAST] =
    if (depth >= 100) { genLeaf }
    else { Gen.oneOf(genLeaf, genRegex(depth + 1)) }

  implicit val arbRegex: Arbitrary[RegexAST] = Arbitrary(genRegex(0))

}
