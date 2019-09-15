package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv._

/**
  * Created by enki on 3/19/17.
  */
class REParserSpec extends BaseSpec {
  "REParser" should "generate EmptyAST when parsing no tokens" in {
    REParser(RELexer("").right.get) shouldBe Right(EmptyAST)
  }

  it should "fail to parse a lone metacharacter" in {
    REParser(RELexer("+").right.get) shouldBe 'Left
  }

  it should "give higher precedence to * than |" in {
    REParser(RELexer("a|b*").right.get) shouldBe Right(OrAST(CharAST('a'), StarAST(CharAST('b'))))
  }

  it should "give higher precedence to operators than ~" in {
    REParser(RELexer("a~b*").right.get) shouldBe Right(CatAST(CharAST('a'), ComplementAST(StarAST(CharAST('b')))))
  }

  it should "give higher precedence to + than |" in {
    REParser(RELexer("a|b+").right.get) shouldBe Right(OrAST(CharAST('a'), CatAST(CharAST('b'), StarAST(CharAST('b')))))
  }

  it should "give same precedence to & as |" in {
    REParser(RELexer("a*&ab*").right.get) shouldBe Right(AndAST(StarAST(CharAST('a')), CatAST(CharAST('a'), StarAST(CharAST('b')))))
  }

  it should "give higher precedence to ? than |" in {
    REParser(RELexer("a|b?").right.get) shouldBe Right(OrAST(CharAST('a'), OrAST(EmptyAST, CharAST('b'))))
  }

  it should "give higher precedence to * than concatenation" in {
    REParser(RELexer("ab*").right.get) shouldBe Right(CatAST(CharAST('a'), StarAST(CharAST('b'))))
  }

  it should "give higher precedence to + than concatenation" in {
    REParser(RELexer("ab+").right.get) shouldBe Right(CatAST(CharAST('a'), CatAST(CharAST('b'), StarAST(CharAST('b')))))
  }

  it should "give higher precedence to ? than concatenation" in {
    REParser(RELexer("ab?").right.get) shouldBe Right(CatAST(CharAST('a'), OrAST(EmptyAST, CharAST('b'))))
  }

  it should "give higher precedence to concatenation than |" in {
    REParser(RELexer("ab|c").right.get) shouldBe Right(OrAST(CatAST(CharAST('a'), CharAST('b')), CharAST('c')))
  }

  it should "give parentheses highest precedence" in {
    REParser(RELexer("(a|b)*").right.get) shouldBe Right(StarAST(OrAST(CharAST('a'), CharAST('b'))))
  }

  it should "give character classes higher precedence than operators" in {
    REParser(RELexer("[a-c][d-f]*").right.get) shouldBe Right(CatAST(CharClassAST(Set('a','b','c'), false), StarAST(CharClassAST(Set('d','e','f'), false))))
  }

  it should "handle repeated characters in character classes" in {
    val chars = REParser(RELexer("[a-cb-d]").right.get) match {
      case Right(CharClassAST(cs, _)) => cs
      case _ => Set.empty[Char]
    }

    chars should contain('a')
    chars should contain('b')
    chars should contain('c')
    chars should contain('d')
    chars shouldNot contain('-')
    chars.size shouldBe 4
  }


  it should "parse a quantifier" in {
      REParser(RELexer("a{2}").right.get) shouldBe Right(CatAST(CharAST('a'), CharAST('a')))
      REParser(RELexer("a{2,4}").right.get) shouldBe Right(CatAST(CharAST('a'), CatAST(CharAST('a'), CatAST(OrAST(EmptyAST, CharAST('a')), OrAST(EmptyAST, CharAST('a'))))))
      REParser(RELexer("a{3,}").right.get) shouldBe Right(CatAST(CharAST('a'), CatAST(CharAST('a'), CatAST(CharAST('a'), StarAST(CharAST('a'))))))
  }

  it should "quantify numeric characters" in {
    REParser(RELexer("2{4}").right.get) shouldBe Right(CatAST(CharAST('2'), CatAST(CharAST('2'), CatAST(CharAST('2'), CharAST('2')))))
  }

  it should "fail parsing negative quantifiers" in {
    REParser(RELexer("2{-2}").right.get) shouldBe 'Left
  }

  it should "fail when when m is greater than n in {m,n}" in {
    REParser(RELexer("2{5,2}").right.get) shouldBe Left(RegexParserError("In quantifier {m,n}, m is 5 and n is 2, but m cannot be greater than n."))
  }
}
