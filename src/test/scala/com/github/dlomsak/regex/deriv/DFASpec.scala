package com.github.dlomsak.regex.deriv

import com.github.dlomsak.regex.deriv.phase.{ParseContext, RE2DFA}

class DFASpec extends BaseSpec {
  "DFA" should "match equivalently to RegexAST" in {
    forAll { (r: RegexAST) =>
      val dfa = RE2DFA(r, new ParseContext())
      forAll { (s: String) =>
        r.matches(s) shouldEqual dfa.accepts(s)
      }
    }
  }

  // removed group matching, retain tests for now
/*
  it should "group properly with *" in {
    RegExpr("((abc)*)").right.get("abcabcabc").matches shouldBe List((0,0,8), (1,6,2), (0,0,5), (1,3,2), (0,0,2), (1,0,2))
  }

  it should "group properly with +" in {
    RegExpr("((abc)+)").right.get("abcabcabc").matches shouldBe List((0,0,8), (1,6,2), (0,0,5), (1,3,2), (0,0,2), (1,0,2))
  }

  it should "extract simple named groups" in {
    val result = RegExpr("(?<start>\\d+)[a-zA-Z]+(?<end>\\d+)").right.get("101test2")
    result.getMatches("start") shouldBe List((0,3), (0,2), (0,1))
    result.getMatches("end") shouldBe List((7, 1))
  }
  */
}
