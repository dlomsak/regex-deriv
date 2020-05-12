package com.github.dlomsak.regex.deriv

import com.github.dlomsak.regex.deriv.phase.RE2DFA

class DFASpec extends BaseSpec {
  "DFA" should "match equivalently to RegexAST" in {
    forAll { (r: RegexAST) =>
      val dfa = RE2DFA(r)
      forAll { (s: String) =>
        r.matches(s) shouldEqual dfa.accepts(s)
      }
    }
  }

  it should "group properly with *" in {
    RegExpr("((abc)*)").right.get.consumeAll("abcabcabc").getMatches shouldBe List((0,0,8), (1,6,2), (0,0,5), (1,3,2), (0,0,2), (1,0,2))
  }

  it should "group properly with +" in {
    RegExpr("((abc)+)").right.get.consumeAll("abcabcabc").getMatches shouldBe List((0,0,8), (1,6,2), (0,0,5), (1,3,2), (0,0,2), (1,0,2))
  }
}
